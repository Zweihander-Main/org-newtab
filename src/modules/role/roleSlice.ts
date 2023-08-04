import { createSlice } from '@reduxjs/toolkit';
import { listenerMiddleware } from 'app/middleware';
import { RootState } from 'app/store';
import Port from 'lib/Port';
import { LogLoc, LogMsgDir, logMsg } from 'lib/logging';
import {
	SendResponseType,
	getMasterWSTabId,
	handleConfirmingAlive,
	handleConfirmingRoleAsMaster,
	handlePassingMessage,
	sendMsgToAllTabs,
	sendMsgToBGSWPort,
	sendMsgToTab,
	sendUpdateInWSState,
} from 'lib/messages';
import {
	EmacsSendMsg,
	MsgDirection,
	MsgToBGSWType,
	MsgToTab,
	MsgToTabType,
	SendJsonMessage,
	WSStateMsg,
	getMsgToTabType,
} from 'lib/types';
import { sendMsgToEmacs } from 'modules/emacs/emacsSlice';
import {
	addToResponsesWaitingFor,
	closeWS,
	openWS,
	removeFromResponsesWaitingFor,
	setReadyStateTo,
	setResponsesWaitingForTo,
} from 'modules/ws/wsSlice';

export interface RoleState {
	amMasterWS: boolean;
	stateResolved: boolean;
}

const initialState: RoleState = {
	amMasterWS: false,
	stateResolved: false,
};

export const roleSlice = createSlice({
	name: 'role',
	initialState,
	reducers: {
		becomeMasterWS: (state) => {
			state.amMasterWS = true;
		},
		becomeClientWS: (state) => {
			state.amMasterWS = false;
		},
		setStateAsResolved: (state) => {
			state.stateResolved = true;
		},
		establishRole: () => {},
	},
});
export const {
	becomeMasterWS,
	becomeClientWS,
	setStateAsResolved,
	establishRole,
} = roleSlice.actions;

export const selectedAmMasterWs = (state: RootState) => state.role.amMasterWS;
export const selectedStateResolved = (state: RootState) =>
	state.role.stateResolved;

export default roleSlice.reducer;

// TODO: rename amMasterWS to amMasterRole

/**
 * Open websocket when role becomes master and state is resolved.
 * This, websocket should not open before role and state are established.
 */
listenerMiddleware.startListening({
	predicate: (action, currentState, originalState) =>
		(action.type === becomeMasterWS.type &&
			!originalState.role.amMasterWS &&
			currentState.role.stateResolved) ||
		(action.type === setStateAsResolved && currentState.role.amMasterWS),
	effect: (_action, listenerApi) => {
		const { dispatch } = listenerApi;
		dispatch(openWS());
	},
});

listenerMiddleware.startListening({
	predicate: (action, _currentState, originalState) =>
		action.type === becomeClientWS.type && originalState.role.amMasterWS,
	effect: (_action, listenerApi) => {
		const { dispatch } = listenerApi;
		dispatch(closeWS());
	},
});

listenerMiddleware.startListening({
	actionCreator: establishRole,
	effect: (_action, listenerApi) => {
		const { dispatch } = listenerApi;
		const getState = listenerApi.getState.bind(this);

		const sendJsonMessage: SendJsonMessage = (
			jsonMessage: EmacsSendMsg
		) => {
			if (getState().role.amMasterWS) {
				dispatch(sendMsgToEmacs(jsonMessage));
			} else {
				void getMasterWSTabId().then((masterWSTabAsNumber) => {
					if (masterWSTabAsNumber) {
						sendMsgToTab(
							MsgToTabType.PASS_TO_EMACS,
							masterWSTabAsNumber,
							jsonMessage
						);
					}
				});
			}
		};

		const handleQueryStateOfWS = () => {
			const {
				role: { amMasterWS },
				ws: { readyState, responsesWaitingFor },
			} = getState();
			if (amMasterWS) {
				sendMsgToAllTabs(MsgToTabType.SET_WS_STATE, {
					readyState,
					responsesWaitingFor,
				});
			}
		};

		const handleUpdateStateOfWS = (message: MsgToTab) => {
			if (!getState().role.amMasterWS && message?.data) {
				const {
					responsesWaitingFor: responsesWaitingForFromMaster,
					readyState: readyStateFromMaster,
				} = message.data as WSStateMsg;
				if (typeof readyStateFromMaster === 'number') {
					dispatch(setReadyStateTo(readyStateFromMaster));
				}
				if (Array.isArray(responsesWaitingForFromMaster)) {
					dispatch(
						setResponsesWaitingForTo(responsesWaitingForFromMaster)
					);
				}
			}
		};

		const queryStateOfWS = () => {
			void getMasterWSTabId().then((masterWSTabAsNumber) => {
				const { readyState, responsesWaitingFor } = getState().ws;
				if (masterWSTabAsNumber) {
					sendMsgToTab(
						MsgToTabType.QUERY_WS_STATE,
						masterWSTabAsNumber,
						{
							readyState,
							responsesWaitingFor,
						}
					);
				}
			});
		};

		const handleMessage = (
			message: MsgToTab,
			_sender: chrome.runtime.MessageSender,
			sendResponse: SendResponseType
		) => {
			if (message.direction !== MsgDirection.TO_NEWTAB) {
				return;
			}
			logMsg(
				LogLoc.NEWTAB,
				LogMsgDir.RECV,
				'Data recv:',
				getMsgToTabType(message.type),
				message?.data ? `with data ${JSON.stringify(message.data)}` : ''
			);
			switch (message.type) {
				case MsgToTabType.CONFIRM_YOUR_ROLE_IS_MASTER:
					handleConfirmingRoleAsMaster(
						sendResponse,
						getState().role.amMasterWS
					);
					break;
				case MsgToTabType.SET_ROLE_MASTER:
					dispatch(becomeMasterWS());
					break;
				case MsgToTabType.SET_ROLE_CLIENT:
					dispatch(becomeClientWS());
					queryStateOfWS();
					break;
				case MsgToTabType.QUERY_ALIVE:
					handleConfirmingAlive(sendResponse);
					break;
				case MsgToTabType.PASS_TO_EMACS:
					handlePassingMessage(sendJsonMessage, message);
					break;
				case MsgToTabType.QUERY_WS_STATE:
					handleQueryStateOfWS();
					break;
				case MsgToTabType.SET_WS_STATE:
					handleUpdateStateOfWS(message);
					break;
			}
		};

		if (!chrome.runtime.onMessage.hasListener(handleMessage)) {
			chrome.runtime.onMessage.addListener(handleMessage);
		}

		// 1. Ask if any master web sockets exist
		sendMsgToBGSWPort(MsgToBGSWType.QUERY_WS_ROLE, Port.port);
	},
});

listenerMiddleware.startListening({
	predicate: (action, currentState) =>
		currentState.role.amMasterWS &&
		(action.type === removeFromResponsesWaitingFor.type ||
			action.type === addToResponsesWaitingFor.type ||
			action.type === setResponsesWaitingForTo.type ||
			action.type === setReadyStateTo.type),
	effect: (action, listenerApi) => {
		const getState = listenerApi.getState.bind(this);
		const { responsesWaitingFor, readyState } = getState().ws;
		if (action.type === setReadyStateTo.type) {
			sendUpdateInWSState({ readyState });
		} else {
			sendUpdateInWSState({ responsesWaitingFor });
		}
	},
});

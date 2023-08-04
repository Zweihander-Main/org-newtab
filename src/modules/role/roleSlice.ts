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
	WSPortMsg,
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
	setWSPortTo,
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

/**
 * Close websocket when role becomes client
 */
listenerMiddleware.startListening({
	actionCreator: becomeClientWS,
	effect: (_action, listenerApi) => {
		const { dispatch } = listenerApi;
		dispatch(closeWS());
	},
});

/**
 * Everytime the port changes, restart the websocket as master or let the
 * master know it needs to.
 */
listenerMiddleware.startListening({
	actionCreator: setWSPortTo,
	effect: (action, listenerApi) => {
		const { dispatch } = listenerApi;
		const getState = listenerApi.getState.bind(this);
		const {
			role: { amMasterWS },
		} = getState();
		if (amMasterWS) {
			dispatch(closeWS());
			dispatch(openWS());
		} else {
			void getMasterWSTabId().then((masterWSTabNum) => {
				if (masterWSTabNum) {
					const port = action.payload;
					sendMsgToTab(MsgToTabType.SET_WS_PORT, masterWSTabNum, {
						port,
					});
				}
			});
		}
	},
});

/**
 * Establish role using BGSW, handle messages
 */
listenerMiddleware.startListening({
	actionCreator: establishRole,
	effect: (_action, listenerApi) => {
		const { dispatch } = listenerApi;
		const getState = listenerApi.getState.bind(this);

		const sendJsonMessage: SendJsonMessage = (
			jsonMessage: EmacsSendMsg
		) => {
			const {
				role: { amMasterWS },
			} = getState();
			if (amMasterWS) {
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
			void getMasterWSTabId().then((masterWSTabNum) => {
				const { readyState, responsesWaitingFor } = getState().ws;
				if (masterWSTabNum) {
					sendMsgToTab(MsgToTabType.QUERY_WS_STATE, masterWSTabNum, {
						readyState,
						responsesWaitingFor,
					});
				}
			});
		};

		const handleSetWSPort = (message: MsgToTab) => {
			if (getState().role.amMasterWS && message?.data) {
				const { port } = message.data as WSPortMsg;
				if (typeof port === 'number') {
					dispatch(setWSPortTo(port));
				}
			}
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
				case MsgToTabType.SET_WS_PORT:
					handleSetWSPort(message);
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

/**
 * As master, send WS updates to all other tabs
 */
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
		} else if (
			action.type === addToResponsesWaitingFor.type ||
			action.type === removeFromResponsesWaitingFor.type ||
			action.type === setResponsesWaitingForTo.type
		) {
			sendUpdateInWSState({ responsesWaitingFor });
		}
	},
});

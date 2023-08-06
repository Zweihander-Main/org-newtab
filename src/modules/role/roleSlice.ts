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

/**
 * Role is defined as master or client and refers to the websocket connection.
 * The Master websocket is the one that actually talks to Emacs while the client
 * is a dummy connection that talks through the master. This is done to avoid
 * multiple websocket connections to Emacs.
 */

export interface RoleState {
	amMasterRole: boolean;
	stateResolved: boolean;
}

const initialState: RoleState = {
	amMasterRole: false,
	stateResolved: false,
};

export const roleSlice = createSlice({
	name: 'role',
	initialState,
	reducers: {
		becomeMasterRole: (state) => {
			state.amMasterRole = true;
		},
		becomeClientRole: (state) => {
			state.amMasterRole = false;
		},
		setStateAsResolved: (state) => {
			state.stateResolved = true;
		},
		establishRole: () => {},
	},
});
export const {
	becomeMasterRole,
	becomeClientRole,
	setStateAsResolved,
	establishRole,
} = roleSlice.actions;

export const selectedAmMasterRole = (state: RootState) =>
	state.role.amMasterRole;
export const selectedStateResolved = (state: RootState) =>
	state.role.stateResolved;

export default roleSlice.reducer;

/**
 * Open websocket when role becomes master and state is resolved.
 * The role is necessary to avoid multiple websocket connections to Emacs. The
 * state is necessary to allow custom websocket ports.
 */
listenerMiddleware.startListening({
	predicate: (action, currentState, originalState) =>
		(action.type === becomeMasterRole.type &&
			!originalState.role.amMasterRole &&
			currentState.role.stateResolved) ||
		(action.type === setStateAsResolved && currentState.role.amMasterRole),
	effect: (_action, listenerApi) => {
		const { dispatch } = listenerApi;
		dispatch(openWS());
	},
});

/**
 * Close websocket when role becomes client.
 */
listenerMiddleware.startListening({
	actionCreator: becomeClientRole,
	effect: (_action, listenerApi) => {
		const { dispatch } = listenerApi;
		dispatch(closeWS());
	},
});

/**
 * Everytime the port changes, restart the websocket as master or let the
 * master know it needs to. The master will not restart without the message
 * from client as the side effect is not triggered without an action (which the
 * storage sync doesn't trigger).
 */
listenerMiddleware.startListening({
	actionCreator: setWSPortTo,
	effect: (action, listenerApi) => {
		const { dispatch } = listenerApi;
		const getState = listenerApi.getState.bind(this);
		const {
			role: { amMasterRole: amMasterWS },
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
 * Establish role using BGSW, handle messages.
 */
// TODO: Some of this should be moved into actions for easier debugging.
listenerMiddleware.startListening({
	actionCreator: establishRole,
	effect: (_action, listenerApi) => {
		const { dispatch } = listenerApi;
		const getState = listenerApi.getState.bind(this);

		const sendJsonMessage: SendJsonMessage = (
			jsonMessage: EmacsSendMsg
		) => {
			const {
				role: { amMasterRole: amMasterWS },
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
				role: { amMasterRole: amMasterWS },
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
			if (!getState().role.amMasterRole && message?.data) {
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
			if (getState().role.amMasterRole && message?.data) {
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
						getState().role.amMasterRole
					);
					break;
				case MsgToTabType.SET_ROLE_MASTER:
					dispatch(becomeMasterRole());
					break;
				case MsgToTabType.SET_ROLE_CLIENT:
					dispatch(becomeClientRole());
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
 * As master, send WS updates to all other tabs. Using messaging rather than
 * storage to avoid persisting ephemeral state in storage.
 */
listenerMiddleware.startListening({
	predicate: (action, currentState) =>
		currentState.role.amMasterRole &&
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

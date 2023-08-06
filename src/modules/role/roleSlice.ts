/* eslint-disable @typescript-eslint/no-unused-vars */
import { PayloadAction, createSlice } from '@reduxjs/toolkit';
import { listenerMiddleware } from 'app/middleware';
import { RootState } from 'app/store';
import Port from 'lib/Port';
import { LogLoc, LogMsgDir, logMsg, logMsgErr } from 'lib/logging';
import {
	SendResponseType,
	getMasterWSTabId,
	handleConfirmingAlive,
	handleConfirmingRoleAsMaster,
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
	WSPortMsg,
	WSStateMsg,
	getMsgToTabType,
} from 'lib/types';
import { _sendMsgToEmacs } from 'modules/emacs/emacsSlice';
import {
	_addToResponsesWaitingFor,
	_closeWS,
	_openWS,
	_removeFromResponsesWaitingFor,
	_resetWS,
	_setReadyStateTo,
	_setResponsesWaitingForTo,
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

export const name = 'role';
export const persistenceBlacklist: Array<keyof RoleState> = [
	'stateResolved',
	'amMasterRole',
];

const initialState: RoleState = {
	amMasterRole: false,
	stateResolved: false,
};

export const roleSlice = createSlice({
	name,
	initialState,
	reducers: {
		establishRole: () => {},
		_becomeMasterRole: (state) => {
			state.amMasterRole = true;
		},
		_becomeClientRole: (state) => {
			state.amMasterRole = false;
		},
		_setStateAsResolved: (state) => {
			state.stateResolved = true;
		},
		_handleBGSWMsg_confirmRoleAsMaster: (
			_state,
			_action: PayloadAction<SendResponseType>
		) => {},
		_handleBGSWMsg_setRoleAsMaster: () => {},
		_handleBGSWMsg_setRoleAsClient: () => {},
		_handleBGSWMsg_confirmAlive: (
			_state,
			_action: PayloadAction<SendResponseType>
		) => {},
		_handleTabMsg_passToEmacs: (
			_state,
			_action: PayloadAction<MsgToTab>
		) => {},
		_handleTabMsg_getWSState: () => {},
		_handleTabMsg_setWSState: (
			_state,
			_action: PayloadAction<MsgToTab>
		) => {},
		_handleTabMsg_setWSPort: (
			_state,
			_action: PayloadAction<MsgToTab>
		) => {},
	},
});
export const {
	establishRole,
	_becomeMasterRole,
	_becomeClientRole,
	_setStateAsResolved,
	_handleBGSWMsg_confirmRoleAsMaster,
	_handleBGSWMsg_setRoleAsMaster,
	_handleBGSWMsg_setRoleAsClient,
	_handleBGSWMsg_confirmAlive,
	_handleTabMsg_passToEmacs,
	_handleTabMsg_getWSState,
	_handleTabMsg_setWSState,
	_handleTabMsg_setWSPort,
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
		(action.type === _becomeMasterRole.type &&
			!originalState.role.amMasterRole &&
			currentState.role.stateResolved) ||
		(action.type === _setStateAsResolved && currentState.role.amMasterRole),
	effect: (_action, listenerApi) => {
		const { dispatch } = listenerApi;
		dispatch(_openWS());
	},
});

/**
 * Close websocket when role becomes client.
 */
listenerMiddleware.startListening({
	actionCreator: _becomeClientRole,
	effect: (_action, listenerApi) => {
		const { dispatch } = listenerApi;
		dispatch(_closeWS());
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
			dispatch(_resetWS());
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
 * As master, send updates in readyState to other tabs
 * (not persisted in storage)
 */
listenerMiddleware.startListening({
	predicate: (action, currentState) =>
		currentState.role.amMasterRole && action.type === _setReadyStateTo.type,
	effect: (_action, listenerApi) => {
		const getState = listenerApi.getState.bind(this);
		const { readyState } = getState().ws;
		sendUpdateInWSState({ readyState });
	},
});

/**
 * As master, send updates in responsesWaitingFor to other tabs
 * (not persisted in storage)
 */
listenerMiddleware.startListening({
	predicate: (action, currentState) =>
		currentState.role.amMasterRole &&
		(action.type === _removeFromResponsesWaitingFor.type ||
			action.type === _addToResponsesWaitingFor.type ||
			action.type === _setResponsesWaitingForTo.type),
	effect: (_action, listenerApi) => {
		const getState = listenerApi.getState.bind(this);
		const { responsesWaitingFor } = getState().ws;
		sendUpdateInWSState({ responsesWaitingFor });
	},
});

listenerMiddleware.startListening({
	actionCreator: _handleBGSWMsg_confirmRoleAsMaster,
	effect: (action, listenerApi) => {
		const getState = listenerApi.getState.bind(this);
		const {
			role: { amMasterRole },
		} = getState();
		handleConfirmingRoleAsMaster(action.payload, amMasterRole);
	},
});

listenerMiddleware.startListening({
	actionCreator: _handleBGSWMsg_setRoleAsMaster,
	effect: (_action, listenerApi) => {
		const { dispatch } = listenerApi;
		dispatch(_becomeMasterRole());
	},
});

listenerMiddleware.startListening({
	actionCreator: _handleBGSWMsg_setRoleAsClient,
	effect: (_action, listenerApi) => {
		const { dispatch } = listenerApi;
		const getState = listenerApi.getState.bind(this);
		dispatch(_becomeClientRole());
		void getMasterWSTabId().then((masterWSTabNum) => {
			if (masterWSTabNum) {
				const {
					ws: { readyState, responsesWaitingFor },
				} = getState();
				sendMsgToTab(MsgToTabType.QUERY_WS_STATE, masterWSTabNum, {
					readyState,
					responsesWaitingFor,
				});
			}
		});
	},
});

listenerMiddleware.startListening({
	actionCreator: _handleBGSWMsg_confirmAlive,
	effect: (action) => {
		handleConfirmingAlive(action.payload);
	},
});

listenerMiddleware.startListening({
	actionCreator: _handleTabMsg_passToEmacs,
	effect: (action, listenerApi) => {
		const { dispatch } = listenerApi;
		const getState = listenerApi.getState.bind(this);
		const message = action.payload;
		if (!message?.data) {
			logMsgErr(
				LogLoc.NEWTAB,
				LogMsgDir.RECV,
				'Bad or no data for updating match query',
				message?.data
			);
			return;
		}

		const {
			role: { amMasterRole: amMasterWS },
		} = getState();
		if (amMasterWS) {
			dispatch(_sendMsgToEmacs(message.data as EmacsSendMsg));
		} else {
			void getMasterWSTabId().then((masterWSTabAsNumber) => {
				if (masterWSTabAsNumber) {
					sendMsgToTab(
						MsgToTabType.PASS_TO_EMACS,
						masterWSTabAsNumber,
						message.data
					);
				}
			});
		}
	},
});

listenerMiddleware.startListening({
	actionCreator: _handleTabMsg_getWSState,
	effect: (_action, listenerApi) => {
		const getState = listenerApi.getState.bind(this);
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
	},
});

listenerMiddleware.startListening({
	actionCreator: _handleTabMsg_setWSState,
	effect: (action, listenerApi) => {
		const { dispatch } = listenerApi;
		const getState = listenerApi.getState.bind(this);
		const {
			role: { amMasterRole },
		} = getState();
		const message = action.payload;
		if (amMasterRole && message?.data) {
			const {
				responsesWaitingFor: responsesWaitingForFromMaster,
				readyState: readyStateFromMaster,
			} = message.data as WSStateMsg;
			if (typeof readyStateFromMaster === 'number') {
				dispatch(_setReadyStateTo(readyStateFromMaster));
			}
			if (Array.isArray(responsesWaitingForFromMaster)) {
				dispatch(
					_setResponsesWaitingForTo(responsesWaitingForFromMaster)
				);
			}
		}
	},
});

listenerMiddleware.startListening({
	actionCreator: _handleTabMsg_setWSPort,
	effect: (action, listenerApi) => {
		const { dispatch } = listenerApi;
		const getState = listenerApi.getState.bind(this);
		const {
			role: { amMasterRole },
		} = getState();
		const message = action.payload;
		if (amMasterRole && message?.data) {
			const { port } = message.data as WSPortMsg;
			if (typeof port === 'number') {
				dispatch(setWSPortTo(port));
			}
		}
	},
});

/**
 * Establish role using BGSW, handle messages.
 */
listenerMiddleware.startListening({
	actionCreator: establishRole,
	effect: (_action, listenerApi) => {
		const { dispatch } = listenerApi;
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
					dispatch(_handleBGSWMsg_confirmRoleAsMaster(sendResponse));
					break;
				case MsgToTabType.SET_ROLE_MASTER:
					dispatch(_handleBGSWMsg_setRoleAsMaster());
					break;
				case MsgToTabType.SET_ROLE_CLIENT:
					dispatch(_handleBGSWMsg_setRoleAsClient());
					break;
				case MsgToTabType.QUERY_ALIVE:
					dispatch(_handleBGSWMsg_confirmAlive(sendResponse));
					break;
				case MsgToTabType.PASS_TO_EMACS:
					dispatch(_handleTabMsg_passToEmacs(message));
					break;
				case MsgToTabType.QUERY_WS_STATE:
					dispatch(_handleTabMsg_getWSState());
					break;
				case MsgToTabType.SET_WS_STATE:
					dispatch(_handleTabMsg_setWSState(message));
					break;
				case MsgToTabType.SET_WS_PORT:
					dispatch(_handleTabMsg_setWSPort(message));
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

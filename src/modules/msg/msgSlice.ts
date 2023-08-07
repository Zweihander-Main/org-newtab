/* eslint-disable @typescript-eslint/no-unused-vars */
import { PayloadAction, createSlice } from '@reduxjs/toolkit';
import { listenerMiddleware } from 'app/middleware';
import { LogLoc, LogMsgDir, logMsg, logMsgErr } from 'lib/logging';
import {
	SendResponseType,
	getMasterWSTabId,
	handleConfirmingAlive,
	handleConfirmingRoleAsMaster,
	sendMsgToAllTabs,
	sendMsgToTab,
} from 'lib/messages';
import {
	EmacsSendMsg,
	MsgDirection,
	MsgToTab,
	MsgToTabType,
	WSPortMsg,
	WSStateMsg,
	getMsgToTabType,
} from 'lib/types';
import { _sendMsgToEmacs } from 'modules/emacs/emacsSlice';
import {
	_becomeClientRole,
	_becomeMasterRole,
	establishRole,
} from 'modules/role/roleSlice';
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
 * Handles messages from the background script as well as from other tabs.
 * Port updates from client tabs go to the master tab. Other WS state updates
 * go from the master tab to all others. The BGSW is used to establish and
 * maintain websocket roles.
 */

export interface MsgState {
	handlerConnected: boolean;
}

const initialState: MsgState = {
	handlerConnected: false,
};

export const name = 'msg';
export const persistenceBlacklist: Array<keyof MsgState> = Object.keys(
	initialState
) as Array<keyof MsgState>;

export const msgSlice = createSlice({
	name,
	initialState,
	reducers: {
		initMessaging: () => {},
		_handlerIsConnected: (state) => {
			state.handlerConnected = true;
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
	initMessaging,
	_handlerIsConnected,
	_handleBGSWMsg_confirmRoleAsMaster,
	_handleBGSWMsg_setRoleAsMaster,
	_handleBGSWMsg_setRoleAsClient,
	_handleBGSWMsg_confirmAlive,
	_handleTabMsg_passToEmacs,
	_handleTabMsg_getWSState,
	_handleTabMsg_setWSState,
	_handleTabMsg_setWSPort,
} = msgSlice.actions;

export default msgSlice.reducer;

/**
 * Let the BGSW know that this is the master websocket.
 */
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

/**
 * The BGSW has instructed that this tab is the master websocket.
 */
listenerMiddleware.startListening({
	actionCreator: _handleBGSWMsg_setRoleAsMaster,
	effect: (_action, listenerApi) => {
		const { dispatch } = listenerApi;
		dispatch(_becomeMasterRole());
	},
});

/**
 * The BGSW has instructed that this tab is a client websocket.
 * Send a message to the master tab to get the current state of the master
 * websocket.
 */
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

/**
 * Respond to the BGSW's request to confirm that this tab is still alive.
 */
listenerMiddleware.startListening({
	actionCreator: _handleBGSWMsg_confirmAlive,
	effect: (action) => {
		handleConfirmingAlive(action.payload);
	},
});

/**
 * If master, send passed in message to Emacs. If not, pass the message to the
 * master websocket.
 */
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
			role: { amMasterRole },
		} = getState();
		if (amMasterRole) {
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

/**
 * If master, send the current state of the master websocket to all tabs.
 * Triggered from a client request.
 */
listenerMiddleware.startListening({
	actionCreator: _handleTabMsg_getWSState,
	effect: (_action, listenerApi) => {
		const getState = listenerApi.getState.bind(this);
		const {
			role: { amMasterRole },
			ws: { readyState, responsesWaitingFor },
		} = getState();
		if (amMasterRole) {
			sendMsgToAllTabs(MsgToTabType.SET_WS_STATE, {
				readyState,
				responsesWaitingFor,
			});
		}
	},
});

/**
 * If a client websocket, set the dummy websocket status based on the
 * data from the master websocket.
 * Triggered from a master response.
 */
listenerMiddleware.startListening({
	actionCreator: _handleTabMsg_setWSState,
	effect: (action, listenerApi) => {
		const { dispatch } = listenerApi;
		const getState = listenerApi.getState.bind(this);
		const {
			role: { amMasterRole },
		} = getState();
		const message = action.payload;
		if (!amMasterRole && message?.data) {
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

/**
 * Received an FYI from a client websocket tab that the user has changed
 * the websocket port. Will be reflected in the state due to syncing but
 * needs this action called to trigger side effects (reopening websocket
 * with new port).
 */
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
 * Setup listener and message handler. Accepts message from both the BGSW
 * and other tabs.
 */
listenerMiddleware.startListening({
	actionCreator: initMessaging,
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
			dispatch(_handlerIsConnected());
		}
		dispatch(establishRole());
	},
});

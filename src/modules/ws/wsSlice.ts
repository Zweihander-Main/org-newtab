import {
	EmacsRecvMsg,
	EmacsSendMsgWithResid,
	MsgToTabType,
	WSReadyState,
} from 'lib/types';
import { PayloadAction, createSlice } from '@reduxjs/toolkit';
import { RootState } from 'app/store';
import Socket from 'lib/Socket';
import { listenerMiddleware } from 'app/middleware';
import { _recvMsgFromEmacs, _sendMsgToEmacs } from 'modules/emacs/emacsSlice';
import { sendToMasterTab } from 'lib/messages';

const MAXIMUM_TIME_TO_WAIT_FOR_RESPONSE = 60000;
export interface WSState {
	readyState: WSReadyState;
	responsesWaitingFor: Array<number>;
	wsPort: number;
	reconnectionAttempt: number;
	reconnectionTimeout: NodeJS.Timeout | null;
}

export const name = 'ws';
export const persistenceBlacklist: Array<keyof WSState> = [
	'readyState',
	'responsesWaitingFor',
	'reconnectionAttempt',
	'reconnectionTimeout',
];

const initialState: WSState = {
	readyState: WSReadyState.UNINSTANTIATED,
	responsesWaitingFor: [],
	wsPort: 35942,
	reconnectionAttempt: 0,
	reconnectionTimeout: null,
};

export const wsSlice = createSlice({
	name,
	initialState,
	reducers: {
		setWSPortTo: (state, action: PayloadAction<number>) => {
			state.wsPort = action.payload;
		},
		_setReadyStateTo: (state, action: PayloadAction<WSReadyState>) => {
			state.readyState = action.payload;
		},
		_addToResponsesWaitingFor: (state, action: PayloadAction<number>) => {
			state.responsesWaitingFor.push(action.payload);
		},
		_removeFromResponsesWaitingFor: (
			state,
			action: PayloadAction<number>
		) => {
			state.responsesWaitingFor = state.responsesWaitingFor.filter(
				(id) => id !== action.payload
			);
		},
		_setResponsesWaitingForTo: (
			state,
			action: PayloadAction<Array<number>>
		) => {
			state.responsesWaitingFor = action.payload;
		},
		_setReconnectionAttemptAndTimeoutTo: (
			state,
			action: PayloadAction<{
				reconnectionAttempt: number;
				reconnectionTimeout: NodeJS.Timeout | null;
			}>
		) => {
			const { reconnectionAttempt, reconnectionTimeout } = action.payload;
			state.reconnectionAttempt = reconnectionAttempt;
			state.reconnectionTimeout = reconnectionTimeout;
		},
		_openWS: () => {},
		_closeWS: () => {},
		_resetWS: () => {},
	},
});

export const {
	setWSPortTo,
	_setReadyStateTo,
	_removeFromResponsesWaitingFor,
	_addToResponsesWaitingFor,
	_setResponsesWaitingForTo,
	_setReconnectionAttemptAndTimeoutTo,
	_openWS,
	_closeWS,
	_resetWS,
} = wsSlice.actions;

export const selectedReadyState = (state: RootState) => state.ws.readyState;
export const selectedIsWaitingForResponse = (state: RootState) =>
	state.ws.responsesWaitingFor.length > 0;
export const selectedResponsesWaitingFor = (state: RootState) =>
	state.ws.responsesWaitingFor;
export const selectedWSPort = (state: RootState) => state.ws.wsPort;

export default wsSlice.reducer;

/**
 * Open the websocket
 */
listenerMiddleware.startListening({
	actionCreator: _openWS,
	effect: (_action, listenerApi) => {
		const { dispatch } = listenerApi;
		const getState = listenerApi.getState.bind(this);
		const {
			ws: { wsPort, reconnectionAttempt, reconnectionTimeout },
		} = getState();
		dispatch(_setReadyStateTo(WSReadyState.CONNECTING));
		Socket.connect(`ws://localhost:${wsPort}/`);
		Socket.on('open', () => {
			if (reconnectionTimeout) clearTimeout(reconnectionTimeout);
			dispatch(
				_setReconnectionAttemptAndTimeoutTo({
					reconnectionAttempt: 0,
					reconnectionTimeout: null,
				})
			);
			dispatch(_setReadyStateTo(WSReadyState.OPEN));
		});
		Socket.on('close', () => {
			dispatch(_setReadyStateTo(WSReadyState.CLOSED));
			dispatch(_setResponsesWaitingForTo([]));
			// Algo: Exponential Backoff
			const newReconnectionAttempt = Math.min(
				reconnectionAttempt + 1,
				16
			); // 2^16 secs = ~18 hours
			const interval = Math.pow(2, newReconnectionAttempt) * 1000;
			const newReconnectionTimeout = setTimeout(() => {
				dispatch(_resetWS());
			}, interval);
			dispatch(
				_setReconnectionAttemptAndTimeoutTo({
					reconnectionAttempt: newReconnectionAttempt,
					reconnectionTimeout: newReconnectionTimeout,
				})
			);
		});
		Socket.on('error', (event) => {
			console.error('Websocket error', event.data);
		});
		Socket.on('message', (event) => {
			const message = event.data;
			const parsed = JSON.parse(message) as EmacsRecvMsg;
			if (parsed === null) return;
			dispatch(_recvMsgFromEmacs(parsed));
			if ('resid' in parsed) {
				dispatch(_removeFromResponsesWaitingFor(parsed?.resid || -1));
			}
		});
	},
});

/**
 * Close the websocket
 */
listenerMiddleware.startListening({
	actionCreator: _closeWS,
	effect: (_action, listenerApi) => {
		const { dispatch } = listenerApi;
		dispatch(_setReadyStateTo(WSReadyState.CLOSING));
		dispatch(_setResponsesWaitingForTo([]));
		Socket.disconnect();
	},
});

/**
 * Reset (open and close) the websocket
 */
listenerMiddleware.startListening({
	actionCreator: _resetWS,
	effect: (_action, listenerApi) => {
		const { dispatch } = listenerApi;
		dispatch(_closeWS());
		dispatch(_openWS());
	},
});

/**
 * Send a message to Emacs or to the master websocket to pass to Emacs
 */
listenerMiddleware.startListening({
	actionCreator: _sendMsgToEmacs,
	effect: (action, listenerApi) => {
		const { dispatch } = listenerApi;
		const getState = listenerApi.getState.bind(this);
		const {
			role: { amMasterRole },
			ws: { readyState },
		} = getState();
		const data = action.payload;
		if (amMasterRole && readyState === WSReadyState.OPEN) {
			const resid = Math.floor(Math.random() * 1000000000);
			const toSend: EmacsSendMsgWithResid = { ...data, resid };
			Socket.sendJSON(toSend);
			dispatch(_addToResponsesWaitingFor(resid));
			setTimeout(() => {
				dispatch(_removeFromResponsesWaitingFor(resid));
			}, MAXIMUM_TIME_TO_WAIT_FOR_RESPONSE);
		} else {
			sendToMasterTab(MsgToTabType.PASS_TO_EMACS, data);
		}
	},
});

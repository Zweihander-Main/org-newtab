import {
	EmacsRecvMsg,
	EmacsSendMsg,
	MsgToTabType,
	WSReadyState,
} from 'lib/types';
import { PayloadAction, createSlice } from '@reduxjs/toolkit';
import { RootState } from 'app/store';
import Socket from 'lib/Socket';
import { listenerMiddleware } from 'app/middleware';
import {
	getItem,
	_recvMsgFromEmacs,
	_sendMsgToEmacs,
} from 'modules/emacs/emacsSlice';
import { getMasterWSTabId, sendMsgToTab } from 'lib/messages';

const MAXIMUM_TIME_TO_WAIT_FOR_RESPONSE = 60000;
export interface WSState {
	readyState: WSReadyState;
	responsesWaitingFor: Array<number>;
	wsPort: number;
}

export const name = 'ws';
export const persistenceBlacklist: Array<keyof WSState> = [
	'readyState',
	'responsesWaitingFor',
];

const initialState: WSState = {
	readyState: WSReadyState.UNINSTANTIATED,
	responsesWaitingFor: [],
	wsPort: 35942,
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

// TODO: alot of the role logic can be moved out of this file and into the roleSlice
/**
 * Open the websocket
 */
listenerMiddleware.startListening({
	actionCreator: _openWS,
	effect: (_action, listenerApi) => {
		const { dispatch } = listenerApi;
		const getState = listenerApi.getState.bind(this);
		const {
			ws: { wsPort },
		} = getState();
		dispatch(_setReadyStateTo(WSReadyState.CONNECTING));
		// eslint-disable-next-line no-console
		console.log('connecting');
		Socket.connect(`ws://localhost:${wsPort}/`);
		Socket.on('open', () => {
			// eslint-disable-next-line no-console
			console.log('open');
			dispatch(_setReadyStateTo(WSReadyState.OPEN));
		});
		Socket.on('close', () => {
			// eslint-disable-next-line no-console
			console.log('close');
			dispatch(_setReadyStateTo(WSReadyState.CLOSED));
		});
		Socket.on('error', (event) => {
			console.error('Websocket error', event.data);
		});
		Socket.on('message', (event) => {
			const message = event.data;
			const parsed = JSON.parse(message) as EmacsRecvMsg;
			if (parsed === null) return;
			dispatch(_recvMsgFromEmacs(parsed));
			if (parsed.type === 'ITEM') {
				// TODO: more general case for this
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
 * Every time the websocket opens, ask Emacs for the current item
 * (assuming master role)
 */
listenerMiddleware.startListening({
	predicate: (action, currentState) =>
		action.type === _setReadyStateTo.type &&
		currentState.role.amMasterRole &&
		currentState.ws.readyState === WSReadyState.OPEN,
	effect: (_action, listenerApi) => {
		const { dispatch } = listenerApi;
		const getState = listenerApi.getState.bind(this);
		const {
			emacs: { matchQuery },
		} = getState();
		if (matchQuery) {
			dispatch(getItem());
		}
	},
});

/**
 * Send a message to Emacs or to the master websocket to pass to Emacs
 */
listenerMiddleware.startListening({
	predicate: (action, currentState) =>
		action.type === _sendMsgToEmacs.type &&
		currentState.ws.readyState === WSReadyState.OPEN,
	effect: (action, listenerApi) => {
		const { dispatch } = listenerApi;
		const getState = listenerApi.getState.bind(this);
		const {
			role: { amMasterRole },
		} = getState();
		if (amMasterRole) {
			const resid = Math.floor(Math.random() * 1000000000);
			const data = { ...action.payload, resid } as EmacsSendMsg;
			Socket.sendJSON(data);
			dispatch(_addToResponsesWaitingFor(resid));
			setTimeout(() => {
				dispatch(_removeFromResponsesWaitingFor(resid));
			}, MAXIMUM_TIME_TO_WAIT_FOR_RESPONSE);
		} else {
			void getMasterWSTabId().then((masterWSTabAsNumber) => {
				if (masterWSTabAsNumber) {
					const data = {
						...action.payload,
					} as EmacsSendMsg;
					sendMsgToTab(
						MsgToTabType.PASS_TO_EMACS,
						masterWSTabAsNumber,
						data
					);
				}
			});
		}
	},
});

// TODO reconnecting logic -- reconnect on failure and also when browser starts before Emacs

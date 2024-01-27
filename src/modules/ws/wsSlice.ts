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
import {
	DEFAULT_WEBSOCKET_PORT,
	MAXIMUM_TIME_TO_WAIT_FOR_RESPONSE,
} from 'lib/constants';
import { resetData } from 'app/actions';
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
	wsPort: DEFAULT_WEBSOCKET_PORT,
};

export const wsSlice = createSlice({
	name,
	initialState,
	extraReducers: (builder) => builder.addCase(resetData, () => initialState),
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
export const selectedIsInSync = (state: RootState) =>
	state.ws.readyState === WSReadyState.OPEN &&
	!selectedIsWaitingForResponse(state);

export default wsSlice.reducer;

/**
 * Open the websocket, idempotent
 */
listenerMiddleware.startListening({
	actionCreator: _openWS,
	effect: (_action, listenerApi) => {
		const { dispatch } = listenerApi;
		const getState = listenerApi.getState.bind(this);
		const {
			ws: { wsPort },
		} = getState();
		if (!Socket.exists) {
			dispatch(_setReadyStateTo(WSReadyState.CONNECTING));
			Socket.connect(`ws://localhost:${wsPort}/`);
			Socket.on('open', () => {
				dispatch(_setReadyStateTo(WSReadyState.OPEN));
			});
			Socket.on('close', () => {
				dispatch(_setReadyStateTo(WSReadyState.CLOSED));
				dispatch(_setResponsesWaitingForTo([]));
			});
			Socket.on('error', (event) => {
				console.error('Websocket error', event);
			});
			Socket.on('message', (event: MessageEvent<string>) => {
				const message = event.data;
				const parsed = JSON.parse(message) as EmacsRecvMsg;
				if (parsed === null) return;
				dispatch(_recvMsgFromEmacs(parsed));
				if ('resid' in parsed) {
					dispatch(
						_removeFromResponsesWaitingFor(parsed?.resid || -1)
					);
				}
				// TODO: Hack to stop quick changes in match query or slow responses
				// Will occur when a getItem has been sent and overriden by a clock in
				// in Emacs.
				if (parsed.data?.CURRENT_CLOCK_START_TIMESTAMP) {
					dispatch(_setResponsesWaitingForTo([]));
				}
			});
		}
	},
});

/**
 * Close the websocket
 */
listenerMiddleware.startListening({
	actionCreator: _closeWS,
	effect: (_action, listenerApi) => {
		const { dispatch } = listenerApi;
		const {
			ws: { readyState },
		} = listenerApi.getState();
		// Don't assume it isn't already closed
		if (readyState !== WSReadyState.CLOSED) {
			dispatch(_setReadyStateTo(WSReadyState.CLOSING));
		}
		dispatch(_setResponsesWaitingForTo([]));
		Socket.disconnect();
	},
});

/**
 * Reset (open and close) the websocket
 */
listenerMiddleware.startListening({
	actionCreator: _resetWS,
	effect: async (_action, listenerApi) => {
		const { dispatch } = listenerApi;
		dispatch(_closeWS());
		// Wait for the websocket to fully close before opening it again
		await listenerApi.condition((_action, currentState) => {
			return currentState.ws.readyState === WSReadyState.CLOSED;
		});
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
			// TODO: Hack to stop quick changes in match query or slow responses
			// from emacs causing dropped responses (as they override each other)
			// in chronological order. Need a more built out solution when using
			// multiple types of requests.
			dispatch(_setResponsesWaitingForTo([]));
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

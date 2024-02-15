import {
	ResponseData,
	EmacsRecvMsg,
	EmacsSendMsgWithResid,
	MsgToTabType,
	WSReadyState,
	EmacsItemMsg,
	getTypeFromCommand,
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
import { flushData, resetData } from 'app/actions';

export interface WSState {
	readyState: WSReadyState;
	responsesWaitingFor: Array<ResponseData>;
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
		_addToResponsesWaitingFor: (
			state,
			action: PayloadAction<ResponseData>
		) => {
			const newResponsesWaitingFor = state.responsesWaitingFor
				.filter((res) => res.type !== action.payload.type)
				.concat(action.payload);
			return {
				...state,
				responsesWaitingFor: newResponsesWaitingFor,
			};
		},
		_removeFromResponsesWaitingFor: (
			state,
			action: PayloadAction<ResponseData>
		) => {
			const newResponsesWaitingFor = state.responsesWaitingFor
				.filter((res) => res.id !== action.payload.id)
				.filter((res) => res.type !== action.payload.type);
			return {
				...state,
				responsesWaitingFor: newResponsesWaitingFor,
			};
		},
		_setResponsesWaitingForTo: (
			state,
			action: PayloadAction<Array<ResponseData>>
		) => {
			return {
				...state,
				responsesWaitingFor: action.payload,
			};
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
				dispatch(_setResponsesWaitingForTo([]));
				dispatch(_setReadyStateTo(WSReadyState.CLOSED));
			});
			Socket.on('error', (event) => {
				console.error('Websocket error', event);
			});
			Socket.on('message', (event: MessageEvent<string>) => {
				const message = event.data;
				const parsed = JSON.parse(message) as EmacsRecvMsg;
				if (parsed === null) return;
				dispatch(_recvMsgFromEmacs(parsed));
				dispatch(
					_removeFromResponsesWaitingFor({
						id: (parsed as EmacsItemMsg)?.resid || -1,
						type: parsed?.type || '',
					})
				);
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
			Socket.sendJSON(toSend);
			const response: ResponseData = {
				id: resid,
				type: getTypeFromCommand(data.command),
			};
			dispatch(_addToResponsesWaitingFor(response));
			setTimeout(() => {
				dispatch(_removeFromResponsesWaitingFor(response));
			}, MAXIMUM_TIME_TO_WAIT_FOR_RESPONSE);
		} else {
			sendToMasterTab(MsgToTabType.PASS_TO_EMACS, data);
		}
	},
});

/**
 * Flush (write to storage) when a message is received from Emacs. Prevents race
 * conditions with storage overwriting new data with old data.
 */
listenerMiddleware.startListening({
	actionCreator: _recvMsgFromEmacs,
	effect: (_action, listenerApi) => {
		const { dispatch } = listenerApi;
		dispatch(flushData());
	},
});

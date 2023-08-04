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
	recvMsgFromEmacs,
	sendMsgToEmacs,
} from 'modules/emacs/emacsSlice';
import { getMasterWSTabId, sendMsgToTab } from 'lib/messages';

const MAXIMUM_TIME_TO_WAIT_FOR_RESPONSE = 60000;
export interface WSState {
	readyState: WSReadyState;
	responsesWaitingFor: Array<number>;
	wsPort: number;
}

const initialState: WSState = {
	readyState: WSReadyState.UNINSTANTIATED,
	responsesWaitingFor: [],
	wsPort: 35942,
};

export const wsSlice = createSlice({
	name: 'ws',
	initialState,
	reducers: {
		setReadyStateTo: (state, action: PayloadAction<WSReadyState>) => {
			state.readyState = action.payload;
		},
		removeFromResponsesWaitingFor: (
			state,
			action: PayloadAction<number>
		) => {
			state.responsesWaitingFor = state.responsesWaitingFor.filter(
				(id) => id !== action.payload
			);
		},
		addToResponsesWaitingFor: (state, action: PayloadAction<number>) => {
			state.responsesWaitingFor.push(action.payload);
		},
		setResponsesWaitingForTo: (
			state,
			action: PayloadAction<Array<number>>
		) => {
			state.responsesWaitingFor = action.payload;
		},
		openWS: () => {},
		closeWS: () => {},
		resetWS: () => {},
		setWSPortTo: (state, action: PayloadAction<number>) => {
			state.wsPort = action.payload;
		},
	},
});

export const {
	setReadyStateTo,
	removeFromResponsesWaitingFor,
	addToResponsesWaitingFor,
	setResponsesWaitingForTo,
	openWS,
	closeWS,
	resetWS,
	setWSPortTo,
} = wsSlice.actions;

export const selectedReadyState = (state: RootState) => state.ws.readyState;
export const selectedIsWaitingForResponse = (state: RootState) =>
	state.ws.responsesWaitingFor.length > 0;
export const selectedResponsesWaitingFor = (state: RootState) =>
	state.ws.responsesWaitingFor;
export const selectedWSPort = (state: RootState) => state.ws.wsPort;

export default wsSlice.reducer;

/**
 * Open the websocket (assuming master role)
 */
listenerMiddleware.startListening({
	predicate: (action, currentState) =>
		action.type === openWS.type && currentState.role.amMasterWS,
	effect: (_action, listenerApi) => {
		const { dispatch } = listenerApi;
		const getState = listenerApi.getState.bind(this);
		const {
			ws: { wsPort },
		} = getState();
		dispatch(setReadyStateTo(WSReadyState.CONNECTING));
		// eslint-disable-next-line no-console
		console.log('connecting');
		Socket.connect(`ws://localhost:${wsPort}/`);
		Socket.on('open', () => {
			// eslint-disable-next-line no-console
			console.log('open');
			dispatch(setReadyStateTo(WSReadyState.OPEN));
		});
		Socket.on('close', () => {
			// eslint-disable-next-line no-console
			console.log('close');
			dispatch(setReadyStateTo(WSReadyState.CLOSED));
		});
		Socket.on('error', (event) => {
			console.error('Websocket error', event.data);
		});
		Socket.on('message', (event) => {
			const message = event.data;
			const parsed = JSON.parse(message) as EmacsRecvMsg;
			if (parsed === null) return;
			dispatch(recvMsgFromEmacs(parsed));
			if (parsed.type === 'ITEM') {
				// TODO: more general case for this
				dispatch(removeFromResponsesWaitingFor(parsed?.resid || -1));
			}
		});
	},
});

/**
 * Close the websocket (role doesn't matter)
 */
listenerMiddleware.startListening({
	actionCreator: closeWS,
	effect: (_action, listenerApi) => {
		const { dispatch } = listenerApi;
		dispatch(setReadyStateTo(WSReadyState.CLOSING));
		dispatch(setResponsesWaitingForTo([]));
		Socket.disconnect();
	},
});

/**
 * Every time the websocket opens, ask Emacs for the current item
 * (assuming master role)
 */
listenerMiddleware.startListening({
	predicate: (action, currentState) =>
		action.type === setReadyStateTo.type &&
		currentState.role.amMasterWS &&
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
		action.type === sendMsgToEmacs.type &&
		currentState.ws.readyState === WSReadyState.OPEN,
	effect: (action, listenerApi) => {
		const { dispatch } = listenerApi;
		const getState = listenerApi.getState.bind(this);
		const {
			role: { amMasterWS },
		} = getState();
		if (amMasterWS) {
			const resid = Math.floor(Math.random() * 1000000000);
			const data = { ...action.payload, resid } as EmacsSendMsg;
			Socket.sendJSON(data);
			dispatch(addToResponsesWaitingFor(resid));
			setTimeout(() => {
				dispatch(removeFromResponsesWaitingFor(resid));
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

// TODO reconnecting

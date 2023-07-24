import { createListenerMiddleware, isAnyOf } from '@reduxjs/toolkit';

import {
	addToResponsesWaitingFor,
	becomeClientWS,
	becomeMasterWS,
	removeFromResponsesWaitingFor,
	sendMsgToEmacs,
	setOrgItemTo,
	setReadyStateTo,
	setTagsDataTo,
} from './stateReducer';
import Socket from 'lib/Socket';
import { EmacsRecvMsg, EmacsSendMsg, WSReadyState } from 'lib/types';
import { RootState } from 'store';

const MAXIMUM_TIME_TO_WAIT_FOR_RESPONSE = 60000;

const listenerMiddleware = createListenerMiddleware<RootState>();

listenerMiddleware.startListening({
	predicate: (action, _currentState, originalState) => {
		if (action.type === becomeMasterWS.type && !originalState.amMasterWS) {
			return true;
		}
		return false;
	},
	effect: (_action, listenerApi) => {
		const { dispatch } = listenerApi;
		dispatch(setReadyStateTo(WSReadyState.CONNECTING));
		// eslint-disable-next-line no-console
		console.log('connecting');
		Socket.connect('ws://localhost:35942/');
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
			switch (parsed.type) {
				case 'ITEM':
					dispatch(setOrgItemTo(parsed?.data || null));
					dispatch(
						removeFromResponsesWaitingFor(parsed?.resid || -1)
					);
					break;
				case 'TAGS':
					dispatch(setTagsDataTo(parsed?.data || {}));
					break;
				default:
					console.error('[NewTab] Unknown message: ', parsed);
					break;
			}
		});
	},
});

listenerMiddleware.startListening({
	predicate: (action, _currentState, originalState) => {
		if (action.type === becomeClientWS.type && originalState.amMasterWS) {
			return true;
		}
		return false;
	},
	effect: (_action, listenerApi) => {
		const { dispatch } = listenerApi;
		dispatch(setReadyStateTo(WSReadyState.CLOSING));
		Socket.disconnect();
	},
});

listenerMiddleware.startListening({
	matcher: isAnyOf(sendMsgToEmacs),
	effect: (action, listenerApi) => {
		if (action.type === sendMsgToEmacs.type) {
			const { dispatch } = listenerApi;
			const resid = Math.floor(Math.random() * 1000000000);
			const data = { ...action.payload, resid } as EmacsSendMsg;
			Socket.sendJSON(data);
			dispatch(addToResponsesWaitingFor(resid));
			setTimeout(() => {
				dispatch(
					removeFromResponsesWaitingFor(action.payload as number)
				);
			}, MAXIMUM_TIME_TO_WAIT_FOR_RESPONSE);
		}
	},
});

// TODO reconnecting
// TODO remove react-use-websocket

export default listenerMiddleware.middleware;

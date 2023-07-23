import { createListenerMiddleware, isAnyOf } from '@reduxjs/toolkit';

import {
	becomeClientWS,
	becomeMasterWS,
	sendMsgToEmacs,
	setOrgItemTo,
	setReadyStateTo,
	setTagsDataTo,
} from './stateReducer';
import Socket from 'lib/Socket';
import { ReadyState } from 'react-use-websocket';
import { EmacsRecvMsg, EmacsSendMsg } from 'lib/types';
import { RootState } from 'store';

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
		dispatch(setReadyStateTo(ReadyState.CONNECTING));
		// eslint-disable-next-line no-console
		console.log('connecting');
		Socket.connect('ws://localhost:35942/');
		Socket.on('open', () => {
			// eslint-disable-next-line no-console
			console.log('open');
			dispatch(setReadyStateTo(ReadyState.OPEN));
		});
		Socket.on('close', () => {
			// eslint-disable-next-line no-console
			console.log('close');
			dispatch(setReadyStateTo(ReadyState.CLOSED));
		});
		Socket.on('error', (event) => {
			console.error('Websocket error', event.data);
		});
		Socket.on('message', (event) => {
			const message = event.data;
			const parsed = JSON.parse(message) as EmacsRecvMsg;
			// eslint-disable-next-line no-console
			console.log(parsed);
			if (parsed === null) return;
			switch (parsed.type) {
				case 'ITEM':
					dispatch(setOrgItemTo(parsed?.data || null));
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
		dispatch(setReadyStateTo(ReadyState.CLOSING));
		Socket.disconnect();
	},
});

listenerMiddleware.startListening({
	matcher: isAnyOf(sendMsgToEmacs),
	effect: (action) => {
		if (action.type === sendMsgToEmacs.type) {
			Socket.sendJSON(action.payload as EmacsSendMsg);
		}
	},
});

// TODO reconnecting
// TODO remove react-use-websocket

export default listenerMiddleware.middleware;

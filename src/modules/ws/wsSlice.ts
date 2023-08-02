import {
	EmacsRecvMsg,
	EmacsSendMsg,
	MsgDirection,
	MsgToBGSWType,
	MsgToTab,
	MsgToTabType,
	SendJsonMessage,
	WSReadyState,
	WSStateMsg,
	getMsgToTabType,
} from 'lib/types';
import { PayloadAction, createSlice } from '@reduxjs/toolkit';
import { RootState } from 'app/store';
import Socket from 'lib/Socket';
import { listenerMiddleware } from 'app/middleware';
import { recvMsgFromEmacs, sendMsgToEmacs } from 'modules/emacs/emacsSlice';
import {
	SendResponseType,
	handleConfirmingAlive,
	handleConfirmingRoleAsMaster,
	handlePassingMessage,
	sendMsgToAllTabs,
	sendMsgToBGSWPort,
	sendMsgToTab,
	sendUpdateInWSState,
} from 'lib/messages';
import { LogLoc, LogMsgDir, logMsg } from 'lib/logging';
import Port from 'lib/Port';

const MAXIMUM_TIME_TO_WAIT_FOR_RESPONSE = 60000;
export interface WSState {
	amMasterWS: boolean;
	readyState: WSReadyState;
	responsesWaitingFor: Array<number>;
}

const initialState: WSState = {
	amMasterWS: false,
	readyState: WSReadyState.UNINSTANTIATED,
	responsesWaitingFor: [],
};

export const wsSlice = createSlice({
	name: 'ws',
	initialState,
	reducers: {
		becomeMasterWS: (state) => {
			state.amMasterWS = true;
		},
		becomeClientWS: (state) => {
			state.amMasterWS = false;
		},
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
		establishRole: () => {},
	},
});

export const {
	becomeMasterWS,
	becomeClientWS,
	setReadyStateTo,
	removeFromResponsesWaitingFor,
	addToResponsesWaitingFor,
	setResponsesWaitingForTo,
	establishRole,
} = wsSlice.actions;

export const selectedReadyState = (state: RootState) => state.ws.readyState;
export const selectedAmMasterWs = (state: RootState) => state.ws.amMasterWS;
export const selectedIsWaitingForResponse = (state: RootState) =>
	state.ws.responsesWaitingFor.length > 0;
export const selectedResponsesWaitingFor = (state: RootState) =>
	state.ws.responsesWaitingFor;

export default wsSlice.reducer;

const getMasterWSTabId = async () => {
	const masterWSObject = await chrome.storage.local.get('masterWSTabId');
	const { masterWSTabId } = masterWSObject;
	const masterWSTabAsNumber =
		masterWSTabId && typeof masterWSTabId === 'string'
			? parseInt(masterWSTabId, 10)
			: null;

	return masterWSTabAsNumber;
};

listenerMiddleware.startListening({
	predicate: (action, _currentState, originalState) =>
		action.type === becomeMasterWS.type && !originalState.ws.amMasterWS,
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
			dispatch(recvMsgFromEmacs(parsed));
			if (parsed.type === 'ITEM') {
				dispatch(removeFromResponsesWaitingFor(parsed?.resid || -1));
			}
		});
	},
});

listenerMiddleware.startListening({
	predicate: (action, _currentState, originalState) =>
		action.type === becomeClientWS.type && originalState.ws.amMasterWS,
	effect: (_action, listenerApi) => {
		const { dispatch } = listenerApi;
		dispatch(setReadyStateTo(WSReadyState.CLOSING));
		Socket.disconnect();
	},
});

listenerMiddleware.startListening({
	actionCreator: sendMsgToEmacs,
	effect: (action, listenerApi) => {
		const { dispatch } = listenerApi;
		const getState = listenerApi.getState.bind(this);
		const { amMasterWS, readyState } = getState().ws;
		if (amMasterWS && readyState === WSReadyState.OPEN) {
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

listenerMiddleware.startListening({
	actionCreator: establishRole,
	effect: (_action, listenerApi) => {
		const { dispatch } = listenerApi;
		const getState = listenerApi.getState.bind(this);

		const sendJsonMessage: SendJsonMessage = (
			jsonMessage: EmacsSendMsg
		) => {
			if (getState().ws.amMasterWS) {
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
			const { amMasterWS, readyState, responsesWaitingFor } =
				getState().ws;
			if (amMasterWS) {
				sendMsgToAllTabs(MsgToTabType.SET_WS_STATE, {
					readyState,
					responsesWaitingFor,
				});
			}
		};

		const handleUpdateStateOfWS = (message: MsgToTab) => {
			if (!getState().ws.amMasterWS && message?.data) {
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
			void getMasterWSTabId().then((masterWSTabAsNumber) => {
				const { readyState, responsesWaitingFor } = getState().ws;
				if (masterWSTabAsNumber) {
					sendMsgToTab(
						MsgToTabType.QUERY_WS_STATE,
						masterWSTabAsNumber,
						{
							readyState,
							responsesWaitingFor,
						}
					);
				}
			});
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
						getState().ws.amMasterWS
					);
					break;
				case MsgToTabType.SET_ROLE_MASTER:
					dispatch(becomeMasterWS());
					break;
				case MsgToTabType.SET_ROLE_CLIENT:
					dispatch(becomeClientWS());
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
			}
		};

		if (!chrome.runtime.onMessage.hasListener(handleMessage)) {
			chrome.runtime.onMessage.addListener(handleMessage);
		}

		// 1. Ask if any master web sockets exist
		sendMsgToBGSWPort(MsgToBGSWType.QUERY_WS_ROLE, Port.port);
	},
});

listenerMiddleware.startListening({
	predicate: (action, currentState) =>
		currentState.ws.amMasterWS &&
		(action.type === removeFromResponsesWaitingFor.type ||
			action.type === addToResponsesWaitingFor.type ||
			action.type === setResponsesWaitingForTo.type ||
			action.type === setReadyStateTo.type),
	effect: (action, listenerApi) => {
		const getState = listenerApi.getState.bind(this);
		const { responsesWaitingFor, readyState } = getState().ws;
		if (action.type === setReadyStateTo.type) {
			sendUpdateInWSState({ readyState });
		} else {
			sendUpdateInWSState({ responsesWaitingFor });
		}
	},
});

// TODO reconnecting

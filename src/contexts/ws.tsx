import { createContext, useCallback, useEffect, useRef } from 'react';
import {
	MsgDirection,
	type MsgToTab,
	MsgToTabType,
	MsgToBGSWType,
	type WSCommonProps,
	getMsgToTabType,
	WSStateMsg,
	EmacsSendMsg,
} from '../lib/types';
import {
	SendResponseType,
	handleConfirmingAlive,
	handleMasterQueryConfirmation,
	sendMsgToBGSWPort,
	sendMsgToTabAsResponse,
} from '../lib/messages';
import useSingleWebsocket from 'hooks/useSingleWS';
import { LogLoc, LogMsgDir, logMsg, logMsgErr } from 'lib/logging';
import usePort from 'hooks/usePort';
import { useAppDispatch, useAppSelector } from '../hooks';
import {
	becomeClientWS,
	becomeMasterWS,
	setReadyStateTo,
	startWaitingForResponse,
	stopWaitingForResponse,
} from '../stateReducer';

export type WSContextProps = {
	updateMatchQuery: (matchQuery: string) => void;
	getItem: (matchQuery: string) => void;
} & WSCommonProps;

const WSContext = createContext<WSContextProps>({
	sendJsonMessage: () => {
		return;
	},
	lastRecvJsonMessage: null,
	updateMatchQuery: () => {},
	getItem: () => {},
});

export default WSContext;

export const WSProvider: React.FC<{ children?: React.ReactNode }> = ({
	children,
}) => {
	const dispatch = useAppDispatch();
	const amMasterWS = useAppSelector((state) => state.amMasterWS);
	const readyState = useAppSelector((state) => state.readyState);
	const isWaitingForResponse = useAppSelector(
		(state) => state.isWaitingForResponse
	);
	const { sendJsonMessage, lastRecvJsonMessage } = useSingleWebsocket();
	const port = usePort();

	const isInitialRender = useRef(true);

	const updateMatchQuery = useCallback(
		(newMatchQuery: string) =>
			sendJsonMessage({
				command: 'updateMatchQuery',
				data: newMatchQuery,
			}),
		[sendJsonMessage]
	);

	const getItem = useCallback(
		(matchQuery: string) =>
			sendJsonMessage({
				command: 'getItem',
				data: matchQuery,
			}),
		[sendJsonMessage]
	);

	const handlePassingMessage = useCallback(
		(message: MsgToTab) => {
			if (message.data) {
				sendJsonMessage(message.data as EmacsSendMsg);
			} else {
				logMsgErr(
					LogLoc.NEWTAB,
					LogMsgDir.RECV,
					'Bad or no data for updating match query',
					message?.data
				);
			}
		},
		[sendJsonMessage]
	);

	const handleQueryStateOfWS = useCallback(
		(sendResponse: SendResponseType) => {
			if (amMasterWS) {
				sendMsgToTabAsResponse(
					MsgToTabType.SET_WS_STATE,
					sendResponse,
					{
						readyState,
						isWaitingForResponse,
					}
				);
			}
		},
		[amMasterWS, readyState, isWaitingForResponse]
	);

	const handleUpdateStateOfWS = useCallback(
		(message: MsgToTab) => {
			if (!amMasterWS && message?.data) {
				const {
					isWaitingForResponse: isWaitingForResponseFromMaster,
					readyState: readyStateFromMaster,
				} = message.data as WSStateMsg;
				if (typeof readyStateFromMaster === 'number') {
					dispatch(setReadyStateTo(readyStateFromMaster));
				}
				if (typeof isWaitingForResponseFromMaster === 'boolean') {
					dispatch(
						isWaitingForResponseFromMaster
							? startWaitingForResponse()
							: stopWaitingForResponse()
					);
				}
			}
		},
		[amMasterWS, dispatch]
	);

	const handleMessage = useCallback(
		(
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
					handleMasterQueryConfirmation(sendResponse, amMasterWS);
					break;
				case MsgToTabType.SET_ROLE_MASTER:
					dispatch(becomeMasterWS());
					break;
				case MsgToTabType.SET_ROLE_CLIENT:
					dispatch(becomeClientWS());
					break;
				case MsgToTabType.QUERY_ALIVE:
					handleConfirmingAlive(sendResponse);
					break;
				case MsgToTabType.PASS_TO_EMACS:
					handlePassingMessage(message);
					break;
				case MsgToTabType.QUERY_WS_STATE:
					handleQueryStateOfWS(sendResponse);
					break;
				case MsgToTabType.SET_WS_STATE:
					handleUpdateStateOfWS(message);
					break;
			}
		},
		[
			amMasterWS,
			dispatch,
			handlePassingMessage,
			handleQueryStateOfWS,
			handleUpdateStateOfWS,
		]
	);

	useEffect(() => {
		// TODO: confirm one add
		if (!chrome.runtime.onMessage.hasListener(handleMessage)) {
			chrome.runtime.onMessage.addListener(handleMessage);
		}
		return () => {
			chrome.runtime.onMessage.removeListener(handleMessage);
		};
	}, [handleMessage]);

	useEffect(() => {
		if (isInitialRender.current) {
			// 1. Ask if any master web sockets exist
			sendMsgToBGSWPort(MsgToBGSWType.QUERY_WS_ROLE, port);
			isInitialRender.current = false;
		}
	}, [port]);

	return (
		<WSContext.Provider
			value={{
				sendJsonMessage,
				lastRecvJsonMessage,
				updateMatchQuery,
				getItem,
			}}
		>
			{children}
		</WSContext.Provider>
	);
};

export const { Consumer: WSConsumer } = WSContext;

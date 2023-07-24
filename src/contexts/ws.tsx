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
	sendMsgToAllTabs,
	sendMsgToBGSWPort,
	sendMsgToTab,
} from '../lib/messages';
import useSingleWebsocket from 'hooks/useSingleWS';
import { LogLoc, LogMsgDir, logMsg, logMsgErr } from 'lib/logging';
import usePort from 'hooks/usePort';
import { useAppDispatch, useAppSelector } from '../hooks';
import {
	becomeClientWS,
	becomeMasterWS,
	setReadyStateTo,
	setResponsesWaitingForTo,
} from '../stateReducer';

export type WSContextProps = {
	updateMatchQuery: (matchQuery: string) => void;
	getItem: (matchQuery: string) => void;
} & WSCommonProps;

const WSContext = createContext<WSContextProps>({
	sendJsonMessage: () => {
		return;
	},
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
	const responsesWaitingFor = useAppSelector(
		(state) => state.responsesWaitingFor
	);
	const { sendJsonMessage } = useSingleWebsocket();
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

	const handleQueryStateOfWS = useCallback(() => {
		if (amMasterWS) {
			sendMsgToAllTabs(MsgToTabType.SET_WS_STATE, {
				readyState,
				responsesWaitingFor,
			});
		}
	}, [amMasterWS, readyState, responsesWaitingFor]);

	const handleUpdateStateOfWS = useCallback(
		(message: MsgToTab) => {
			if (!amMasterWS && message?.data) {
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
		},
		[amMasterWS, dispatch]
	);

	const queryStateOfWS = useCallback(() => {
		// TODO: DRY
		void chrome.storage.local
			.get('masterWSTabId')
			.then((masterWSObject) => {
				const { masterWSTabId } = masterWSObject;
				const masterWSTabAsNumber =
					masterWSTabId && typeof masterWSTabId === 'string'
						? parseInt(masterWSTabId, 10)
						: null;
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
	}, [readyState, responsesWaitingFor]);

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
					queryStateOfWS();
					break;
				case MsgToTabType.QUERY_ALIVE:
					handleConfirmingAlive(sendResponse);
					break;
				case MsgToTabType.PASS_TO_EMACS:
					handlePassingMessage(message);
					break;
				case MsgToTabType.QUERY_WS_STATE:
					handleQueryStateOfWS();
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
			queryStateOfWS,
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
				updateMatchQuery,
				getItem,
			}}
		>
			{children}
		</WSContext.Provider>
	);
};

export const { Consumer: WSConsumer } = WSContext;

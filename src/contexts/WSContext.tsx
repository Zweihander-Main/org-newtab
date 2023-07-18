import { createContext, useCallback, useEffect, useRef } from 'react';
import {
	MsgDirection,
	type MsgToTab,
	MsgToTabType,
	MsgToBGSWType,
	type MsgToBGSW,
	type WSCommonProps,
	getMsgToBGSWType,
	getMsgToTabType,
} from '../util/types';
import useSingleWebsocket from 'hooks/useSingleWebsocket';
import { LogLoc, LogMsgDir, logMsg, logMsgErr } from 'util/logging';
import usePort from 'hooks/usePort';

const sendMsgToBGSWPort = (type: MsgToBGSWType, port: chrome.runtime.Port) => {
	logMsg(
		LogLoc.NEWTAB,
		LogMsgDir.SEND,
		'Sending message to BGSW port',
		getMsgToBGSWType(type)
	);
	port.postMessage({
		type,
		direction: MsgDirection.TO_BGSW,
	});
};

const sendMsgAsResponse = (
	type: MsgToBGSWType,
	sendResponse: SendResponseType
) => {
	logMsg(
		LogLoc.NEWTAB,
		LogMsgDir.SEND,
		'Sending response to BGSW msg',
		getMsgToBGSWType(type)
	);
	sendResponse({
		type,
		direction: MsgDirection.TO_BGSW,
	});
};

const sendMsgToTab = (type: MsgToTabType, tabId: number, data?: string) => {
	logMsg(
		LogLoc.NEWTAB,
		LogMsgDir.SEND,
		'Sending request to master tab',
		tabId,
		getMsgToTabType(type),
		data ? data : ''
	);
	void chrome.tabs.sendMessage<MsgToTab>(tabId, {
		direction: MsgDirection.TO_NEWTAB,
		type,
		data,
	});
};

const handleMasterQueryConfirmation = (
	sendResponse: SendResponseType,
	amMasterWS: boolean
) => {
	if (amMasterWS) {
		sendMsgAsResponse(MsgToBGSWType.IDENTIFY_AS_MASTER_WS, sendResponse);
	} else {
		sendMsgAsResponse(MsgToBGSWType.IDENTIFY_AS_WS_CLIENT, sendResponse);
	}
};

const handleConfirmingAlive = (sendResponse: SendResponseType) => {
	sendMsgAsResponse(MsgToBGSWType.CONFIRMED_ALIVE, sendResponse);
};

type SendResponseType = (message: MsgToBGSW) => unknown;

export type WSContextProps = {
	amMasterWS: boolean;
	updateMatchQuery: (matchQuery: string) => Promise<void>;
} & WSCommonProps;

const WSContext = createContext<WSContextProps>({
	amMasterWS: false,
	sendJsonMessage: () => {
		return;
	},
	lastRecvJsonMessage: null,
	updateMatchQuery: () => Promise.resolve(),
});

export default WSContext;

export const WSProvider: React.FC<{ children?: React.ReactNode }> = ({
	children,
}) => {
	const { sendJsonMessage, lastRecvJsonMessage, amMasterWS, setAmMasterWS } =
		useSingleWebsocket();
	const port = usePort();

	const isInitialRender = useRef(true);

	const handleUpdatingMatchQuery = useCallback(
		(newMatchQuery: string) => {
			sendJsonMessage({
				command: 'updateMatchQuery',
				data: newMatchQuery,
			});
		},
		[sendJsonMessage]
	);

	const updateMatchQuery = useCallback(
		async (newMatchQuery: string) => {
			if (amMasterWS) {
				handleUpdatingMatchQuery(newMatchQuery);
			} else {
				const masterWSObject = await chrome.storage.local.get(
					'masterWSTabId'
				);
				const { masterWSTabId } = masterWSObject;
				const masterWSTabAsNumber =
					masterWSTabId && typeof masterWSTabId === 'string'
						? parseInt(masterWSTabId, 10)
						: null;
				if (masterWSTabAsNumber) {
					sendMsgToTab(
						MsgToTabType.UPDATE_MATCH_QUERY,
						masterWSTabAsNumber,
						newMatchQuery
					);
				}
			}
		},
		[amMasterWS, handleUpdatingMatchQuery]
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
				message?.data ? `with data ${message.data}` : ''
			);
			switch (message.type) {
				case MsgToTabType.CONFIRM_IF_MASTER_WS:
					handleMasterQueryConfirmation(sendResponse, amMasterWS);
					break;
				case MsgToTabType.YOU_ARE_MASTER_WS:
					setAmMasterWS(true);
					break;
				case MsgToTabType.YOU_ARE_CLIENT_WS:
					setAmMasterWS(false);
					break;
				case MsgToTabType.CONFIRM_IF_ALIVE:
					handleConfirmingAlive(sendResponse);
					break;
				case MsgToTabType.UPDATE_MATCH_QUERY:
					if (message.data && typeof message.data === 'string') {
						handleUpdatingMatchQuery(message.data);
					} else {
						logMsgErr(
							LogLoc.NEWTAB,
							LogMsgDir.RECV,
							'Bad or no data for updating match query',
							message?.data
						);
					}
					break;
			}
		},
		[setAmMasterWS, amMasterWS, handleUpdatingMatchQuery]
	);

	useEffect(() => {
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
			sendMsgToBGSWPort(MsgToBGSWType.QUERY_STATUS_OF_WS, port);
			isInitialRender.current = false;
		}
	}, [port]);

	return (
		<WSContext.Provider
			value={{
				amMasterWS,
				sendJsonMessage,
				lastRecvJsonMessage,
				updateMatchQuery,
			}}
		>
			{children}
		</WSContext.Provider>
	);
};

export const { Consumer: WSConsumer } = WSContext;

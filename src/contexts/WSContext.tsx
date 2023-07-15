import { createContext, useCallback, useEffect, useRef } from 'react';
import { ReadyState } from 'react-use-websocket';
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
import { LogLoc, LogMsgDir, logMsg } from 'util/logging';
import useMasterWS from 'hooks/useMasterWS';
import usePort from 'hooks/usePort';

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
	readyState: ReadyState.UNINSTANTIATED,
	updateMatchQuery: () => Promise.resolve(),
});

export default WSContext;

export const WSProvider: React.FC<{ children?: React.ReactNode }> = ({
	children,
}) => {
	const [amMasterWS, setAmMasterWS] = useMasterWS();
	const { sendJsonMessage, lastRecvJsonMessage, readyState } =
		useSingleWebsocket(amMasterWS);
	const port = usePort();

	const isInitialRender = useRef(true);
	const sendMsgToBGSWPort = useCallback(
		(type: MsgToBGSWType) => {
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
		},
		[port]
	);

	const sendMsgAsResponse = useCallback(
		(type: MsgToBGSWType, sendResponse: SendResponseType) => {
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
		},
		[]
	);

	const setAsMaster = useCallback(() => {
		if (amMasterWS === false) {
			setAmMasterWS(true);
		}
	}, [amMasterWS, setAmMasterWS]);

	const setAsClient = useCallback(() => {
		if (amMasterWS === true) {
			setAmMasterWS(false);
		}
	}, [amMasterWS, setAmMasterWS]);

	const handleMasterQueryConfirmation = useCallback(
		(sendResponse: SendResponseType) => {
			if (amMasterWS) {
				sendMsgAsResponse(
					MsgToBGSWType.IDENTIFY_AS_MASTER_WS,
					sendResponse
				);
			} else {
				sendMsgAsResponse(
					MsgToBGSWType.IDENTIFY_AS_WS_CLIENT,
					sendResponse
				);
			}
		},
		[amMasterWS, sendMsgAsResponse]
	);

	const handleConfirmingAlive = useCallback(
		(sendResponse: SendResponseType) => {
			sendMsgAsResponse(MsgToBGSWType.CONFIRMED_ALIVE, sendResponse);
		},
		[sendMsgAsResponse]
	);

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
					logMsg(
						LogLoc.NEWTAB,
						LogMsgDir.SEND,
						'Sending update match query request to masterWS',
						masterWSTabAsNumber
					);
					void chrome.tabs.sendMessage<MsgToTab>(
						masterWSTabAsNumber,
						{
							direction: MsgDirection.TO_NEWTAB,
							type: MsgToTabType.UPDATE_MATCH_QUERY,
							data: newMatchQuery,
						}
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
					handleMasterQueryConfirmation(sendResponse);
					break;
				case MsgToTabType.YOU_ARE_MASTER_WS:
					setAsMaster();
					break;
				case MsgToTabType.YOU_ARE_CLIENT_WS:
					setAsClient();
					break;
				case MsgToTabType.CONFIRM_IF_ALIVE:
					handleConfirmingAlive(sendResponse);
					break;
				case MsgToTabType.UPDATE_MATCH_QUERY:
					if (message.data && typeof message.data === 'string') {
						handleUpdatingMatchQuery(message.data);
					}
					break;
			}
		},
		[
			setAsMaster,
			setAsClient,
			handleMasterQueryConfirmation,
			handleConfirmingAlive,
			handleUpdatingMatchQuery,
		]
	);

	useEffect(() => {
		chrome.runtime.onMessage.addListener(handleMessage);
		return () => {
			chrome.runtime.onMessage.removeListener(handleMessage);
		};
	}, [amMasterWS, handleMessage]);

	useEffect(() => {
		if (isInitialRender.current) {
			// 1. Ask if any master web sockets exist
			sendMsgToBGSWPort(MsgToBGSWType.QUERY_STATUS_OF_WS);
			isInitialRender.current = false;
		}
	}, [sendMsgToBGSWPort]);

	return (
		<WSContext.Provider
			value={{
				amMasterWS,
				sendJsonMessage,
				lastRecvJsonMessage,
				readyState,
				updateMatchQuery,
			}}
		>
			{children}
		</WSContext.Provider>
	);
};

export const { Consumer: WSConsumer } = WSContext;

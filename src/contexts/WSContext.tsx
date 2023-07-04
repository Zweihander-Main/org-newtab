/* eslint-disable no-console */
import { createContext, useCallback, useEffect, useRef, useState } from 'react';
import {
	MsgDirection,
	type MsgBGSWToNewTab,
	MsgBGSWToNewTabType,
	MsgNewTabToBGSWType,
	type MsgNewTabToBGSW,
	type WSCommonProps,
	getMsgNewTabToBGSWType,
	getMsgBGSWToNewType,
} from '../types';
import { ReadyState } from 'react-use-websocket';
import useSingleWebsocket from 'hooks/useSingleWebsocket';

type SendResponseType = (message: MsgNewTabToBGSW) => unknown;

export type WSContextProps = {
	amMasterWS: boolean;
} & WSCommonProps;

const WSContext = createContext<WSContextProps>({
	amMasterWS: false,
	sendJsonMessage: () => {
		return;
	},
	lastRecvJsonMessage: null,
	readyState: ReadyState.UNINSTANTIATED,
});

export default WSContext;

export const WSProvider: React.FC<{ children?: React.ReactNode }> = ({
	children,
}) => {
	const [amMasterWS, setAmMasterWS] = useState(false);
	const { sendJsonMessage, lastRecvJsonMessage, readyState } =
		useSingleWebsocket(amMasterWS);

	const isInitialRender = useRef(true);
	const port = useRef(chrome.runtime.connect({ name: 'ws' }));

	const sendMsgToBGSWPort = useCallback((type: MsgNewTabToBGSWType) => {
		console.log(
			'[NewTab] => Sending message to BGSW port: %s',
			getMsgNewTabToBGSWType(type)
		);
		port.current.postMessage({
			type,
			direction: MsgDirection.TO_BGSW,
		});
	}, []);

	const sendMsgAsResponse = useCallback(
		(type: MsgNewTabToBGSWType, sendResponse: SendResponseType) => {
			console.log(
				'[NewTab] => Sending response to BGSW msg: %s',
				getMsgNewTabToBGSWType(type)
			);
			sendResponse({
				type,
				direction: MsgDirection.TO_BGSW,
			});
		},
		[]
	);

	const handleSetAsMaster = useCallback(() => {
		if (amMasterWS === false) {
			setAmMasterWS(true);
		}
	}, [amMasterWS, setAmMasterWS]);

	const handleSetAsClient = useCallback(() => {
		if (amMasterWS === true) {
			setAmMasterWS(false);
		}
	}, [amMasterWS, setAmMasterWS]);

	const handleMasterQueryConfirmation = useCallback(
		(sendResponse: SendResponseType) => {
			if (amMasterWS) {
				sendMsgAsResponse(
					MsgNewTabToBGSWType.IDENTIFY_AS_MASTER_WS,
					sendResponse
				);
			} else {
				sendMsgAsResponse(
					MsgNewTabToBGSWType.IDENTIFY_AS_WS_CLIENT,
					sendResponse
				);
			}
		},
		[amMasterWS, sendMsgAsResponse]
	);

	const handleConfirmingAlive = useCallback(
		(sendResponse: SendResponseType) => {
			sendMsgAsResponse(
				MsgNewTabToBGSWType.CONFIRMED_ALIVE,
				sendResponse
			);
		},
		[sendMsgAsResponse]
	);

	const handleMessage = useCallback(
		(
			message: MsgBGSWToNewTab,
			_sender: chrome.runtime.MessageSender,
			sendResponse: SendResponseType
		) => {
			if (message.direction !== MsgDirection.TO_NEWTAB) {
				return;
			}
			console.log(
				'[NewTab] <= Data recv from BGSW: %s',
				getMsgBGSWToNewType(message.type)
			);
			switch (message.type) {
				case MsgBGSWToNewTabType.CONFIRM_IF_MASTER_WS:
					handleMasterQueryConfirmation(sendResponse);
					break;
				case MsgBGSWToNewTabType.YOU_ARE_MASTER_WS:
					handleSetAsMaster();
					break;
				case MsgBGSWToNewTabType.YOU_ARE_CLIENT_WS:
					handleSetAsClient();
					break;
				case MsgBGSWToNewTabType.CONFIRM_IF_ALIVE:
					handleConfirmingAlive(sendResponse);
					break;
			}
		},
		[
			handleSetAsMaster,
			handleSetAsClient,
			handleMasterQueryConfirmation,
			handleConfirmingAlive,
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
			sendMsgToBGSWPort(MsgNewTabToBGSWType.QUERY_STATUS_OF_WS);
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
			}}
		>
			{children}
		</WSContext.Provider>
	);
};

export const { Consumer: WSConsumer } = WSContext;

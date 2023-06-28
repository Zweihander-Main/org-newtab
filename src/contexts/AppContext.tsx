/* eslint-disable no-console */
import {
	createContext,
	useCallback,
	useContext,
	useEffect,
	useRef,
	useState,
} from 'react';
import type { SendJsonMessage } from 'react-use-websocket/dist/lib/types';
import { ReadyState } from 'react-use-websocket';
import WSClientContext, { WSClientProvider } from './WSClientContext';
import WSMasterContext, { WSMasterProvider } from './WSMasterContext';
import {
	MsgBGSWToNewTabType,
	type MsgBGSWToNewTab,
	MsgNewTabToBGSWType,
	type MsgNewTabToBGSW,
} from '../background';

export type AllTagsRecv = string | Array<string | number>;

export type WebSocketItemMessage = {
	type: 'ITEM';
	data: {
		ITEM: string;
		ALLTAGS?: AllTagsRecv;
	};
};

export type WebSocketTagsMessage = {
	type: 'TAGS';
	data: {
		[key: string]: string;
	};
};

export type WebSocketRecvMessage =
	| WebSocketItemMessage
	| WebSocketTagsMessage
	| null;

export type WSContextProps = {
	amMasterWS: boolean;
	setAmMasterWS: (amMasterWS: boolean) => void;
};

export type WSCommonProps = {
	sendJsonMessage: SendJsonMessage;
	lastRecvJsonMessage: WebSocketRecvMessage;
	readyState: ReadyState;
};

const WSContext = createContext<WSContextProps>({
	amMasterWS: false,
	setAmMasterWS: () => {
		return;
	},
});

export default WSContext;

export const WSProvider: React.FC<{ children?: React.ReactNode }> = ({
	children,
}) => {
	const [amMasterWS, setAmMasterWS] = useState(false);

	const isInitialRender = useRef(true);
	const port = useRef(chrome.runtime.connect({ name: 'ws' }));
	const handleMessage = useCallback(
		(message: MsgBGSWToNewTab) => {
			console.log(
				'[NewTab] NewTab received message from BGSW: ',
				message.type
			);
			switch (message.type) {
				case MsgBGSWToNewTabType.CONFIRM_IF_MASTER_WS:
					if (amMasterWS) {
						port.current.postMessage({
							type: MsgNewTabToBGSWType.IDENTIFY_AS_MASTER_WS,
						} as MsgNewTabToBGSW);
					} else {
						port.current.postMessage({
							type: MsgNewTabToBGSWType.IDENTIFY_AS_WS_CLIENT,
						} as MsgNewTabToBGSW);
					}
					break;
				case MsgBGSWToNewTabType.YOU_ARE_CLIENT_WS:
					if (amMasterWS) {
						setAmMasterWS(false);
					}
					break;
				case MsgBGSWToNewTabType.YOU_ARE_MASTER_WS:
					if (!amMasterWS) {
						setAmMasterWS(true);
					}
					break;
			}
		},
		[amMasterWS, setAmMasterWS]
	);

	useEffect(() => {
		if (isInitialRender.current) {
			port.current.onMessage.addListener(handleMessage);
			// 1. Ask if any master web sockets exist
			port.current.postMessage({
				type: MsgNewTabToBGSWType.QUERY_STATUS_OF_WS,
			} as MsgNewTabToBGSW);
			isInitialRender.current = false;
			chrome.runtime.onMessage.addListener(
				(request, sender, sendResponse) => {
					console.log('[NewTab] request: ', request);
				}
			);
		}
	}, [handleMessage]);

	return (
		<WSContext.Provider value={{ amMasterWS, setAmMasterWS }}>
			{amMasterWS ? (
				<WSMasterProvider>{children}</WSMasterProvider>
			) : (
				<WSClientProvider>{children}</WSClientProvider>
			)}
		</WSContext.Provider>
	);
};

export const { Consumer: AppConsumer } = WSContext;

export const useWSContext = () => {
	const { amMasterWS } = useContext(WSContext);
	const contextToUse = amMasterWS ? WSMasterContext : WSClientContext;

	return useContext(contextToUse);
};

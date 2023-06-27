/* eslint-disable no-console */
import {
	createContext,
	useRef,
	type ReactNode,
	useCallback,
	useEffect,
	useContext,
} from 'react';
import type { WSCommonProps, WebSocketRecvMessage } from './AppContext';
import { ReadyState } from 'react-use-websocket';
import type { SendJsonMessage } from 'react-use-websocket/dist/lib/types';
import {
	MsgBGSWToNewTabType,
	type MsgBGSWToNewTab,
	MsgNewTabToBSGWType,
	type MsgNewTabToBGSW,
} from '../background';
import AppContext from './AppContext';

const WSClientContext = createContext<WSCommonProps>({
	sendJsonMessage: () => {
		return;
	},
	lastRecvJsonMessage: null,
	readyState: ReadyState.UNINSTANTIATED,
});

export default WSClientContext;

export const WSClientProvider: React.FC<{ children?: ReactNode }> = ({
	children,
}) => {
	console.log('testing');
	// NEXT: For some reason, this is never called
	const isInitialRender = useRef(true);
	const { amMasterWS, setAmMasterWS } = useContext(AppContext);
	const sendJsonMessage: SendJsonMessage = () => {
		return;
	};
	const lastRecvJsonMessage: WebSocketRecvMessage = null;
	const readyState: ReadyState = ReadyState.UNINSTANTIATED;

	const port = useRef(chrome.runtime.connect());
	const handleMessage = useCallback(
		(message: MsgBGSWToNewTab) => {
			console.log('NewTab received message from BGSW: ', message);
			switch (message.type) {
				case MsgBGSWToNewTabType.CONFIRM_IF_MASTER_WS:
					if (amMasterWS) {
						port.current.postMessage({
							type: MsgNewTabToBSGWType.IDENTIFY_AS_MASTER_WS,
						} as MsgNewTabToBGSW);
					} else {
						port.current.postMessage({
							type: MsgNewTabToBSGWType.IDENTIFY_AS_WS_CLIENT,
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
		console.log('effect');
		if (isInitialRender.current) {
			// 1. Ask if any master web sockets exist
			port.current.postMessage({
				type: MsgNewTabToBSGWType.QUERY_STATUS_OF_WS,
			} as MsgNewTabToBGSW);
			port.current.onMessage.addListener(handleMessage);
			isInitialRender.current = false;
		}
	}, [handleMessage]);

	return (
		<WSClientContext.Provider
			value={{
				sendJsonMessage,
				lastRecvJsonMessage,
				readyState,
			}}
		>
			{children}
		</WSClientContext.Provider>
	);
};

export const { Consumer: WSClientConsumer } = WSClientContext;

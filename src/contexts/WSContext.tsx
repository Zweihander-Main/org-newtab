/* eslint-disable no-console */
import {
	createContext,
	useCallback,
	useContext,
	useEffect,
	useRef,
	useState,
} from 'react';
import WSClientContext, { WSClientProvider } from './WSClientContext';
import WSMasterContext, { WSMasterProvider } from './WSMasterContext';
import {
	MsgDirection,
	type MsgBGSWToNewTab,
	MsgBGSWToNewTabType,
	MsgNewTabToBGSWType,
	type MsgNewTabToBGSW,
} from '../types';

type SendResponseType = (message: MsgNewTabToBGSW) => unknown;

export type WSContextProps = {
	amMasterWS: boolean;
	setAmMasterWS: (amMasterWS: boolean) => void;
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

	const sendMsgToBGSWPort = useCallback((type: MsgNewTabToBGSWType) => {
		port.current.postMessage({
			type,
			direction: MsgDirection.TO_BGSW,
		});
	}, []);

	const sendMsgAsResponse = useCallback(
		(type: MsgNewTabToBGSWType, sendResponse: SendResponseType) => {
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

	const handleConfirmation = useCallback(
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
		[amMasterWS, sendMsgToBGSWPort]
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
				'[NewTab] handleMessage -- data recv: %d',
				message.type
			);
			switch (message.type) {
				case MsgBGSWToNewTabType.CONFIRM_IF_MASTER_WS:
					handleConfirmation(sendResponse);
					break;
				case MsgBGSWToNewTabType.YOU_ARE_MASTER_WS:
					handleSetAsMaster();
					break;
				case MsgBGSWToNewTabType.YOU_ARE_CLIENT_WS:
					handleSetAsClient();
					break;
			}
		},
		[handleSetAsMaster, handleSetAsClient, handleConfirmation]
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
		<WSContext.Provider value={{ amMasterWS, setAmMasterWS }}>
			{amMasterWS ? (
				<WSMasterProvider>{children}</WSMasterProvider>
			) : (
				<WSClientProvider>{children}</WSClientProvider>
			)}
		</WSContext.Provider>
	);
};

export const { Consumer: WSConsumer } = WSContext;

export const useWSContext = () => {
	const { amMasterWS } = useContext(WSContext);
	const contextToUse = amMasterWS ? WSMasterContext : WSClientContext;

	return { ...useContext(contextToUse), amMasterWS };
};

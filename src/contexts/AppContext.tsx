import { createContext, useContext, useState } from 'react';
import type { SendJsonMessage } from 'react-use-websocket/dist/lib/types';
import { ReadyState } from 'react-use-websocket';
import WSClientContext, { WSClientProvider } from './WSClientContext';
import WSMasterContext, { WSMasterProvider } from './WSMasterContext';

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

export type AppContextProps = {
	amMasterWS: boolean;
	setAmMasterWS: (amMasterWS: boolean) => void;
};

export type WSCommonProps = {
	sendJsonMessage: SendJsonMessage;
	lastRecvJsonMessage: WebSocketRecvMessage;
	readyState: ReadyState;
};

const AppContext = createContext<AppContextProps>({
	amMasterWS: false,
	setAmMasterWS: () => {
		return;
	},
});

export default AppContext;

export const WSProvider: React.FC<{ children?: React.ReactNode }> = ({
	children,
}) => {
	const [amMasterWS, setAmMasterWS] = useState(false);

	return (
		<AppContext.Provider value={{ amMasterWS, setAmMasterWS }}>
			{amMasterWS ? (
				<WSMasterProvider>{children}</WSMasterProvider>
			) : (
				<WSClientProvider>{children}</WSClientProvider>
			)}
		</AppContext.Provider>
	);
};

export const { Consumer: AppConsumer } = AppContext;

export const useWSContext = () => {
	const { amMasterWS } = useContext(AppContext);
	const contextToUse = amMasterWS ? WSMasterContext : WSClientContext;

	return useContext(contextToUse);
};

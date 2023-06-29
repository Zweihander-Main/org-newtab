import { createContext, type ReactNode } from 'react';
import useWebSocket, { ReadyState } from 'react-use-websocket';
import type { WebSocketRecvMessage, WSCommonProps } from '../types';

const WSMasterContext = createContext<WSCommonProps>({
	sendJsonMessage: () => {
		return;
	},
	lastRecvJsonMessage: null,
	readyState: ReadyState.UNINSTANTIATED,
});

export default WSMasterContext;

export const WSMasterProvider: React.FC<{ children?: ReactNode }> = ({
	children,
}) => {
	const {
		sendJsonMessage,
		lastJsonMessage: lastRecvJsonMessage,
		readyState,
	} = useWebSocket<WebSocketRecvMessage>('ws://localhost:35942/');
	return (
		<WSMasterContext.Provider
			value={{
				sendJsonMessage,
				lastRecvJsonMessage,
				readyState,
			}}
		>
			{children}
		</WSMasterContext.Provider>
	);
};

export const { Consumer: WSMasterConsumer } = WSMasterContext;

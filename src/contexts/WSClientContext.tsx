/* eslint-disable no-console */
import { createContext, type ReactNode } from 'react';
import { ReadyState } from 'react-use-websocket';
import type { SendJsonMessage } from 'react-use-websocket/dist/lib/types';
import type { WebSocketRecvMessage, WSCommonProps } from '../types';

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
	const sendJsonMessage: SendJsonMessage = () => {
		return;
	};
	const lastRecvJsonMessage: WebSocketRecvMessage = null;
	const readyState: ReadyState = ReadyState.UNINSTANTIATED;

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

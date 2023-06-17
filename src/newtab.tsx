import { useCallback, useEffect, useState } from 'react';
import useWebSocket, { ReadyState } from 'react-use-websocket';
import './newtab.css';

const IndexNewtab: React.FC = () => {
	// const [messageHistory, setMessageHistory] = useState<
	// 	Array<MessageEvent<string>>
	// >([]);

	const {
		sendMessage,
		lastMessage: lastRecvMessage,
		sendJsonMessage,
		lastJsonMessage: lastRecvJsonMessage,
		readyState,
		getWebSocket,
	} = useWebSocket('ws://localhost:35942/');

	// useEffect(() => {
	// 	if (lastRecvMessage !== null) {
	// 		setMessageHistory((prev) => prev.concat(lastRecvMessage));
	// 	}
	// }, [lastRecvMessage, setMessageHistory]);

	const handleClickSendMessage = useCallback(() => sendMessage('Hello'), []);

	const connectionStatus = {
		[ReadyState.CONNECTING]: 'Connecting',
		[ReadyState.OPEN]: 'Open',
		[ReadyState.CLOSING]: 'Closing',
		[ReadyState.CLOSED]: 'Closed',
		[ReadyState.UNINSTANTIATED]: 'Uninstantiated',
	}[readyState];
	return (
		<div className="app">
			<header className="app-header">
				<button
					onClick={handleClickSendMessage}
					disabled={readyState !== ReadyState.OPEN}
				>
					Click Me to send &apos;Hello&apos;
				</button>
				<p>The WebSocket is currently {connectionStatus}</p>
				{lastRecvMessage ? (
					<span>Last message: {lastRecvMessage.data}</span>
				) : null}
			</header>
		</div>
	);
};

export default IndexNewtab;

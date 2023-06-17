import { useCallback } from 'react';
import useWebSocket, { ReadyState } from 'react-use-websocket';
import './newtab.css';

const IndexNewtab: React.FC = () => {
	const {
		sendJsonMessage,
		lastJsonMessage: lastRecvJsonMessage,
		readyState,
	} = useWebSocket('ws://localhost:35942/');

	const handleClickSendMessage = useCallback(
		() => sendJsonMessage({ data: 'Hello' }),
		[sendJsonMessage]
	);

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
				{lastRecvJsonMessage ? (
					<>
						Last message:
						<div>
							<pre>
								{JSON.stringify(lastRecvJsonMessage, null, 2)}
							</pre>
						</div>
					</>
				) : null}
			</header>
		</div>
	);
};

export default IndexNewtab;

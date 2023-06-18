import { useCallback } from 'react';
import useWebSocket, { ReadyState } from 'react-use-websocket';
import './newtab.css';

const IndexNewtab: React.FC = () => {
	const {
		sendJsonMessage,
		lastJsonMessage: lastRecvJsonMessage,
		readyState,
	} = useWebSocket('ws://localhost:35942/');

	const handleFilterSubmit = useCallback(
		(event: React.FormEvent<HTMLFormElement>) => {
			event.preventDefault();
			const { currentTarget } = event;
			const data = new FormData(currentTarget);
			const filter = data.get('filter');
			if (filter && typeof filter === 'string') {
				sendJsonMessage({ action: 'changeFilter', data: filter });
			}
		},
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
				<form method="post" onSubmit={handleFilterSubmit}>
					<input
						type="text"
						name="filter"
						defaultValue='TODO="TODO"'
					/>
					<button type="submit">Pull data</button>
				</form>
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

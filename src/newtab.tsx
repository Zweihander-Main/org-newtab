import { useCallback, useState } from 'react';
import useWebSocket, { ReadyState } from 'react-use-websocket';
import './newtab.css';
import type { JsonValue } from 'react-use-websocket/dist/lib/types';

type OptionsMenuProps = {
	handleMatchQuerySubmit: (event: React.FormEvent<HTMLFormElement>) => void;
	lastRecvJsonMessage: JsonValue | null;
};

const OptionsMenu: React.FC<OptionsMenuProps> = ({
	handleMatchQuerySubmit,
	lastRecvJsonMessage,
}) => {
	const [optionsVisible, setOptionsVisible] = useState(false);

	const toggleMenu = useCallback(() => {
		setOptionsVisible(!optionsVisible);
	}, [optionsVisible]);

	const optionsMenuButtonClass = [
		'options-menu-button',
		optionsVisible ? 'active' : '',
	].join(' ');

	const optionsMenuClass = [
		'options-menu',
		optionsVisible ? 'active' : '',
	].join(' ');

	return (
		<>
			<button className={optionsMenuButtonClass} onClick={toggleMenu}>
				<div className="options-menu-button-bar1"></div>
				<div className="options-menu-button-bar2"></div>
				<div className="options-menu-button-bar3"></div>
			</button>
			<nav className={optionsMenuClass}>
				<form method="post" onSubmit={handleMatchQuerySubmit}>
					<input
						type="text"
						name="matchQuery"
						defaultValue='TODO="TODO"'
					/>
					<button type="submit">Pull data</button>
				</form>
				{lastRecvJsonMessage ? (
					<>
						Last message:
						<pre className="options-menu-json">
							{JSON.stringify(lastRecvJsonMessage, null, 2)}
						</pre>
					</>
				) : null}
			</nav>
		</>
	);
};

const IndexNewtab: React.FC = () => {
	const {
		sendJsonMessage,
		lastJsonMessage: lastRecvJsonMessage,
		readyState,
	} = useWebSocket('ws://localhost:35942/');

	const handleMatchQuerySubmit = useCallback(
		(event: React.FormEvent<HTMLFormElement>) => {
			event.preventDefault();
			const { currentTarget } = event;
			const data = new FormData(currentTarget);
			const matchQuery = data.get('matchQuery');
			if (matchQuery && typeof matchQuery === 'string') {
				sendJsonMessage({ action: 'changeFilter', data: matchQuery });
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
			<OptionsMenu
				handleMatchQuerySubmit={handleMatchQuerySubmit}
				lastRecvJsonMessage={lastRecvJsonMessage}
			/>
			<p>The WebSocket is currently {connectionStatus}</p>
		</div>
	);
};

export default IndexNewtab;

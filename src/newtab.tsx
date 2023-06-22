import { useCallback, useState } from 'react';
import useWebSocket, { ReadyState } from 'react-use-websocket';
import type { JsonValue } from 'react-use-websocket/dist/lib/types';
import '@fontsource/public-sans/700.css';
import './newtab.css';

type ConnectionStatusIndicatorProps = {
	readyState: ReadyState;
};

const ConnectionStatusIndicator: React.FC<ConnectionStatusIndicatorProps> = ({
	readyState,
}) => {
	const connectionStatus = {
		[ReadyState.CONNECTING]: 'Connecting',
		[ReadyState.OPEN]: 'Open',
		[ReadyState.CLOSING]: 'Closing',
		[ReadyState.CLOSED]: 'Closed',
		[ReadyState.UNINSTANTIATED]: 'Uninstantiated',
	}[readyState];

	return <p className="connection-status">{connectionStatus}</p>;
};

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

type OrgItemProps = {
	lastRecvJsonMessage: WebSocketRecvMessage;
};

const OrgItem: React.FC<OrgItemProps> = ({ lastRecvJsonMessage }) => {
	const itemText = lastRecvJsonMessage ? lastRecvJsonMessage?.ITEM : null;

	return <>{itemText && <div className="org-item">{itemText}</div>}</>;
};

type WebSocketRecvMessage = {
	ITEM: string;
} | null;

const IndexNewtab: React.FC = () => {
	const {
		sendJsonMessage,
		lastJsonMessage: lastRecvJsonMessage,
		readyState,
	} = useWebSocket<WebSocketRecvMessage>('ws://localhost:35942/');

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

	return (
		<div className="app">
			<OptionsMenu
				handleMatchQuerySubmit={handleMatchQuerySubmit}
				lastRecvJsonMessage={lastRecvJsonMessage}
			/>
			<ConnectionStatusIndicator readyState={readyState} />
			<OrgItem lastRecvJsonMessage={lastRecvJsonMessage} />
		</div>
	);
};

export default IndexNewtab;

import { useEffect, useState } from 'react';
import './newtab.css';

const OrgClock: React.FC = () => {
	const [clockedData, setClockedData] = useState(
		'No clocked data received from Emacs.'
	);

	useEffect(() => {
		// call websocket
	}, [setClockedData]);

	return <p>{clockedData}</p>;
};

let websocket: WebSocket | undefined;
const connect = (host: string | URL) => {
	if (websocket === undefined) {
		websocket = new WebSocket(host);
	}

	websocket.onopen = () => {
		if (websocket) {
			websocket.send(JSON.stringify({ msg: 'test' }));
		}
	};

	websocket.onmessage = (event) => {
		// eslint-disable-next-line no-console
		console.log(JSON.parse(event.data as unknown as string));
	};
};

function createWebSocketConnection() {
	if ('WebSocket' in window) {
		chrome.storage.local.get('instance', (data) => {
			connect(
				'wss://' +
					(data.instance as string) +
					'/ws/demoPushNotifications'
			);
		});
	}
}
//Close the websocket connection
function closeWebSocketConnection() {
	if (websocket != null || websocket != undefined) {
		websocket.close();
		websocket = undefined;
	}
}
const IndexNewtab: React.FC = () => {
	useEffect(() => {
		connect('ws://localhost:35942/');
	}, []);

	return (
		<div className="app">
			<header className="app-header">
				<OrgClock />
			</header>
		</div>
	);
};

export default IndexNewtab;

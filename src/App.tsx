import { useEffect, useState } from 'react';
import './App.css';

const OrgClock: React.FC = () => {
	const [clockedData, setClockedData] = useState(
		'No clocked data received from Emacs.'
	);

	useEffect(() => {
		// call websocket
	}, [setClockedData]);

	return <p>{clockedData}</p>;
};

// function createWebSocketConnection() {
// 	if ('WebSocket' in window) {
// 		chrome.storage.local.get('instance', function (data) {
// 			connect('wss://' + data.instance + '/ws/demoPushNotifications');
// 		});
// 	}
// }

var websocket;
function connect(host) {
	if (websocket === undefined) {
		websocket = new WebSocket(host);
	}

	websocket.onopen = function () {
		websocket.send(JSON.stringify({ msg: 'test' }));
	};

	websocket.onmessage = function (event) {
		console.log(JSON.parse(event.data));
	};
}

//Close the websocket connection
function closeWebSocketConnection() {
	if (websocket != null || websocket != undefined) {
		websocket.close();
		websocket = undefined;
	}
}
function App() {
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
}

export default App;

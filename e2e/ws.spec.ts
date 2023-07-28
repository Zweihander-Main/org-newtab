import {
	MASTER_MESSAGE,
	STATUS_LOCATOR,
	WEBSOCKET_PORT,
	WEBSOCKET_URL,
} from './common';
import { test, expect } from './fixture';
import WebSocket from 'ws';
import net from 'net';

function startTestWebSocketServer() {
	const wss = new WebSocket.Server({ port: WEBSOCKET_PORT });

	wss.on('connection', (ws) => {
		// ws.send('Hello from the test WebSocket server!');

		ws.on('message', (message) => {
			// eslint-disable-next-line no-console
			console.log('Received message from client:', message);
		});
	});
}

async function isPortInUse(port: number) {
	return new Promise((resolve) => {
		const server = net.createServer();
		server.once('error', (err: Error & { code: string }) => {
			if (err.code === 'EADDRINUSE') {
				resolve(true);
			} else {
				resolve(false);
			}
		});
		server.once('listening', () => {
			server.close();
			resolve(false);
		});
		server.listen(port);
	});
}
test('Should open a websocket connection to emacs from the master tab', async ({
	extensionId,
	context,
}) => {
	if (!(await isPortInUse(WEBSOCKET_PORT))) {
		startTestWebSocketServer();
	}
	const tab1 = await context.newPage();
	async function waitForWebsocket(): Promise<boolean> {
		return new Promise(function (resolve) {
			tab1.on('websocket', (ws) => {
				if (ws.url() === WEBSOCKET_URL) {
					resolve(true);
				}
				setTimeout(() => resolve(false), 5000);
				// ws.on('framesent', (event) => console.log(event.payload));
				// ws.on('framereceived', (event) => console.log(event.payload));
				// ws.on('close', () => console.log('WebSocket closed'));
			});
		});
	}

	await tab1.goto(`chrome-extension://${extensionId}/newtab.html`);
	await expect(tab1.getByTestId(STATUS_LOCATOR)).toContainText(
		MASTER_MESSAGE
	);
	expect(await waitForWebsocket()).toBeTruthy();
});

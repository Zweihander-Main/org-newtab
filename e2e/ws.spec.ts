import {
	CLIENT_MESSAGE,
	GET_ITEM_COMMAND,
	HOW_LONG_TO_WAIT_FOR_RESPONSE,
	HOW_LONG_TO_WAIT_FOR_WEBSOCKET,
	ITEM_TEXT_LOCATOR,
	MASTER_MESSAGE,
	STATUS_LOCATOR,
	WEBSOCKET_PORT,
	WEBSOCKET_URL,
	WSS_TEST_TEXT,
} from './common';
import { test, expect } from './fixture';
import WebSocket from 'ws';
import net from 'net';

function startTestWebSocketServer() {
	const wss = new WebSocket.Server({ port: WEBSOCKET_PORT });

	wss.on('connection', (ws) => {
		ws.on('message', (message) => {
			// eslint-disable-next-line no-console
			console.log('Received message from client:', message);
			ws.send(
				JSON.stringify({ type: 'ITEM', data: { ITEM: WSS_TEST_TEXT } })
			);
		});

		ws.on('close', () => {
			// eslint-disable-next-line no-console
			console.log('Client disconnected');
		});
	});
	return wss;
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

test.describe('WebSocket', () => {
	test.describe.configure({ mode: 'serial' });

	let websocketServer: WebSocket.Server | undefined;
	test.beforeEach(async () => {
		// Don't run while Emacs is running
		expect(await isPortInUse(WEBSOCKET_PORT)).toBe(false);
		websocketServer = startTestWebSocketServer();
	});

	test.afterEach(() => {
		if (websocketServer) {
			websocketServer.close();
		}
	});

	test('Should open a connection to emacs from the master tab', async ({
		extensionId,
		context,
	}) => {
		const tab1 = await context.newPage();
		async function websocketOpened(): Promise<boolean> {
			return new Promise(function (resolve) {
				tab1.on('websocket', (ws) => {
					if (ws.url() === WEBSOCKET_URL) {
						resolve(true);
					}
				});
				setTimeout(
					() => resolve(false),
					HOW_LONG_TO_WAIT_FOR_WEBSOCKET
				);
			});
		}

		await tab1.goto(`chrome-extension://${extensionId}/newtab.html`);
		await expect(tab1.getByTestId(STATUS_LOCATOR)).toContainText(
			MASTER_MESSAGE
		);
		expect(await websocketOpened()).toBeTruthy();
	});

	test('Should not open a connection to emacs from a client tab', async ({
		extensionId,
		context,
	}) => {
		const tabMaster = await context.newPage();
		const tabClient = await context.newPage();
		async function clientWebsocketOpened(): Promise<boolean> {
			return new Promise(function (resolve) {
				tabClient.on('websocket', (ws) => {
					if (ws.url() === WEBSOCKET_URL) {
						resolve(true);
					}
				});
				setTimeout(
					() => resolve(false),
					HOW_LONG_TO_WAIT_FOR_WEBSOCKET
				);
			});
		}

		await tabMaster.goto(`chrome-extension://${extensionId}/newtab.html`);
		await tabClient.goto(`chrome-extension://${extensionId}/newtab.html`);
		await expect(tabMaster.getByTestId(STATUS_LOCATOR)).toContainText(
			MASTER_MESSAGE
		);
		await expect(tabClient.getByTestId(STATUS_LOCATOR)).toContainText(
			CLIENT_MESSAGE
		);
		expect(await clientWebsocketOpened()).toBeFalsy();
	});

	test('Should ask for item after opening', async ({
		extensionId,
		context,
	}) => {
		const tab1 = await context.newPage();
		async function getItemMessageSent(): Promise<boolean> {
			return new Promise(function (resolve) {
				tab1.on('websocket', (ws) => {
					if (ws.url() === WEBSOCKET_URL) {
						ws.on('framesent', (data) => {
							const json = JSON.parse(data.payload as string) as {
								command?: string;
							};
							if (json?.command === GET_ITEM_COMMAND) {
								resolve(true);
							}
						});
						ws.on('close', () => resolve(false));
						ws.on('socketerror', () => resolve(false));
					}
				});
				setTimeout(() => resolve(false), HOW_LONG_TO_WAIT_FOR_RESPONSE);
			});
		}

		await tab1.goto(`chrome-extension://${extensionId}/newtab.html`);
		await expect(tab1.getByTestId(STATUS_LOCATOR)).toContainText(
			MASTER_MESSAGE
		);
		expect(await getItemMessageSent()).toBeTruthy();
	});

	test('Should update item text based on data from server', async ({
		extensionId,
		context,
	}) => {
		const tab1 = await context.newPage();
		await tab1.goto(`chrome-extension://${extensionId}/newtab.html`);
		await expect(tab1.getByTestId(STATUS_LOCATOR)).toContainText(
			MASTER_MESSAGE
		);
		async function getAnyDataSent(): Promise<boolean> {
			return new Promise(function (resolve) {
				tab1.on('websocket', (ws) => {
					if (ws.url() === WEBSOCKET_URL) {
						ws.on('framesent', () => {
							resolve(true);
						});
						ws.on('close', () => resolve(false));
						ws.on('socketerror', () => resolve(false));
					}
				});
				setTimeout(() => resolve(false), HOW_LONG_TO_WAIT_FOR_RESPONSE);
			});
		}
		expect(await getAnyDataSent()).toBeTruthy();
		await expect(tab1.getByTestId(ITEM_TEXT_LOCATOR)).toContainText(
			WSS_TEST_TEXT
		);
	});
});

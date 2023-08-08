/* eslint-disable @typescript-eslint/no-base-to-string */
/* eslint-disable no-console */
import {
	CLIENT_MESSAGE,
	CONNECTION_STATUS_LOCATOR,
	CONNECTION_STATUS_OPEN,
	GET_ITEM_COMMAND,
	HOW_LONG_TO_WAIT_FOR_RESPONSE,
	HOW_LONG_TO_WAIT_FOR_WEBSOCKET,
	ITEM_TEXT_LOCATOR,
	MASTER_MESSAGE,
	MATCH_QUERY_LABEL,
	ROLE_LOCATOR,
	WSS_TEST_TEXT,
	WS_PORT_LABEL,
	INITIAL_STATE_LOCATOR,
	INITIAL_STATE_RESOLVED,
	HOW_LONG_TO_WAIT_FOR_STORAGE,
	LOADING_BAR_LOCATOR,
} from './common';
import { test, expect } from './fixture';
import WebSocket from 'ws';
import net from 'net';
import { Page } from '@playwright/test';
import { DEFAULT_WEBSOCKET_PORT } from 'lib/constants';

function startTestWebSocketServer(port: number) {
	const wss = new WebSocket.Server({ port: port });

	wss.on('connection', (ws) => {
		ws.on('message', (message) => {
			console.log('Received message from client: %s', message);
			let resid = -1;
			try {
				const parsed = JSON.parse(message.toString()) as {
					command: string;
					data: string;
					resid: number;
				};
				resid = parsed?.resid || -1;
			} catch {
				console.log('Could not parse message, not JSON.');
			}
			const toSend = JSON.stringify({
				type: 'ITEM',
				data: { ITEM: WSS_TEST_TEXT },
				resid,
			});
			console.log('Sending response', toSend);
			ws.send(toSend);
		});

		ws.on('close', () => {
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

async function pickARandomPort() {
	const port = Math.floor(Math.random() * (55000 - 10000 + 1)) + 10000;
	if (!(await isPortInUse(port)) && port !== DEFAULT_WEBSOCKET_PORT) {
		return port;
	} else {
		return pickARandomPort();
	}
}

async function openSocketConnection() {
	const port = await pickARandomPort();
	const wss = startTestWebSocketServer(port);
	return { port, wss };
}

async function setupWebsocketPort(
	conn: Awaited<ReturnType<typeof openSocketConnection>>,
	tab: Page
) {
	await expect(tab.getByTestId(INITIAL_STATE_LOCATOR)).toContainText(
		INITIAL_STATE_RESOLVED,
		{ timeout: HOW_LONG_TO_WAIT_FOR_STORAGE }
	);
	await tab.getByLabel(WS_PORT_LABEL).fill(conn.port.toString());
	await tab.getByLabel(WS_PORT_LABEL).press('Enter');
	await expect(tab.getByLabel(WS_PORT_LABEL)).toHaveValue(
		conn.port.toString()
	);
}

function webSocketURL(conn: Awaited<ReturnType<typeof openSocketConnection>>) {
	return `ws://localhost:${conn.port}/`;
}

test.describe('WebSocket', () => {
	test.describe.configure({
		retries: 3,
		timeout:
			HOW_LONG_TO_WAIT_FOR_STORAGE +
			HOW_LONG_TO_WAIT_FOR_WEBSOCKET +
			HOW_LONG_TO_WAIT_FOR_RESPONSE,
	});

	test('Should open a connection to emacs from the master tab', async ({
		extensionId,
		context,
	}) => {
		const conn = await openSocketConnection();
		const tabMaster = await context.newPage();

		async function websocketOpened(): Promise<boolean> {
			return new Promise(function (resolve) {
				tabMaster.on('websocket', (ws) => {
					if (ws.url() === webSocketURL(conn)) {
						resolve(true);
					}
				});
				void expect(tabMaster.getByTestId(INITIAL_STATE_LOCATOR))
					.toContainText(INITIAL_STATE_RESOLVED, {
						timeout: HOW_LONG_TO_WAIT_FOR_STORAGE,
					})
					.then(() => {
						setTimeout(
							() => resolve(false),
							HOW_LONG_TO_WAIT_FOR_WEBSOCKET
						);
					});
			});
		}

		const wsFunc = websocketOpened();

		await tabMaster.goto(`chrome-extension://${extensionId}/newtab.html`);
		await setupWebsocketPort(conn, tabMaster);
		await expect(tabMaster.getByTestId(ROLE_LOCATOR)).toContainText(
			MASTER_MESSAGE
		);
		expect(await wsFunc).toBeTruthy();
	});

	test('Should not open a connection to emacs from a client tab', async ({
		extensionId,
		context,
	}) => {
		const conn = await openSocketConnection();
		const tabMaster = await context.newPage();
		const tabClient = await context.newPage();
		async function clientWebsocketOpened(): Promise<boolean> {
			return new Promise(function (resolve) {
				tabClient.on('websocket', (ws) => {
					if (ws.url() === webSocketURL(conn)) {
						resolve(true);
					}
				});
				void expect(tabClient.getByTestId(INITIAL_STATE_LOCATOR))
					.toContainText(INITIAL_STATE_RESOLVED, {
						timeout: HOW_LONG_TO_WAIT_FOR_STORAGE,
					})
					.then(() => {
						setTimeout(
							() => resolve(false),
							HOW_LONG_TO_WAIT_FOR_WEBSOCKET
						);
					});
			});
		}

		const wsFunc = clientWebsocketOpened();

		await tabMaster.goto(`chrome-extension://${extensionId}/newtab.html`);
		await tabClient.goto(`chrome-extension://${extensionId}/newtab.html`);
		await setupWebsocketPort(conn, tabClient);
		await expect(tabMaster.getByLabel(WS_PORT_LABEL)).toHaveValue(
			conn.port.toString()
		);
		await expect(tabMaster.getByTestId(ROLE_LOCATOR)).toContainText(
			MASTER_MESSAGE
		);
		await expect(tabClient.getByTestId(ROLE_LOCATOR)).toContainText(
			CLIENT_MESSAGE
		);
		expect(await wsFunc).toBeFalsy();
	});

	test('Should ask for item after opening', async ({
		extensionId,
		context,
	}) => {
		const conn = await openSocketConnection();
		const tabMaster = await context.newPage();
		async function getItemMessageSent(): Promise<boolean> {
			return new Promise(function (resolve) {
				tabMaster.on('websocket', (ws) => {
					if (ws.url() === webSocketURL(conn)) {
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
				void expect(tabMaster.getByTestId(INITIAL_STATE_LOCATOR))
					.toContainText(INITIAL_STATE_RESOLVED, {
						timeout: HOW_LONG_TO_WAIT_FOR_STORAGE,
					})
					.then(() => {
						setTimeout(
							() => resolve(false),
							HOW_LONG_TO_WAIT_FOR_WEBSOCKET
						);
					});
			});
		}
		const wsFunc = getItemMessageSent();

		await tabMaster.goto(`chrome-extension://${extensionId}/newtab.html`);
		await setupWebsocketPort(conn, tabMaster);
		await expect(tabMaster.getByTestId(ROLE_LOCATOR)).toContainText(
			MASTER_MESSAGE
		);
		expect(await wsFunc).toBeTruthy();
	});

	test('Should update item text based on data from server', async ({
		extensionId,
		context,
	}) => {
		test.slow();
		const conn = await openSocketConnection();
		const tabMaster = await context.newPage();

		async function getAnyDataSent(): Promise<boolean> {
			return new Promise(function (resolve) {
				tabMaster.on('websocket', (ws) => {
					if (ws.url() === webSocketURL(conn)) {
						ws.on('framesent', () => {
							resolve(true);
						});
						ws.on('close', () => resolve(false));
						ws.on('socketerror', () => resolve(false));
					}
				});
				void expect(tabMaster.getByTestId(INITIAL_STATE_LOCATOR))
					.toContainText(INITIAL_STATE_RESOLVED, {
						timeout: HOW_LONG_TO_WAIT_FOR_STORAGE,
					})
					.then(() => {
						setTimeout(
							() => resolve(false),
							HOW_LONG_TO_WAIT_FOR_WEBSOCKET
						);
					});
			});
		}

		const wsFunc = getAnyDataSent();

		await tabMaster.goto(`chrome-extension://${extensionId}/newtab.html`);
		await setupWebsocketPort(conn, tabMaster);
		await expect(tabMaster.getByTestId(ROLE_LOCATOR)).toContainText(
			MASTER_MESSAGE
		);
		expect(await wsFunc).toBeTruthy();
		await expect(tabMaster.getByTestId(ITEM_TEXT_LOCATOR)).toContainText(
			WSS_TEST_TEXT,
			{ timeout: HOW_LONG_TO_WAIT_FOR_RESPONSE }
		);
	});

	test('Should send an update match query request from client through master', async ({
		extensionId,
		context,
	}) => {
		const conn = await openSocketConnection();
		const tabMaster = await context.newPage();
		const tabClient = await context.newPage();

		async function clientWebsocketOpened(): Promise<boolean> {
			return new Promise(function (resolve) {
				tabClient.on('websocket', (ws) => {
					if (ws.url() === webSocketURL(conn)) {
						resolve(true);
					}
				});
				void expect(tabClient.getByTestId(INITIAL_STATE_LOCATOR))
					.toContainText(INITIAL_STATE_RESOLVED, {
						timeout: HOW_LONG_TO_WAIT_FOR_STORAGE,
					})
					.then(() => {
						setTimeout(
							() => resolve(false),
							HOW_LONG_TO_WAIT_FOR_WEBSOCKET
						);
					});
			});
		}

		async function masterWebSocketUpdatesQuery(): Promise<boolean> {
			return new Promise(function (resolve) {
				tabMaster.on('websocket', (ws) => {
					if (ws.url() === webSocketURL(conn)) {
						ws.on('framesent', (event) => {
							if (typeof event.payload === 'string') {
								const payload = JSON.parse(event.payload) as {
									command?: string;
									data?: string;
								};
								if (
									payload?.command === GET_ITEM_COMMAND &&
									payload?.data === WSS_TEST_TEXT
								) {
									resolve(true);
								}
							}
						});
					}
				});
				void expect(tabMaster.getByTestId(INITIAL_STATE_LOCATOR))
					.toContainText(INITIAL_STATE_RESOLVED, {
						timeout: HOW_LONG_TO_WAIT_FOR_STORAGE,
					})
					.then(() => {
						setTimeout(
							() => resolve(false),
							HOW_LONG_TO_WAIT_FOR_WEBSOCKET
						);
					});
			});
		}

		const masterSocketUpdate = masterWebSocketUpdatesQuery();
		const clientSocketUpdate = clientWebsocketOpened();
		await tabMaster.goto(`chrome-extension://${extensionId}/newtab.html`);
		await tabClient.goto(`chrome-extension://${extensionId}/newtab.html`);
		await setupWebsocketPort(conn, tabClient);
		await expect(tabMaster.getByTestId(ROLE_LOCATOR)).toContainText(
			MASTER_MESSAGE
		);
		await expect(tabClient.getByTestId(ROLE_LOCATOR)).toContainText(
			CLIENT_MESSAGE
		);
		await tabClient.getByLabel(MATCH_QUERY_LABEL).fill(WSS_TEST_TEXT);
		await tabClient.getByLabel(MATCH_QUERY_LABEL).press('Enter');
		expect(await clientSocketUpdate).toBeFalsy();
		expect(await masterSocketUpdate).toBeTruthy();
	});

	test('Should sync websocket state between tabs', async ({
		extensionId,
		context,
	}) => {
		const conn = await openSocketConnection();
		const tabMaster = await context.newPage();
		const tabClient = await context.newPage();
		await tabMaster.goto(`chrome-extension://${extensionId}/newtab.html`);
		await setupWebsocketPort(conn, tabMaster);
		await tabClient.goto(`chrome-extension://${extensionId}/newtab.html`);
		await expect(tabMaster.getByTestId(ROLE_LOCATOR)).toContainText(
			MASTER_MESSAGE
		);
		await expect(tabClient.getByTestId(ROLE_LOCATOR)).toContainText(
			CLIENT_MESSAGE
		);
		await expect(
			tabMaster.getByTestId(CONNECTION_STATUS_LOCATOR)
		).toContainText(CONNECTION_STATUS_OPEN);
		await expect(
			tabClient.getByTestId(CONNECTION_STATUS_LOCATOR)
		).toContainText(CONNECTION_STATUS_OPEN);
	});

	test('Should sync match query between roles', async ({
		extensionId,
		context,
	}) => {
		const conn = await openSocketConnection();
		const tabMaster = await context.newPage();
		const tabClient = await context.newPage();
		await tabMaster.goto(`chrome-extension://${extensionId}/newtab.html`);
		await setupWebsocketPort(conn, tabMaster);
		await tabClient.goto(`chrome-extension://${extensionId}/newtab.html`);
		await expect(tabMaster.getByTestId(ROLE_LOCATOR)).toContainText(
			MASTER_MESSAGE
		);
		await expect(tabClient.getByTestId(ROLE_LOCATOR)).toContainText(
			CLIENT_MESSAGE
		);
		await expect(
			tabMaster.getByTestId(CONNECTION_STATUS_LOCATOR)
		).toContainText(CONNECTION_STATUS_OPEN);
		await tabClient.getByLabel(MATCH_QUERY_LABEL).fill(WSS_TEST_TEXT);
		await tabClient.getByLabel(MATCH_QUERY_LABEL).press('Enter');
		await expect(tabClient.getByLabel(MATCH_QUERY_LABEL)).toHaveValue(
			WSS_TEST_TEXT
		);
		await expect(tabMaster.getByLabel(MATCH_QUERY_LABEL)).toHaveValue(
			WSS_TEST_TEXT
		);
	});

	test('Should add and remove waiting responses', async ({
		extensionId,
		context,
	}) => {
		const conn = await openSocketConnection();
		const tabMaster = await context.newPage();

		await tabMaster.goto(`chrome-extension://${extensionId}/newtab.html`);
		await expect(
			tabMaster.getByTestId(LOADING_BAR_LOCATOR)
		).not.toBeVisible();
		await setupWebsocketPort(conn, tabMaster);
		await expect(tabMaster.getByTestId(ROLE_LOCATOR)).toContainText(
			MASTER_MESSAGE
		);
		await expect(
			tabMaster.getByTestId(CONNECTION_STATUS_LOCATOR)
		).toContainText(CONNECTION_STATUS_OPEN);
		await expect(tabMaster.getByTestId(LOADING_BAR_LOCATOR)).toBeVisible();
		await expect(tabMaster.getByTestId(ITEM_TEXT_LOCATOR)).toContainText(
			WSS_TEST_TEXT,
			{ timeout: HOW_LONG_TO_WAIT_FOR_RESPONSE }
		);
		await expect(
			tabMaster.getByTestId(LOADING_BAR_LOCATOR)
		).not.toBeVisible();
	});
});

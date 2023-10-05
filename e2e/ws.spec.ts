/* eslint-disable no-console */
import {
	CONNECTION_STATUS_LOCATOR,
	CONNECTION_STATUS_OPEN,
	GET_ITEM_COMMAND,
	HOW_LONG_TO_WAIT_FOR_RESPONSE,
	HOW_LONG_TO_WAIT_FOR_WEBSOCKET,
	ITEM_TEXT_LOCATOR,
	MATCH_QUERY_LABEL,
	WSS_TEST_TEXT,
	WS_PORT_LABEL,
	HOW_LONG_TO_WAIT_FOR_STORAGE,
	LOADING_BAR_LOCATOR,
	storageIsResolved,
	RETRIES_FOR_WEBSOCKET,
	closeOptions,
	gotoOptPanel,
	openSocketConnection,
	webSocketURL,
	setupWebsocketPort,
} from './common';
import { test, expect } from './fixture';

test.describe('WebSocket', () => {
	test.describe.configure({
		retries: RETRIES_FOR_WEBSOCKET,
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
		await tabMaster.goto(`chrome-extension://${extensionId}/newtab.html`);
		await storageIsResolved(tabMaster);

		async function websocketOpened(): Promise<boolean> {
			return new Promise(function (resolve) {
				tabMaster.on('websocket', (ws) => {
					if (ws.url() === webSocketURL(conn)) {
						resolve(true);
					}
				});
				setTimeout(
					() => resolve(false),
					HOW_LONG_TO_WAIT_FOR_WEBSOCKET
				);
			});
		}

		const wsFunc = websocketOpened();
		await setupWebsocketPort(conn, tabMaster);
		expect(await wsFunc).toBeTruthy();
	});

	test('Should not open a connection to emacs from a client tab', async ({
		extensionId,
		context,
	}) => {
		const conn = await openSocketConnection();
		const tabMaster = await context.newPage();
		const tabClient = await context.newPage();
		await tabMaster.goto(`chrome-extension://${extensionId}/newtab.html`);
		await tabClient.goto(`chrome-extension://${extensionId}/newtab.html`);
		await storageIsResolved(tabMaster);
		await storageIsResolved(tabClient);

		async function clientWebsocketOpened(): Promise<boolean> {
			return new Promise(function (resolve) {
				tabClient.on('websocket', (ws) => {
					if (ws.url() === webSocketURL(conn)) {
						resolve(true);
					}
				});
				setTimeout(
					() => resolve(false),
					HOW_LONG_TO_WAIT_FOR_WEBSOCKET
				);
			});
		}

		const wsFunc = clientWebsocketOpened();
		await setupWebsocketPort(conn, tabClient);
		await gotoOptPanel(tabMaster, 'Behavior');
		await expect(tabMaster.getByLabel(WS_PORT_LABEL)).toHaveValue(
			conn.port.toString()
		);
		expect(await wsFunc).toBeFalsy();
	});

	test('Should ask for item after opening', async ({
		extensionId,
		context,
	}) => {
		const conn = await openSocketConnection();
		const tabMaster = await context.newPage();
		await tabMaster.goto(`chrome-extension://${extensionId}/newtab.html`);
		await storageIsResolved(tabMaster);
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
				setTimeout(
					() => resolve(false),
					HOW_LONG_TO_WAIT_FOR_WEBSOCKET
				);
			});
		}

		const wsFunc = getItemMessageSent();
		await setupWebsocketPort(conn, tabMaster);
		expect(await wsFunc).toBeTruthy();
	});

	test('Should update item text based on data from server', async ({
		extensionId,
		context,
	}) => {
		const conn = await openSocketConnection();
		const tabMaster = await context.newPage();
		await tabMaster.goto(`chrome-extension://${extensionId}/newtab.html`);
		await storageIsResolved(tabMaster);

		async function getAnyDataRecv(): Promise<boolean> {
			return new Promise(function (resolve) {
				tabMaster.on('websocket', (ws) => {
					if (ws.url() === webSocketURL(conn)) {
						ws.on('framereceived', () => {
							console.log('Received data from server');
							resolve(true);
						});
						ws.on('close', () => resolve(false));
						ws.on('socketerror', () => resolve(false));
					}
				});
				setTimeout(
					() => resolve(false),
					HOW_LONG_TO_WAIT_FOR_WEBSOCKET
				);
			});
		}

		const wsFunc = getAnyDataRecv();
		await setupWebsocketPort(conn, tabMaster);
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
		await tabMaster.goto(`chrome-extension://${extensionId}/newtab.html`);
		await tabClient.goto(`chrome-extension://${extensionId}/newtab.html`);
		await storageIsResolved(tabMaster);
		await storageIsResolved(tabClient);

		async function clientWebsocketOpened(): Promise<boolean> {
			return new Promise(function (resolve) {
				tabClient.on('websocket', (ws) => {
					if (ws.url() === webSocketURL(conn)) {
						resolve(true);
					}
				});
				setTimeout(
					() => resolve(false),
					HOW_LONG_TO_WAIT_FOR_WEBSOCKET
				);
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
				setTimeout(
					() => resolve(false),
					HOW_LONG_TO_WAIT_FOR_WEBSOCKET
				);
			});
		}

		const masterSocketUpdate = masterWebSocketUpdatesQuery();
		const clientSocketUpdate = clientWebsocketOpened();
		await setupWebsocketPort(conn, tabClient);
		await gotoOptPanel(tabClient, 'Behavior');
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
		await tabClient.goto(`chrome-extension://${extensionId}/newtab.html`);
		await storageIsResolved(tabMaster);
		await storageIsResolved(tabClient);

		await setupWebsocketPort(conn, tabMaster);
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
		await tabClient.goto(`chrome-extension://${extensionId}/newtab.html`);
		await storageIsResolved(tabMaster);
		await storageIsResolved(tabClient);

		await setupWebsocketPort(conn, tabMaster);
		await expect(
			tabMaster.getByTestId(CONNECTION_STATUS_LOCATOR)
		).toContainText(CONNECTION_STATUS_OPEN);
		await gotoOptPanel(tabClient, 'Behavior');
		await tabClient.getByLabel(MATCH_QUERY_LABEL).fill(WSS_TEST_TEXT);
		await tabClient.getByLabel(MATCH_QUERY_LABEL).press('Enter');
		await closeOptions(tabClient);
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
		const loadingBar = tabMaster.getByTestId(LOADING_BAR_LOCATOR);
		const isLoadingBarVisible = tabMaster.waitForSelector(
			`div[data-testid="${LOADING_BAR_LOCATOR}"]`,
			{ state: 'visible' }
		);

		await storageIsResolved(tabMaster);
		await setupWebsocketPort(conn, tabMaster);

		expect(await isLoadingBarVisible).toBeTruthy();

		await expect(tabMaster.getByTestId(ITEM_TEXT_LOCATOR)).toContainText(
			WSS_TEST_TEXT,
			{ timeout: HOW_LONG_TO_WAIT_FOR_RESPONSE }
		);
		await expect(loadingBar).not.toBeVisible();
	});
});

/* eslint-disable no-console */
import { promises as fs } from 'fs';
import * as dns from 'node:dns';
import { test, expect } from './fixture';
import {
	changeTagsFileName,
	closeOptions,
	gotoOptPanel,
	isPortInUse,
	pickARandomPort,
	roleIs,
	setupWebsocketPort,
	startEmacsProcess,
	storageIsResolved,
	testFileName,
	toRGB,
} from './common';
import {
	AGENDA_ITEM_TEXT_NEXT,
	AGENDA_ITEM_TEXT_TAGGED,
	AGENDA_ITEM_TEXT_TODO,
	CONNECTION_STATUS_LOCATOR,
	CONNECTION_STATUS_OPEN,
	HOW_LONG_TO_TEST_CONNECTION_FOR,
	HOW_LONG_TO_WAIT_FOR_RESPONSE,
	HOW_LONG_TO_WAIT_FOR_STORAGE,
	HOW_LONG_TO_WAIT_FOR_WEBSOCKET,
	ITEM_TEXT_LOCATOR,
	MATCH_QUERY_LABEL,
	MATCH_QUERY_NESTED_TAG,
	MATCH_QUERY_NEXT,
	MATCH_QUERY_TAG,
	RETRIES_FOR_EMACS,
	TAG_COLOR,
	TAG_COLOR_NESTED_REGEX,
} from './constants';
import WebSocket from 'ws';

test.describe('Emacs', () => {
	test.describe.configure({
		retries: RETRIES_FOR_EMACS,
		timeout:
			HOW_LONG_TO_WAIT_FOR_STORAGE +
			HOW_LONG_TO_WAIT_FOR_WEBSOCKET +
			HOW_LONG_TO_WAIT_FOR_RESPONSE,
	});

	let port: number;
	let emacs: ReturnType<typeof startEmacsProcess>;

	test.beforeEach(async () => {
		port = await pickARandomPort();
		fs.unlink(testFileName(port)).catch(() => {});
		fs.unlink(changeTagsFileName(port)).catch(() => {});
		emacs = startEmacsProcess(port);
	});

	test.afterEach(() => {
		emacs.kill();
		fs.unlink(testFileName(port)).catch(() => {});
		fs.unlink(changeTagsFileName(port)).catch(() => {});
	});

	test('should connect to emacs', async ({ context, extensionId }) => {
		const tabMaster = await context.newPage();
		await tabMaster.goto(`chrome-extension://${extensionId}/newtab.html`);
		await storageIsResolved(tabMaster);
		await setupWebsocketPort({ port }, tabMaster);
		await expect(
			tabMaster.getByTestId(CONNECTION_STATUS_LOCATOR)
		).toContainText(CONNECTION_STATUS_OPEN);
	});

	test('should stay connected to emacs once connected', async ({
		context,
		extensionId,
	}) => {
		const tabMaster = await context.newPage();
		await tabMaster.goto(`chrome-extension://${extensionId}/newtab.html`);
		await storageIsResolved(tabMaster);
		await setupWebsocketPort({ port }, tabMaster);
		await tabMaster.waitForTimeout(HOW_LONG_TO_TEST_CONNECTION_FOR);
		await expect(
			tabMaster.getByTestId(CONNECTION_STATUS_LOCATOR)
		).toContainText(CONNECTION_STATUS_OPEN);
	});

	test('should send an agenda item which the master tab displays', async ({
		context,
		extensionId,
	}) => {
		const tabMaster = await context.newPage();
		await tabMaster.goto(`chrome-extension://${extensionId}/newtab.html`);
		await storageIsResolved(tabMaster);
		await setupWebsocketPort({ port }, tabMaster);

		await expect(tabMaster.getByTestId(ITEM_TEXT_LOCATOR)).toContainText(
			AGENDA_ITEM_TEXT_TODO,
			{ timeout: HOW_LONG_TO_WAIT_FOR_RESPONSE }
		);
	});

	test('should only open a single connection to emacs', async ({
		context,
		extensionId,
	}) => {
		const tabMaster = await context.newPage();
		const tabClient = await context.newPage();
		await tabMaster.goto(`chrome-extension://${extensionId}/newtab.html`);
		await tabClient.goto(`chrome-extension://${extensionId}/newtab.html`);
		await storageIsResolved(tabMaster);
		await storageIsResolved(tabClient);
		await setupWebsocketPort({ port }, tabClient);

		await roleIs(tabMaster, 'master');
		await roleIs(tabClient, 'client');

		await expect(tabMaster.getByTestId(ITEM_TEXT_LOCATOR)).toContainText(
			AGENDA_ITEM_TEXT_TODO,
			{ timeout: HOW_LONG_TO_WAIT_FOR_RESPONSE + 50000 }
		);
		await expect(tabClient.getByTestId(ITEM_TEXT_LOCATOR)).toContainText(
			AGENDA_ITEM_TEXT_TODO,
			{ timeout: HOW_LONG_TO_WAIT_FOR_RESPONSE }
		);

		await tabMaster.close();
		const newTabClient = await context.newPage();
		await newTabClient.goto(
			`chrome-extension://${extensionId}/newtab.html`
		);
		await storageIsResolved(newTabClient);

		await roleIs(tabClient, 'master');
		await roleIs(newTabClient, 'client');

		await expect(
			tabClient.getByTestId(CONNECTION_STATUS_LOCATOR)
		).toContainText(CONNECTION_STATUS_OPEN);
		await expect(
			newTabClient.getByTestId(CONNECTION_STATUS_LOCATOR)
		).toContainText(CONNECTION_STATUS_OPEN);
	});

	test('should send a new agenda item when given a new match query', async ({
		context,
		extensionId,
	}) => {
		const tabMaster = await context.newPage();
		await tabMaster.goto(`chrome-extension://${extensionId}/newtab.html`);
		await storageIsResolved(tabMaster);
		await setupWebsocketPort({ port }, tabMaster);

		await gotoOptPanel(tabMaster, 'Behavior');
		await tabMaster.getByLabel(MATCH_QUERY_LABEL).fill(MATCH_QUERY_NEXT);
		await tabMaster.getByLabel(MATCH_QUERY_LABEL).press('Enter');
		await closeOptions(tabMaster);

		await expect(tabMaster.getByTestId(ITEM_TEXT_LOCATOR)).toContainText(
			AGENDA_ITEM_TEXT_NEXT,
			{ timeout: HOW_LONG_TO_WAIT_FOR_RESPONSE }
		);
	});

	test('should send and use tag data', async ({ context, extensionId }) => {
		const tabMaster = await context.newPage();
		await tabMaster.goto(`chrome-extension://${extensionId}/newtab.html`);
		await storageIsResolved(tabMaster);
		await setupWebsocketPort({ port }, tabMaster);

		await gotoOptPanel(tabMaster, 'Behavior');
		await tabMaster.getByLabel(MATCH_QUERY_LABEL).fill(MATCH_QUERY_TAG);
		await tabMaster.getByLabel(MATCH_QUERY_LABEL).press('Enter');
		await closeOptions(tabMaster);
		await expect(tabMaster.getByTestId(ITEM_TEXT_LOCATOR)).toHaveCSS(
			'background-color',
			toRGB(TAG_COLOR),
			{ timeout: HOW_LONG_TO_WAIT_FOR_RESPONSE }
		);
	});

	test('should send multiple tags on nested items', async ({
		context,
		extensionId,
	}) => {
		const tabMaster = await context.newPage();
		await tabMaster.goto(`chrome-extension://${extensionId}/newtab.html`);
		await storageIsResolved(tabMaster);
		await setupWebsocketPort({ port }, tabMaster);

		await gotoOptPanel(tabMaster, 'Behavior');
		await tabMaster
			.getByLabel(MATCH_QUERY_LABEL)
			.fill(MATCH_QUERY_NESTED_TAG);
		await tabMaster.getByLabel(MATCH_QUERY_LABEL).press('Enter');
		await closeOptions(tabMaster);
		await expect(tabMaster.getByTestId(ITEM_TEXT_LOCATOR)).toHaveCSS(
			'background',
			TAG_COLOR_NESTED_REGEX,
			{ timeout: HOW_LONG_TO_WAIT_FOR_RESPONSE }
		);
	});

	test('should send along expected response ids', async ({
		context,
		extensionId,
	}) => {
		const tabMaster = await context.newPage();
		await tabMaster.goto(`chrome-extension://${extensionId}/newtab.html`);
		await storageIsResolved(tabMaster);
		await setupWebsocketPort({ port }, tabMaster);

		await gotoOptPanel(tabMaster, 'Behavior');
		await tabMaster.getByLabel(MATCH_QUERY_LABEL).fill(MATCH_QUERY_TAG);
		await tabMaster.getByLabel(MATCH_QUERY_LABEL).press('Enter');
		await closeOptions(tabMaster);

		await expect(tabMaster.getByTestId(ITEM_TEXT_LOCATOR)).toContainText(
			AGENDA_ITEM_TEXT_TAGGED,
			{ timeout: HOW_LONG_TO_WAIT_FOR_RESPONSE }
		);
		// Should not have any opacity other than 1 (ie not stale)
		await expect(tabMaster.getByTestId(ITEM_TEXT_LOCATOR)).not.toHaveCSS(
			'opacity',
			/[^1]/
		);
	});

	test('should degrade gracefully for future compatibility', async () => {
		let passed = false;
		while (!(await isPortInUse(port))) {
			await new Promise((resolve) => setTimeout(resolve, 100));
		}
		// Otherwise attempts to connect to ipv6 address
		dns.setDefaultResultOrder('ipv4first');
		const ws = new WebSocket(`ws://localhost:${port}/`, {
			perMessageDeflate: false,
		});
		ws.on('close', () => {
			console.log('Client disconnected');
		});
		ws.on('error', (e) => {
			console.error(e);
			test.fail();
		});
		ws.on('open', () => {
			ws.send(
				JSON.stringify({ command: 'getItem', data: 'test', resid: 1 })
			);
		});
		function sendNewMsg(id: number) {
			switch (id) {
				case 1:
					ws.send(
						JSON.stringify({
							command: 'newCommand',
							data: 'test',
							resid: 1,
						})
					);
					break;
				case 2:
					ws.send(
						JSON.stringify({ command: 'getItem', data: 'test' })
					);
					break;
				case 3:
					ws.send(
						JSON.stringify({
							command: 'getItem',
							data: 'test',
							extraProp: 'anything',
						})
					);
					break;
				case 4:
					ws.send(
						JSON.stringify({
							command: 'getItem',
						})
					);
					break;
				case 5:
					ws.send(
						JSON.stringify({
							command: 'getItem',
							data: 'TODO="TODO"',
							resid: 1,
						})
					);
					break;
			}
		}
		let id = 0;
		ws.on('message', (data: string) => {
			console.log('Received message from server: %s', data);
			sendNewMsg(++id);
			const json = JSON.parse(data) as Record<string, unknown>;
			if (
				json.type !== 'TAGS' &&
				(json?.data as Record<string, unknown>)?.ITEM ===
					AGENDA_ITEM_TEXT_TODO
			) {
				passed = true;
			}
		});

		while (!passed) {
			await new Promise((resolve) => setTimeout(resolve, 100));
		}
	});
});

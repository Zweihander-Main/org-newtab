/* eslint-disable no-console */
import { promises as fs } from 'fs';
import { test, expect } from './fixture';
import {
	baseDir,
	changeTagsFileName,
	closeOptions,
	gotoOptPanel,
	pickARandomPort,
	setupWebsocketPort,
	startEmacsProcess,
	storageIsResolved,
	testFileName,
} from './common';
import {
	AGENDA_ITEM_TEXT_CLOCKED,
	AGENDA_ITEM_TEXT_TAGGED,
	AGENDA_ITEM_TEXT_TODO,
	CLOCKED_TIME,
	CLOCKED_TIME_LOCATOR,
	EFFORTLESS_CLOCKED_TIME,
	HOW_LONG_TO_WAIT_FOR_RESPONSE,
	HOW_LONG_TO_WAIT_FOR_STORAGE,
	HOW_LONG_TO_WAIT_FOR_WEBSOCKET,
	ITEM_TEXT_LOCATOR,
	LOADING_BAR_LOCATOR,
	MATCH_QUERY_LABEL,
	RETRIES_FOR_EMACS,
} from './constants';

test.describe('Emacs hooks', () => {
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

	test('should send effort data for clocked in items', async ({
		context,
		extensionId,
	}) => {
		const tabMaster = await context.newPage();
		await tabMaster.goto(`chrome-extension://${extensionId}/newtab.html`);
		await storageIsResolved(tabMaster);
		await setupWebsocketPort({ port }, tabMaster);

		await fs.copyFile(
			`${baseDir}/e2e/emacs/clock-in.el`,
			testFileName(port)
		);

		await expect(tabMaster.getByTestId(ITEM_TEXT_LOCATOR)).toContainText(
			AGENDA_ITEM_TEXT_CLOCKED,
			{ timeout: HOW_LONG_TO_WAIT_FOR_RESPONSE }
		);

		await expect(tabMaster.getByTestId(CLOCKED_TIME_LOCATOR)).toContainText(
			CLOCKED_TIME
		);
	});

	test('should send match query after clock out', async ({
		context,
		extensionId,
	}) => {
		const tabMaster = await context.newPage();
		await tabMaster.goto(`chrome-extension://${extensionId}/newtab.html`);
		await storageIsResolved(tabMaster);
		await setupWebsocketPort({ port }, tabMaster);

		await fs.copyFile(
			`${baseDir}/e2e/emacs/clock-out.el`,
			testFileName(port)
		);

		await expect(tabMaster.getByTestId(ITEM_TEXT_LOCATOR)).toContainText(
			AGENDA_ITEM_TEXT_CLOCKED,
			{ timeout: HOW_LONG_TO_WAIT_FOR_RESPONSE }
		);

		await expect(tabMaster.getByTestId(ITEM_TEXT_LOCATOR)).toContainText(
			AGENDA_ITEM_TEXT_TODO,
			{ timeout: HOW_LONG_TO_WAIT_FOR_RESPONSE }
		);
	});

	test('should automatically send updates when agenda item changes', async ({
		context,
		extensionId,
	}) => {
		const tabMaster = await context.newPage();
		await tabMaster.goto(`chrome-extension://${extensionId}/newtab.html`);
		await storageIsResolved(tabMaster);
		await setupWebsocketPort({ port }, tabMaster);

		await expect(tabMaster.getByTestId(ITEM_TEXT_LOCATOR)).toBeVisible({
			timeout: HOW_LONG_TO_WAIT_FOR_RESPONSE,
		});

		await gotoOptPanel(tabMaster, 'Behavior');
		await tabMaster.getByLabel(MATCH_QUERY_LABEL).fill('2#NEWTAG');
		await tabMaster.getByLabel(MATCH_QUERY_LABEL).press('Enter');
		await closeOptions(tabMaster);

		await expect(tabMaster.getByTestId(ITEM_TEXT_LOCATOR)).not.toBeVisible({
			timeout: HOW_LONG_TO_WAIT_FOR_RESPONSE,
		});

		await fs.copyFile(
			`${baseDir}/e2e/emacs/change-tags.el`,
			testFileName(port)
		);

		await expect(tabMaster.getByTestId(ITEM_TEXT_LOCATOR)).toContainText(
			AGENDA_ITEM_TEXT_TAGGED,
			{ timeout: HOW_LONG_TO_WAIT_FOR_RESPONSE }
		);
	});

	test('should show clocked in minutes without effort set', async ({
		context,
		extensionId,
	}) => {
		const tabMaster = await context.newPage();
		await tabMaster.goto(`chrome-extension://${extensionId}/newtab.html`);
		await storageIsResolved(tabMaster);
		await setupWebsocketPort({ port }, tabMaster);

		await fs.copyFile(
			`${baseDir}/e2e/emacs/clock-broken.el`,
			testFileName(port)
		);

		await expect(tabMaster.getByTestId(ITEM_TEXT_LOCATOR)).toContainText(
			AGENDA_ITEM_TEXT_CLOCKED,
			{ timeout: HOW_LONG_TO_WAIT_FOR_RESPONSE }
		);

		await expect(tabMaster.getByTestId(CLOCKED_TIME_LOCATOR)).toContainText(
			EFFORTLESS_CLOCKED_TIME
		);
	});

	// TODO: flakiness? may need to catch the bar sooner, VERY rarely happens, 1/100 level
	test.only('should let the extension know when to expect a new item', async ({
		context,
		extensionId,
	}) => {
		const tabMaster = await context.newPage();
		await tabMaster.goto(`chrome-extension://${extensionId}/newtab.html`);

		await storageIsResolved(tabMaster);
		await setupWebsocketPort({ port }, tabMaster);

		await fs.copyFile(
			`${baseDir}/e2e/emacs/clock-out.el`,
			testFileName(port)
		);

		await expect(tabMaster.getByTestId(ITEM_TEXT_LOCATOR)).toContainText(
			AGENDA_ITEM_TEXT_CLOCKED,
			{ timeout: HOW_LONG_TO_WAIT_FOR_RESPONSE }
		);

		// .el file pauses for 5 seconds before clock out

		const isLoadingBarVisible = tabMaster.waitForSelector(
			`div[data-testid="${LOADING_BAR_LOCATOR}"]`,
			{ state: 'visible' }
		);

		expect(await isLoadingBarVisible).toBeTruthy();
	});
});

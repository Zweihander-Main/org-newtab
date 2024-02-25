/* eslint-disable no-console */
import { test, expect } from './fixture';
import {
	setupEmacs,
	setupClockLisp,
	setupOrgFile,
	setupWebsocketPort,
	startEmacsProcess,
	storageIsResolved,
	teardownEmacs,
} from './common';
import {
	AGENDA_ITEM_TEXT_CLOCKED,
	AGENDA_ITEM_TEXT_TODO,
	HOW_LONG_TO_WAIT_FOR_RESPONSE,
	HOW_LONG_TO_WAIT_FOR_STORAGE,
	HOW_LONG_TO_WAIT_FOR_WEBSOCKET,
	ITEM_TEXT_LOCATOR,
	LOADING_BAR_LOCATOR,
	RETRIES_FOR_EMACS,
} from './constants';

test.describe('Loading bars', () => {
	test.describe.configure({
		retries: RETRIES_FOR_EMACS + 2, // not a bug -- can miss loading bar due to fast speed
		timeout:
			HOW_LONG_TO_WAIT_FOR_STORAGE +
			HOW_LONG_TO_WAIT_FOR_WEBSOCKET +
			HOW_LONG_TO_WAIT_FOR_RESPONSE,
	});

	let port: number;
	let emacs: ReturnType<typeof startEmacsProcess>;
	let tmpDir: string;

	test.beforeEach(async () => {
		({ port, emacs, tmpDir } = await setupEmacs());
	});

	test.afterEach(() => {
		teardownEmacs(emacs);
	});

	test('should correspond to adding and removing waiting responses', async ({
		extensionId,
		context,
	}) => {
		await setupOrgFile('agenda.org', tmpDir);

		const tabMaster = await context.newPage();
		await tabMaster.goto(`chrome-extension://${extensionId}/newtab.html`);

		const loadingBar = tabMaster.getByTestId(LOADING_BAR_LOCATOR);
		const isLoadingBarVisible = tabMaster.waitForSelector(
			`div[data-testid="${LOADING_BAR_LOCATOR}"]`,
			{ state: 'visible' }
		);

		await storageIsResolved(tabMaster);
		await setupWebsocketPort({ port }, tabMaster);

		expect(await isLoadingBarVisible).toBeTruthy();

		await expect(tabMaster.getByTestId(ITEM_TEXT_LOCATOR)).toContainText(
			AGENDA_ITEM_TEXT_TODO,
			{ timeout: HOW_LONG_TO_WAIT_FOR_RESPONSE }
		);

		await expect(loadingBar).not.toBeVisible();
	});

	test('should be shown when expecting an unprompted response', async ({
		context,
		extensionId,
	}) => {
		await setupOrgFile('clock.org', tmpDir);

		const tabMaster = await context.newPage();
		await tabMaster.goto(`chrome-extension://${extensionId}/newtab.html`);

		await storageIsResolved(tabMaster);
		await setupWebsocketPort({ port }, tabMaster);

		await setupClockLisp('clock-out.el', tmpDir);

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

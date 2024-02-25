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
	setupChangeLisp,
	changeMatchQuery,
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
	let tmpDir: string;

	test.beforeEach(async () => {
		({ port, emacs, tmpDir } = await setupEmacs());
	});

	test.afterEach(() => {
		teardownEmacs(emacs);
	});

	test('should send effort data for clocked in items', async ({
		context,
		extensionId,
	}) => {
		await setupOrgFile('agenda.org', tmpDir);
		await setupOrgFile('clock.org', tmpDir);

		const tabMaster = await context.newPage();
		await tabMaster.goto(`chrome-extension://${extensionId}/newtab.html`);
		await storageIsResolved(tabMaster);
		await setupWebsocketPort({ port }, tabMaster);

		await setupClockLisp('clock-in.el', tmpDir);

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
		await setupOrgFile('agenda.org', tmpDir);
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

		await expect(tabMaster.getByTestId(ITEM_TEXT_LOCATOR)).toContainText(
			AGENDA_ITEM_TEXT_TODO,
			{ timeout: HOW_LONG_TO_WAIT_FOR_RESPONSE }
		);
	});

	test('should send match query after clock cancel', async ({
		context,
		extensionId,
	}) => {
		await setupOrgFile('agenda.org', tmpDir);
		await setupOrgFile('clock.org', tmpDir);

		const tabMaster = await context.newPage();
		await tabMaster.goto(`chrome-extension://${extensionId}/newtab.html`);
		await storageIsResolved(tabMaster);
		await setupWebsocketPort({ port }, tabMaster);

		await setupClockLisp('clock-cancel.el', tmpDir);

		await expect(tabMaster.getByTestId(ITEM_TEXT_LOCATOR)).toContainText(
			AGENDA_ITEM_TEXT_CLOCKED,
			{ timeout: HOW_LONG_TO_WAIT_FOR_RESPONSE }
		);

		await expect(tabMaster.getByTestId(ITEM_TEXT_LOCATOR)).toContainText(
			AGENDA_ITEM_TEXT_TODO,
			{ timeout: HOW_LONG_TO_WAIT_FOR_RESPONSE }
		);
	});

	test('should show clocked in minutes without effort set', async ({
		context,
		extensionId,
	}) => {
		await setupOrgFile('agenda.org', tmpDir);
		await setupOrgFile('clock-broken.org', tmpDir);

		const tabMaster = await context.newPage();
		await tabMaster.goto(`chrome-extension://${extensionId}/newtab.html`);
		await storageIsResolved(tabMaster);
		await setupWebsocketPort({ port }, tabMaster);

		await setupClockLisp('clock-broken.el', tmpDir);

		await expect(tabMaster.getByTestId(ITEM_TEXT_LOCATOR)).toContainText(
			AGENDA_ITEM_TEXT_CLOCKED,
			{ timeout: HOW_LONG_TO_WAIT_FOR_RESPONSE }
		);

		await expect(tabMaster.getByTestId(CLOCKED_TIME_LOCATOR)).toContainText(
			EFFORTLESS_CLOCKED_TIME
		);
	});

	test('should update effort data', async ({ context, extensionId }) => {
		await setupOrgFile('agenda.org', tmpDir);
		await setupOrgFile('clock.org', tmpDir);

		const tabMaster = await context.newPage();
		await tabMaster.goto(`chrome-extension://${extensionId}/newtab.html`);
		await storageIsResolved(tabMaster);
		await setupWebsocketPort({ port }, tabMaster);

		await setupClockLisp('clock-effort.el', tmpDir);

		await expect(tabMaster.getByTestId(ITEM_TEXT_LOCATOR)).toContainText(
			AGENDA_ITEM_TEXT_CLOCKED,
			{ timeout: HOW_LONG_TO_WAIT_FOR_RESPONSE }
		);

		await expect(tabMaster.getByTestId(CLOCKED_TIME_LOCATOR)).toContainText(
			CLOCKED_TIME
		);

		await expect(tabMaster.getByTestId(CLOCKED_TIME_LOCATOR)).toContainText(
			'0:01 / 2:34', // TODO const
			{ timeout: HOW_LONG_TO_WAIT_FOR_RESPONSE }
		);
	});

	test('should automatically send updates when tags change', async ({
		context,
		extensionId,
	}) => {
		await setupOrgFile('change.org', tmpDir, 'TAGSCHANGE');

		const tabMaster = await context.newPage();
		await tabMaster.goto(`chrome-extension://${extensionId}/newtab.html`);
		await storageIsResolved(tabMaster);
		await setupWebsocketPort({ port }, tabMaster);

		await expect(tabMaster.getByTestId(ITEM_TEXT_LOCATOR)).toBeVisible({
			timeout: HOW_LONG_TO_WAIT_FOR_RESPONSE,
		});

		await changeMatchQuery(tabMaster, 'NEWTAG');

		await expect(tabMaster.getByTestId(ITEM_TEXT_LOCATOR)).not.toBeVisible({
			timeout: HOW_LONG_TO_WAIT_FOR_RESPONSE,
		});

		await setupChangeLisp('change-tags.el', tmpDir);

		await expect(tabMaster.getByTestId(ITEM_TEXT_LOCATOR)).toContainText(
			AGENDA_ITEM_TEXT_TAGGED,
			{ timeout: HOW_LONG_TO_WAIT_FOR_RESPONSE }
		);
	});

	test('should send updates when todo state changes', async ({
		context,
		extensionId,
	}) => {
		await setupOrgFile('change.org', tmpDir);

		const tabMaster = await context.newPage();
		await tabMaster.goto(`chrome-extension://${extensionId}/newtab.html`);
		await storageIsResolved(tabMaster);
		await setupWebsocketPort({ port }, tabMaster);

		await changeMatchQuery(tabMaster, 'TODO="NEXT"');

		await expect(tabMaster.getByTestId(ITEM_TEXT_LOCATOR)).not.toBeVisible({
			timeout: HOW_LONG_TO_WAIT_FOR_RESPONSE,
		});

		await setupChangeLisp('change-state.el', tmpDir);

		await expect(tabMaster.getByTestId(ITEM_TEXT_LOCATOR)).toContainText(
			AGENDA_ITEM_TEXT_TAGGED,
			{ timeout: HOW_LONG_TO_WAIT_FOR_RESPONSE }
		);
	});

	test('should send updates as headline is edited', async ({
		context,
		extensionId,
	}) => {
		await setupOrgFile('change.org', tmpDir);

		const tabMaster = await context.newPage();
		await tabMaster.goto(`chrome-extension://${extensionId}/newtab.html`);
		await storageIsResolved(tabMaster);
		await setupWebsocketPort({ port }, tabMaster);

		await expect(tabMaster.getByTestId(ITEM_TEXT_LOCATOR)).toContainText(
			AGENDA_ITEM_TEXT_TODO,
			{
				timeout: HOW_LONG_TO_WAIT_FOR_RESPONSE,
			}
		);

		await setupChangeLisp('change-headline.el', tmpDir);

		await expect(tabMaster.getByTestId(ITEM_TEXT_LOCATOR)).toContainText(
			'Sample todo edited', // TODO: const
			{ timeout: HOW_LONG_TO_WAIT_FOR_RESPONSE }
		);
	});

	test('should send updates as priority is changed', async ({
		context,
		extensionId,
	}) => {
		await setupOrgFile('change.org', tmpDir);

		const tabMaster = await context.newPage();
		await tabMaster.goto(`chrome-extension://${extensionId}/newtab.html`);
		await storageIsResolved(tabMaster);
		await setupWebsocketPort({ port }, tabMaster);

		await changeMatchQuery(tabMaster, 'PRIORITY="B"');

		await expect(tabMaster.getByTestId(ITEM_TEXT_LOCATOR)).not.toBeVisible({
			timeout: HOW_LONG_TO_WAIT_FOR_RESPONSE,
		});

		await setupChangeLisp('change-priority.el', tmpDir);

		await expect(tabMaster.getByTestId(ITEM_TEXT_LOCATOR)).toContainText(
			AGENDA_ITEM_TEXT_TODO,
			{
				timeout: HOW_LONG_TO_WAIT_FOR_RESPONSE,
			}
		);
	});

	// TODO: Add test for refile change
});

/* eslint-disable no-console */
import { spawn } from 'child_process';
import { promises as fs } from 'fs';
import { test, expect } from './fixture';
import {
	AGENDA_ITEM_TEXT_CLOCKED,
	AGENDA_ITEM_TEXT_NEXT,
	AGENDA_ITEM_TEXT_TAGGED,
	AGENDA_ITEM_TEXT_TODO,
	CLOCKED_TIME,
	CLOCKED_TIME_LOCATOR,
	CONNECTION_STATUS_LOCATOR,
	CONNECTION_STATUS_OPEN,
	HOW_LONG_TO_TEST_CONNECTION_FOR,
	HOW_LONG_TO_WAIT_FOR_RESPONSE,
	HOW_LONG_TO_WAIT_FOR_STORAGE,
	HOW_LONG_TO_WAIT_FOR_WEBSOCKET,
	ITEM_TEXT_LOCATOR,
	MATCH_QUERY_LABEL,
	MATCH_QUERY_NEXT,
	MATCH_QUERY_TAG,
	MAX_RETRIES_FOR_EMACS_CONNECTION,
	RETRIES_FOR_EMACS,
	TAG_COLOR,
	closeOptions,
	gotoOptPanel,
	pickARandomPort,
	roleIs,
	setupWebsocketPort,
	storageIsResolved,
	toRGB,
} from './common';

const baseDir = process.cwd();
const extraTestCodeFile = `${baseDir}/e2e/emacs/extra-testing-code-`;

const testFileName = (port: number) =>
	`${extraTestCodeFile}${port.toString()}.el`;

function emacsProcess(port: number, retries = 0) {
	let emacs = spawn('emacs', [
		'--batch',
		'--quick',
		'--eval',
		`(setq org-newtab-ws-port ${port})`,
		'-l',
		`${baseDir}/e2e/emacs/init.el`,
		'-l',
		`${baseDir}/e2e/emacs/setup-mode.el`,
	]);

	/* prin1, princ, print to stdout
	    	message and error both to stderr :| */
	emacs.stdout.on('data', (data) => {
		console.log(`stdout: ${data}`);
	});

	emacs.stderr.on('data', (data) => {
		console.log(`stderr: ${data}`);
	});

	emacs.on('close', (code) => {
		console.log(`emacs running on port ${port} exited with code ${code}`);
		// 255 may occur due to file locks
		if (code === 255 && retries < MAX_RETRIES_FOR_EMACS_CONNECTION) {
			console.log('emacs exited with code 255, retrying');
			emacs = emacsProcess(port, retries + 1);
		}
	});

	return emacs;
}

test.describe('Emacs', () => {
	test.describe.configure({
		retries: RETRIES_FOR_EMACS,
		timeout:
			HOW_LONG_TO_WAIT_FOR_STORAGE +
			HOW_LONG_TO_WAIT_FOR_WEBSOCKET +
			HOW_LONG_TO_WAIT_FOR_RESPONSE,
	});

	let port: number;
	let emacs: ReturnType<typeof emacsProcess>;

	test.beforeEach(async () => {
		port = await pickARandomPort();
		fs.unlink(testFileName(port)).catch(() => {});
		emacs = emacsProcess(port);
	});

	test.afterEach(() => {
		emacs.kill();
		fs.unlink(testFileName(port)).catch(() => {});
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

		await expect(tabMaster.getByTestId(ITEM_TEXT_LOCATOR)).toContainText(
			AGENDA_ITEM_TEXT_TODO,
			{ timeout: HOW_LONG_TO_WAIT_FOR_RESPONSE }
		);
		await expect(tabClient.getByTestId(ITEM_TEXT_LOCATOR)).toContainText(
			AGENDA_ITEM_TEXT_TODO,
			{ timeout: HOW_LONG_TO_WAIT_FOR_RESPONSE }
		);

		await roleIs(tabMaster, 'master');
		await roleIs(tabClient, 'client');

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

	test('should send effort data for clocked items', async ({
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

	// test('should automatically send updates when agenda item changes', async ({
	// 	context,
	// 	extensionId,
	// }) => {
	// 	// TODO
	// });
});

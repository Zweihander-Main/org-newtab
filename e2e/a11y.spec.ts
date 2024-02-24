import {
	baseDir,
	changeFileFileName,
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
	AGENDA_ITEM_TEXT_TODO,
	HOW_LONG_TO_WAIT_FOR_RESPONSE,
	ITEM_TEXT_LOCATOR,
} from './constants';
import { test, expect } from './fixture';
import { checkA11y, injectAxe } from 'axe-playwright';
import { promises as fs } from 'fs';

const a11yOptionsFront = {
	detailedReport: true,
	axeOptions: {
		rules: {
			'page-has-heading-one': { enabled: false },
		},
	},
};

test('check accessibility on front page', async ({ page, extensionId }) => {
	await page.goto(`chrome-extension://${extensionId}/newtab.html`);
	await injectAxe(page);

	await checkA11y(page, undefined, a11yOptionsFront);
});

const a11yOptionsPanels = {
	detailedReport: true,
	axeOptions: {
		rules: {
			'aria-allowed-attr': { enabled: false },
			'aria-required-attr': { enabled: false },
		},
	},
};

test('check accessibility on options pages', async ({ page, extensionId }) => {
	await page.goto(`chrome-extension://${extensionId}/newtab.html`);
	await injectAxe(page);
	await gotoOptPanel(page, 'Behavior');
	await checkA11y(page, undefined, a11yOptionsPanels);
	await closeOptions(page);

	await gotoOptPanel(page, 'Layout');
	await checkA11y(page, undefined, a11yOptionsPanels);
	await closeOptions(page);

	await gotoOptPanel(page, 'Theming');
	await checkA11y(page, undefined, a11yOptionsPanels);
	await closeOptions(page);

	await gotoOptPanel(page, 'Debug');
	await checkA11y(page, undefined, a11yOptionsPanels);
	await closeOptions(page);
});

test('check accessibility on clocked and non-clocked item', async ({
	context,
	extensionId,
}) => {
	const port = await pickARandomPort();
	fs.unlink(testFileName(port)).catch(() => {});
	fs.unlink(changeFileFileName(port)).catch(() => {});
	const emacs = startEmacsProcess(port);

	const tabMaster = await context.newPage();
	await tabMaster.goto(`chrome-extension://${extensionId}/newtab.html`);
	await injectAxe(tabMaster);
	await storageIsResolved(tabMaster);
	await setupWebsocketPort({ port }, tabMaster);
	await expect(tabMaster.getByTestId(ITEM_TEXT_LOCATOR)).toContainText(
		AGENDA_ITEM_TEXT_TODO,
		{ timeout: HOW_LONG_TO_WAIT_FOR_RESPONSE }
	);

	await checkA11y(tabMaster, undefined, a11yOptionsFront);

	await fs.copyFile(`${baseDir}/e2e/emacs/clock-in.el`, testFileName(port));
	await expect(tabMaster.getByTestId(ITEM_TEXT_LOCATOR)).toContainText(
		AGENDA_ITEM_TEXT_CLOCKED,
		{ timeout: HOW_LONG_TO_WAIT_FOR_RESPONSE }
	);

	await checkA11y(tabMaster, undefined, a11yOptionsFront);

	emacs.kill();
	fs.unlink(testFileName(port)).catch(() => {});
	fs.unlink(changeFileFileName(port)).catch(() => {});
});

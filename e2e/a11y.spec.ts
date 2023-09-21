import { closeOptions, gotoOptPanel } from './common';
import { test } from './fixture';
import { checkA11y, injectAxe } from 'axe-playwright';

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

// TODO: test clocked in state

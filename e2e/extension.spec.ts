import { test, expect } from './fixture';

// let [backgroundPage] = browserContext.backgroundPages();
// if (!backgroundPage)
// 	backgroundPage = await browserContext.waitForEvent('backgroundpage');

test('newtab page', async ({ page, extensionId }) => {
	await page.goto(`chrome-extension://${extensionId}/newtab.html`);
	await expect(page.locator('.connection-status')).toHaveText(
		'Closed - Master'
	);
});

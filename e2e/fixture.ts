/* eslint-disable no-empty-pattern */
import {
	test as base,
	chromium,
	// firefox,
	type BrowserContext,
} from '@playwright/test';
import path from 'path';

export const test = base.extend<{
	headless: boolean;
	context: BrowserContext;
	extensionId: string;
}>({
	context: async ({ headless, browserName }, use) => {
		const pathToExtension = path.join(__dirname, '../build/chrome-mv3-dev');
		let context: BrowserContext;
		switch (browserName) {
			case 'chromium':
				context = await chromium.launchPersistentContext('', {
					headless: false,
					ignoreHTTPSErrors: true,
					args: [
						`--disable-extensions-except=${pathToExtension}`,
						`--load-extension=${pathToExtension}`,
						headless ? '--headless=new' : '',
					],
				});
				await use(context);
				await context.close();
				break;
			case 'firefox':
				// context = await firefox.launchPersistentContext('', {
				// 	headless: false,
				// 	ignoreHTTPSErrors: true,
				// 	acceptDownloads: true,
				// 	viewport: { width: 1920, height: 1080 },
				// 	args: [`--load-extension=${pathToExtension}`],
				// });
				break;
		}
	},
	extensionId: async ({ context }, use) => {
		// for manifest v3:
		let [background] = context.serviceWorkers();
		if (!background)
			background = await context.waitForEvent('serviceworker');

		const extensionId = background.url().split('/')[2];
		await use(extensionId);
	},
});
export const expect = test.expect;

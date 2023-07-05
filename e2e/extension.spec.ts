import { test, expect } from './fixture';

const MASTER_MESSAGE = 'Master';
const CLIENT_MESSAGE = 'Client';
const STATUS_LOCATOR = 'connection-status';

test('newtab page', async ({ page, extensionId }) => {
	await page.goto(`chrome-extension://${extensionId}/newtab.html`);
	await expect(page.getByTestId(STATUS_LOCATOR)).toContainText(
		MASTER_MESSAGE
	);
});

test('multiple tabs', async ({ extensionId, context }) => {
	const tab1 = await context.newPage();
	const tab2 = await context.newPage();
	await tab1.goto(`chrome-extension://${extensionId}/newtab.html`);
	await tab2.goto(`chrome-extension://${extensionId}/newtab.html`);
	await expect(tab1.getByTestId(STATUS_LOCATOR)).toContainText(
		MASTER_MESSAGE
	);
	await expect(tab2.getByTestId(STATUS_LOCATOR)).toContainText(
		CLIENT_MESSAGE
	);
});

test('tab query flow', async ({ extensionId, context }) => {
	const tab1 = await context.newPage();
	const tab2 = await context.newPage();
	const tab3 = await context.newPage();
	await tab1.goto(`chrome-extension://${extensionId}/newtab.html`);
	await tab2.goto(`chrome-extension://${extensionId}/newtab.html`);
	await tab3.goto(`chrome-extension://${extensionId}/newtab.html`);
	await expect(tab1.getByTestId(STATUS_LOCATOR)).toContainText(
		MASTER_MESSAGE
	);
	await expect(tab2.getByTestId(STATUS_LOCATOR)).toContainText(
		CLIENT_MESSAGE
	);
	await expect(tab3.getByTestId(STATUS_LOCATOR)).toContainText(
		CLIENT_MESSAGE
	);
	await tab1.close();
	await expect(tab2.getByTestId(STATUS_LOCATOR)).toContainText(
		MASTER_MESSAGE
	);
	await expect(tab3.getByTestId(STATUS_LOCATOR)).toContainText(
		CLIENT_MESSAGE
	);
	const tab4 = await context.newPage();
	await tab4.goto(`chrome-extension://${extensionId}/newtab.html`);
	await expect(tab4.getByTestId(STATUS_LOCATOR)).toContainText(
		CLIENT_MESSAGE
	);
	await tab2.reload();
	await expect(tab3.getByTestId(STATUS_LOCATOR)).toContainText(
		MASTER_MESSAGE
	);
	await expect(tab2.getByTestId(STATUS_LOCATOR)).toContainText(
		CLIENT_MESSAGE
	);
	await expect(tab4.getByTestId(STATUS_LOCATOR)).toContainText(
		CLIENT_MESSAGE
	);
	await tab2.close();
	await expect(tab3.getByTestId(STATUS_LOCATOR)).toContainText(
		MASTER_MESSAGE
	);
	await expect(tab4.getByTestId(STATUS_LOCATOR)).toContainText(
		CLIENT_MESSAGE
	);
	await tab3.close();
	await expect(tab4.getByTestId(STATUS_LOCATOR)).toContainText(
		MASTER_MESSAGE
	);
	await tab4.reload();
	await expect(tab4.getByTestId(STATUS_LOCATOR)).toContainText(
		MASTER_MESSAGE
	);
});

test('tab query flow stopping and starting', async ({
	extensionId,
	context,
}) => {
	const tab1 = await context.newPage();
	const tab2 = await context.newPage();
	const tab3 = await context.newPage();
	await tab1.goto(`chrome-extension://${extensionId}/newtab.html`);
	await tab2.goto(`chrome-extension://${extensionId}/newtab.html`);
	await tab3.goto(`chrome-extension://${extensionId}/newtab.html`);
	await expect(tab1.getByTestId(STATUS_LOCATOR)).toContainText(
		MASTER_MESSAGE
	);
	await expect(tab2.getByTestId(STATUS_LOCATOR)).toContainText(
		CLIENT_MESSAGE
	);
	await expect(tab3.getByTestId(STATUS_LOCATOR)).toContainText(
		CLIENT_MESSAGE
	);
	await tab1.close();
	await tab2.close();
	await tab3.close();
	const tab4 = await context.newPage();
	await tab4.goto(`chrome-extension://${extensionId}/newtab.html`);
	await expect(tab4.getByTestId(STATUS_LOCATOR)).toContainText(
		MASTER_MESSAGE
	);
	await tab4.close();
	const tab5 = await context.newPage();
	await tab5.goto(`chrome-extension://${extensionId}/newtab.html`);
	await expect(tab5.getByTestId(STATUS_LOCATOR)).toContainText(
		MASTER_MESSAGE
	);
	await tab5.close();
});

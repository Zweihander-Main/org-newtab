import {
	CLIENT_MESSAGE,
	MASTER_MESSAGE,
	MATCH_QUERY_LABEL,
	ROLE_LOCATOR,
	WSS_TEST_TEXT,
} from './common';
import { test, expect } from './fixture';

test('Should load a newtab page', async ({ page, extensionId }) => {
	await page.goto(`chrome-extension://${extensionId}/newtab.html`);
	await expect(page.getByTestId(ROLE_LOCATOR)).toContainText(MASTER_MESSAGE);
});

test('Should load multiple tabs with different roles', async ({
	extensionId,
	context,
}) => {
	const tab1 = await context.newPage();
	const tab2 = await context.newPage();
	await tab1.goto(`chrome-extension://${extensionId}/newtab.html`);
	await tab2.goto(`chrome-extension://${extensionId}/newtab.html`);
	await expect(tab1.getByTestId(ROLE_LOCATOR)).toContainText(MASTER_MESSAGE);
	await expect(tab2.getByTestId(ROLE_LOCATOR)).toContainText(CLIENT_MESSAGE);
});

test('Should load multiple tabs and maintain one master role', async ({
	extensionId,
	context,
}) => {
	const tab1 = await context.newPage();
	const tab2 = await context.newPage();
	const tab3 = await context.newPage();
	await tab1.goto(`chrome-extension://${extensionId}/newtab.html`);
	await tab2.goto(`chrome-extension://${extensionId}/newtab.html`);
	await tab3.goto(`chrome-extension://${extensionId}/newtab.html`);
	await expect(tab1.getByTestId(ROLE_LOCATOR)).toContainText(MASTER_MESSAGE);
	await expect(tab2.getByTestId(ROLE_LOCATOR)).toContainText(CLIENT_MESSAGE);
	await expect(tab3.getByTestId(ROLE_LOCATOR)).toContainText(CLIENT_MESSAGE);
	await tab1.close();
	await expect(tab2.getByTestId(ROLE_LOCATOR)).toContainText(MASTER_MESSAGE);
	await expect(tab3.getByTestId(ROLE_LOCATOR)).toContainText(CLIENT_MESSAGE);
	const tab4 = await context.newPage();
	await tab4.goto(`chrome-extension://${extensionId}/newtab.html`);
	await expect(tab4.getByTestId(ROLE_LOCATOR)).toContainText(CLIENT_MESSAGE);
	await tab2.reload();
	await expect(tab3.getByTestId(ROLE_LOCATOR)).toContainText(MASTER_MESSAGE);
	await expect(tab2.getByTestId(ROLE_LOCATOR)).toContainText(CLIENT_MESSAGE);
	await expect(tab4.getByTestId(ROLE_LOCATOR)).toContainText(CLIENT_MESSAGE);
	await tab2.close();
	await expect(tab3.getByTestId(ROLE_LOCATOR)).toContainText(MASTER_MESSAGE);
	await expect(tab4.getByTestId(ROLE_LOCATOR)).toContainText(CLIENT_MESSAGE);
	await tab3.close();
	await expect(tab4.getByTestId(ROLE_LOCATOR)).toContainText(MASTER_MESSAGE);
	await tab4.reload();
	await expect(tab4.getByTestId(ROLE_LOCATOR)).toContainText(MASTER_MESSAGE);
});

test('Should load multiple tabs and switch the master role between them as needed', async ({
	extensionId,
	context,
}) => {
	const tab1 = await context.newPage();
	const tab2 = await context.newPage();
	const tab3 = await context.newPage();
	await tab1.goto(`chrome-extension://${extensionId}/newtab.html`);
	await tab2.goto(`chrome-extension://${extensionId}/newtab.html`);
	await tab3.goto(`chrome-extension://${extensionId}/newtab.html`);
	await expect(tab1.getByTestId(ROLE_LOCATOR)).toContainText(MASTER_MESSAGE);
	await expect(tab2.getByTestId(ROLE_LOCATOR)).toContainText(CLIENT_MESSAGE);
	await expect(tab3.getByTestId(ROLE_LOCATOR)).toContainText(CLIENT_MESSAGE);
	await tab1.close();
	await tab2.close();
	await tab3.close();
	const tab4 = await context.newPage();
	await tab4.goto(`chrome-extension://${extensionId}/newtab.html`);
	await expect(tab4.getByTestId(ROLE_LOCATOR)).toContainText(MASTER_MESSAGE);
	await tab4.close();
	const tab5 = await context.newPage();
	await tab5.goto(`chrome-extension://${extensionId}/newtab.html`);
	await expect(tab5.getByTestId(ROLE_LOCATOR)).toContainText(MASTER_MESSAGE);
	await tab5.close();
});

test('Should sync match query between roles', async ({
	extensionId,
	context,
}) => {
	const tab1 = await context.newPage();
	const tab2 = await context.newPage();
	await tab1.goto(`chrome-extension://${extensionId}/newtab.html`);
	await tab2.goto(`chrome-extension://${extensionId}/newtab.html`);
	await expect(tab1.getByTestId(ROLE_LOCATOR)).toContainText(MASTER_MESSAGE);
	await expect(tab2.getByTestId(ROLE_LOCATOR)).toContainText(CLIENT_MESSAGE);
	await tab2.getByLabel(MATCH_QUERY_LABEL).fill(WSS_TEST_TEXT);
	await tab2.getByLabel(MATCH_QUERY_LABEL).press('Enter');
	await expect(tab1.getByLabel(MATCH_QUERY_LABEL)).toHaveValue(WSS_TEST_TEXT);
});

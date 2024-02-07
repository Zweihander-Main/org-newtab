import { roleIs } from './common';
import { test } from './fixture';

test('Should load a newtab page', async ({ page, extensionId }) => {
	await page.goto(`chrome-extension://${extensionId}/newtab.html`);
	await roleIs(page, 'master');
});

test('Should load multiple tabs with different roles', async ({
	extensionId,
	context,
}) => {
	const tab1 = await context.newPage();
	const tab2 = await context.newPage();
	await tab1.goto(`chrome-extension://${extensionId}/newtab.html`);
	await tab2.goto(`chrome-extension://${extensionId}/newtab.html`);
	await roleIs(tab1, 'master');
	await roleIs(tab2, 'client');
});

// TODO: flakiness between 4-24 repeats
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
	await roleIs(tab1, 'master');
	await roleIs(tab2, 'client');
	await roleIs(tab3, 'client');
	await tab1.close();
	await roleIs(tab2, 'master');
	await roleIs(tab3, 'client');
	const tab4 = await context.newPage();
	await tab4.goto(`chrome-extension://${extensionId}/newtab.html`);
	await roleIs(tab4, 'client');
	await tab2.reload();
	await roleIs(tab3, 'master');
	await roleIs(tab2, 'client');
	await roleIs(tab4, 'client');
	await tab2.close();
	await roleIs(tab3, 'master');
	await roleIs(tab4, 'client');
	await tab3.close();
	await roleIs(tab4, 'master');
	await tab4.reload();
	await roleIs(tab4, 'master');
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
	await roleIs(tab1, 'master');
	await roleIs(tab2, 'client');
	await roleIs(tab3, 'client');
	await tab1.close();
	await tab2.close();
	await tab3.close();
	const tab4 = await context.newPage();
	await tab4.goto(`chrome-extension://${extensionId}/newtab.html`);
	await roleIs(tab4, 'master');
	await tab4.close();
	const tab5 = await context.newPage();
	await tab5.goto(`chrome-extension://${extensionId}/newtab.html`);
	await roleIs(tab5, 'master');
	await tab5.close();
});

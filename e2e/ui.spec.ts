import {
	closeOptions,
	gotoOptPanel,
	roleIs,
	storageIsResolved,
} from './common';
import { CONNECTION_STATUS_LOCATOR } from './constants';
import { test, expect } from './fixture';

test('check layout updates sync between tabs', async ({
	context,
	extensionId,
}) => {
	const tabMaster = await context.newPage();
	const tabClient = await context.newPage();
	await tabMaster.goto(`chrome-extension://${extensionId}/newtab.html`);
	await tabClient.goto(`chrome-extension://${extensionId}/newtab.html`);
	await storageIsResolved(tabMaster);
	await storageIsResolved(tabClient);

	await roleIs(tabMaster, 'master');
	await roleIs(tabClient, 'client');

	const initialPositionMaster = await tabMaster
		.getByTestId(CONNECTION_STATUS_LOCATOR)
		.boundingBox();
	const initialPositionClient = await tabClient
		.getByTestId(CONNECTION_STATUS_LOCATOR)
		.boundingBox();

	await gotoOptPanel(tabClient, 'Layout');

	await tabClient
		.getByText('Connection')
		.dragTo(tabClient.getByText('Org Item'));

	await closeOptions(tabClient);

	const finalPositionClient = await tabClient
		.getByTestId(CONNECTION_STATUS_LOCATOR)
		.boundingBox();

	expect(finalPositionClient?.y).not.toEqual(initialPositionClient?.y);

	const finalPositionMaster = await tabMaster
		.getByTestId(CONNECTION_STATUS_LOCATOR)
		.boundingBox();

	expect(finalPositionMaster?.y).not.toEqual(initialPositionMaster?.y);
});

import { Page } from '@playwright/test';
import { test, expect } from './fixture';
import fs from 'fs';
import { OptionCategories } from 'modules/ui/uiSlice';
const locale = 'en';

type Message = {
	message: string;
	description: string;
};

function loadMessagesJson(locale: string): Record<string, Message> {
	const filePath = `locales/${locale}/messages.json`;
	const jsonData = fs.readFileSync(filePath, 'utf8');
	return JSON.parse(jsonData) as Record<string, Message>;
}

const getMessage = (id: string): string => {
	const messages = loadMessagesJson(locale);
	return (messages[id] && messages[id]?.message) || '';
};

// VSCode test environment is slower than CLI
export const QUICK_TIMEOUT = 50;
export const HOW_LONG_TO_WAIT_FOR_STORAGE = 20000;
export const HOW_LONG_TO_WAIT_FOR_WEBSOCKET = 500;
export const HOW_LONG_TO_WAIT_FOR_RESPONSE = 5000;
export const RETRIES_FOR_WEBSOCKET = 3;

export const MASTER_MESSAGE = getMessage('masterRole');
export const CLIENT_MESSAGE = getMessage('clientRole');
export const MATCH_QUERY_LABEL = getMessage('matchQuery');
export const WS_PORT_LABEL = getMessage('wsPort');
export const INITIAL_STATE_RESOLVED = getMessage('storageResolved');
export const CONNECTION_STATUS_OPEN = getMessage('connectionStatusOpen');

export const GET_ITEM_COMMAND = 'getItem';
export const WSS_TEST_TEXT = 'WSS test message';

export const ROLE_LOCATOR = 'websocket-role';
export const ITEM_TEXT_LOCATOR = 'item-text';
export const INITIAL_STATE_LOCATOR = 'initial-state';
export const CONNECTION_STATUS_LOCATOR = 'connection-status';
export const LOADING_BAR_LOCATOR = 'loading-bar';
export const OPTIONS_OPEN_BUTTON_LOCATOR = 'options-open-button';
export const OPTIONS_CLOSE_BUTTON_LOCATOR = 'options-close-button';
export const BEHAVIOR_BUTTON_LOCATOR = 'behavior-button';
export const LAYOUT_BUTTON_LOCATOR = 'layout-button';
export const THEMING_BUTTON_LOCATOR = 'theming-button';
export const DEBUG_BUTTON_LOCATOR = 'debug-button';

export const openOptions = async (page: Page) => {
	await test.step('Open menu', async () => {
		await page.emulateMedia({ reducedMotion: 'reduce' });
		const openButton = page.getByTestId(OPTIONS_OPEN_BUTTON_LOCATOR);
		await openButton.click();
	});
};

export const closeOptions = async (page: Page) => {
	await test.step('Close menu', async () => {
		await page.emulateMedia({ reducedMotion: 'reduce' });
		const closeButton = page.getByTestId(OPTIONS_CLOSE_BUTTON_LOCATOR);
		await closeButton.click();
	});
};

export const gotoOptPanel = async (page: Page, panel: OptionCategories) => {
	await test.step(`Go to ${panel} entry`, async () => {
		await openOptions(page);
		const button = page.getByTestId(`${panel.toLowerCase()}-button`);
		await button.click();
	});
};

export const roleIs = async (page: Page, role: 'master' | 'client') => {
	await test.step(`Check if the websocket role is ${role}`, async () => {
		await page.emulateMedia({ reducedMotion: 'reduce' });
		await gotoOptPanel(page, 'Debug');
		const roleLocator = page.getByTestId(ROLE_LOCATOR);
		await expect(roleLocator).toContainText(
			role === 'master' ? MASTER_MESSAGE : CLIENT_MESSAGE
		);
		await closeOptions(page);
	});
};

export const storageIsResolved = async (page: Page) => {
	await test.step('Check if the storage is resolved', async () => {
		await page.emulateMedia({ reducedMotion: 'reduce' });
		const initialStateLocator = page.getByTestId(INITIAL_STATE_LOCATOR);
		await gotoOptPanel(page, 'Debug');
		await expect(initialStateLocator).toContainText(
			INITIAL_STATE_RESOLVED,
			{ timeout: HOW_LONG_TO_WAIT_FOR_STORAGE }
		);
		await closeOptions(page);
	});
};

import fs from 'fs';
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
export const HOW_LONG_TO_WAIT_FOR_STORAGE = 20000;
export const HOW_LONG_TO_WAIT_FOR_WEBSOCKET = 500;
export const HOW_LONG_TO_WAIT_FOR_RESPONSE = 5000;

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

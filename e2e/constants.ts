import fs from 'fs';

/**
 *  Messages.json loading
 */

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
	const messages = loadMessagesJson(LOCALE);
	return (messages[id] && messages[id]?.message) || '';
};

/**
 * Constants
 */

export const HOW_LONG_TO_WAIT_FOR_STORAGE = 20000;
export const HOW_LONG_TO_WAIT_FOR_WEBSOCKET = 15000;
export const HOW_LONG_TO_WAIT_FOR_RESPONSE = 20000;
export const HOW_LONG_TO_TEST_CONNECTION_FOR = 5000;
export const RETRIES_FOR_WEBSOCKET = 0;
export const RETRIES_FOR_EMACS = 0;
export const MAX_RETRIES_FOR_EMACS_CONNECTION = 3;

export const LOCALE = 'en';

export const MASTER_MESSAGE = getMessage('masterRole');
export const CLIENT_MESSAGE = getMessage('clientRole');
export const MATCH_QUERY_LABEL = getMessage('matchQuery');
export const WS_PORT_LABEL = getMessage('wsPort');
export const INITIAL_STATE_RESOLVED = getMessage('storageResolved');
export const CONNECTION_STATUS_OPEN = getMessage('connectionStatusOpen');

export const GET_ITEM_COMMAND = 'getItem';
export const WSS_TEST_TEXT = 'WSS test message';
export const AGENDA_ITEM_TEXT_TODO = 'Sample todo item';
export const AGENDA_ITEM_TEXT_NEXT = 'Sample next item';
export const AGENDA_ITEM_TEXT_TAGGED = 'Sample tagged item';
export const AGENDA_ITEM_TEXT_CLOCKED = 'Sample clocked item';
export const AGENDA_ITEM_TEXT_EDITED = 'Sample todo edited';
export const CLOCKED_TIME = '0:01 / 1:23';
export const CLOCKED_TIME_CHANGED = '0:01 / 2:34';
export const EFFORTLESS_CLOCKED_TIME = '0:00';
export const MATCH_QUERY_NEXT = 'TODO="NEXT"';
export const MATCH_QUERY_TAG = '1#SAMPLETAG';
export const MATCH_QUERY_NESTED_TAG = '2#OTHERTAG';
export const MATCH_QUERY_CHANGED_TAG = 'NEWTAG';
export const MATCH_QUERY_PRIORITY_B = 'PRIORITY="B"';
export const TAG_COLOR = '#42A5F5';
export const TAG_COLOR_NESTED_REGEX =
	/linear-gradient\(.*, rgb\(0, 255, 51\), rgb\(106, 59, 159\)\)/;

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
export const CLOCKED_TIME_LOCATOR = 'clocked-time';

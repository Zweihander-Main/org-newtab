// TODO -- generalize through app, intl, etc.
export const MASTER_MESSAGE = 'Master';
export const CLIENT_MESSAGE = 'Client';
export const ROLE_LOCATOR = 'websocket-role';
export const ITEM_TEXT_LOCATOR = 'item-text';
export const CONNECTION_STATUS_LOCATOR = 'connection-status';
export const WSS_TEST_TEXT = 'WSS test message';
export const MATCH_QUERY_LABEL = 'Match Query';
export const CONNECTION_STATUS_OPEN = 'Connected';

export const HOW_LONG_TO_WAIT_FOR_WEBSOCKET = 500;
export const HOW_LONG_TO_WAIT_FOR_RESPONSE = 5000;

export const WEBSOCKET_PORT = 35942;
export const WEBSOCKET_URL = `ws://localhost:${WEBSOCKET_PORT}/`;

export const GET_ITEM_COMMAND = 'getItem';
export const UPDATE_MATCH_QUERY_COMMAND = 'updateMatchQuery';

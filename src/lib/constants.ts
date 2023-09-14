import { LogLoc } from './types';

/** Default WebSocket port as also defined on the Elisp side */
export const DEFAULT_WEBSOCKET_PORT = 35942;

/** For testing, how to wait for response from dummy WebSocket server */
export const MAXIMUM_TIME_TO_WAIT_FOR_RESPONSE = 20000;

/** How long to increase wait time between WebSocket reconnection attempts */
export const RECONNECTION_ATTEMPT_GROWTH_FACTOR = 1.5;

/** Percentage of expected effort for when to start warning time spent is getting near */
export const TIME_WARNING_THRESHOLD = 0.8;

/** Enable BGSW related logging and direction for logs */
export const ENABLE_LOGGING: LogLoc = LogLoc.NONE;

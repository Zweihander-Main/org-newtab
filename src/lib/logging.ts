/* eslint-disable no-console */

import { ENABLE_BGSW_LOGGING } from './constants';
import { LogLoc, LogMsgDir } from './types';

export const log = (loc: LogLoc, ...args: Parameters<typeof console.log>) => {
	if (ENABLE_BGSW_LOGGING === loc) {
		console.log(`[${loc}]`, ...args);
	}
};

export const logMsg = (
	loc: LogLoc,
	dir: LogMsgDir,
	...args: Parameters<typeof console.log>
) => {
	if (ENABLE_BGSW_LOGGING === loc) {
		log(loc, dir, ...args);
	}
};

export const logMsgErr = (
	loc: LogLoc,
	dir: LogMsgDir,
	...args: Parameters<typeof console.error>
) => {
	if (ENABLE_BGSW_LOGGING === loc) {
		console.error(`[${loc}]`, dir, ...args);
	}
};

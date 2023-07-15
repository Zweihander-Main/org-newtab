/* eslint-disable no-console */

const ENABLE_LOGGING = true;

export enum LogLoc {
	BGSW = 'BGSW',
	NEWTAB = 'NewTab',
}

export enum LogMsgDir {
	SEND = '=>',
	RECV = '<=',
}

export const log = (loc: LogLoc, ...args: Parameters<typeof console.log>) => {
	if (ENABLE_LOGGING) {
		console.log(`[${loc}]`, ...args);
	}
};

export const logMsg = (
	loc: LogLoc,
	dir: LogMsgDir,
	...args: Parameters<typeof console.log>
) => {
	if (ENABLE_LOGGING) {
		log(loc, dir, ...args);
	}
};

export const logMsgErr = (
	loc: LogLoc,
	dir: LogMsgDir,
	...args: Parameters<typeof console.error>
) => {
	if (ENABLE_LOGGING) {
		console.error(`[${loc}]`, dir, ...args);
	}
};

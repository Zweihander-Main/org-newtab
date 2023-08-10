/* eslint-disable no-console */

export enum LogLoc {
	BGSW = 'BGSW',
	NEWTAB = 'NewTab',
	NONE = 'NONE',
}

export enum LogMsgDir {
	SEND = '=>',
	RECV = '<=',
}

const ENABLE_LOGGING: LogLoc = LogLoc.NONE;

export const log = (loc: LogLoc, ...args: Parameters<typeof console.log>) => {
	if (ENABLE_LOGGING === loc) {
		console.log(`[${loc}]`, ...args);
	}
};

export const logMsg = (
	loc: LogLoc,
	dir: LogMsgDir,
	...args: Parameters<typeof console.log>
) => {
	if (ENABLE_LOGGING === loc) {
		log(loc, dir, ...args);
	}
};

export const logMsgErr = (
	loc: LogLoc,
	dir: LogMsgDir,
	...args: Parameters<typeof console.error>
) => {
	if (ENABLE_LOGGING === loc) {
		console.error(`[${loc}]`, dir, ...args);
	}
};

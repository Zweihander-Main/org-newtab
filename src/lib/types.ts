/*
 * Message logging
 */
export enum LogLoc {
	BGSW = 'BGSW',
	NEWTAB = 'NewTab',
	NONE = 'NONE',
}

export enum LogMsgDir {
	SEND = '=>',
	RECV = '<=',
}

/**
 * BGSW messages
 */

export enum MsgToBGSWType {
	QUERY_WS_ROLE = 1,
	IDENTIFY_ROLE_MASTER = 2,
	IDENTIFY_ROLE_CLIENT = 3,
	CONFIRMING_ALIVE = 4,
}

export const getMsgToBGSWType = (type: MsgToBGSWType) => {
	return MsgToBGSWType[type];
};

export enum MsgToTabType {
	CONFIRM_YOUR_ROLE_IS_MASTER = 1,
	SET_ROLE_MASTER = 2,
	SET_ROLE_CLIENT = 3,
	QUERY_ALIVE = 4,
	PASS_TO_EMACS = 11,
	QUERY_WS_STATE = 12,
	SET_WS_STATE = 13,
	SET_WS_PORT = 14,
}

export const getMsgToTabType = (type: MsgToTabType) => {
	return MsgToTabType[type];
};

export enum MsgDirection {
	TO_BGSW = 1,
	TO_NEWTAB = 2,
}

export type MsgToBGSW = {
	direction: MsgDirection.TO_BGSW;
	type: MsgToBGSWType;
};

/**
 * Inter-tab messages
 */

export type MsgToTabData = EmacsSendMsg | WSStateMsg | WSPortMsg;

export type MsgToTab = {
	direction: MsgDirection.TO_NEWTAB;
	type: MsgToTabType;
	data?: MsgToTabData;
};

export enum WSReadyState {
	UNINSTANTIATED = -1,
	CONNECTING = 0,
	OPEN = 1,
	CLOSING = 2,
	CLOSED = 3,
}

export interface ResponseData {
	id: number;
	type: EmacsMsgTypes | '';
}

export type WSStateMsg = {
	readyState?: WSReadyState;
	responsesWaitingFor?: Array<ResponseData>;
};

export type WSPortMsg = {
	port?: number;
};

/**
 * Messages from Emacs
 */

export type AllTagsRecv = string | Array<string | number>;

export type EmacsItemType = 'ITEM';
export type EmacsItemMsg = {
	type: EmacsItemType;
	data: {
		ITEM: string;
		ALLTAGS?: AllTagsRecv;
		CATEGORY?: string;
		LAST_REPEAT?: string;
		EFFORT?: string;
		TIMESTAMP_IA?: string;
		SCHEDULED?: string;
		DEADLINE?: string;
		FILE?: string;
		PRIORITY?: string;
		TODO?: string;
		EFFORT_MINUTES?: number;
		PREVIOUSLY_CLOCKED_MINUTES?: number;
		CURRENT_CLOCK_START_TIMESTAMP?: number;
	};
	resid?: number; // Clocked in won't have one
};

export type EmacsTagsType = 'TAGS';
export type EmacsTagsMsg = {
	type: EmacsTagsType;
	data: {
		[key: string]: string | null;
	} | null;
};

// Async task started based on Emacs hook, start loading bar
export type EmacsFindingType = 'FINDING';
export type EmacsFindingMsg = {
	type: EmacsFindingType;
	resid: number;
};

export type EmacsMsgTypes = EmacsItemType | EmacsTagsType | EmacsFindingType;

export type EmacsRecvMsg = EmacsItemMsg | EmacsTagsMsg | EmacsFindingMsg | null;

export const isItemMsg = (msg: EmacsRecvMsg): msg is EmacsItemMsg => {
	return msg?.type === 'ITEM';
};

export const isFindingMsg = (msg: EmacsRecvMsg): msg is EmacsFindingMsg => {
	return msg?.type === 'FINDING';
};

/**
 * Messages to Emacs
 */
export type EmacsGetItemCommand = 'getItem';
export type EmacsCommands = EmacsGetItemCommand;

export const commandToTypeMapping: Record<EmacsCommands, EmacsMsgTypes> = {
	getItem: 'ITEM',
};
export const getTypeFromCommand = (command: EmacsCommands): EmacsMsgTypes => {
	return commandToTypeMapping[command];
};

export type EmacsSendMsg = {
	command: EmacsCommands;
	data: string;
};

export type EmacsSendMsgWithResid = EmacsSendMsg & {
	resid: number;
};

/** Utility types */
export type Entries<T> = {
	[K in keyof T]: [K, T[K]];
}[keyof T][];

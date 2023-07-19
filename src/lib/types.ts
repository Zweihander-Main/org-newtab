export enum MsgToBGSWType {
	QUERY_STATUS_OF_WS = 1,
	IDENTIFY_AS_MASTER_WS = 2,
	IDENTIFY_AS_WS_CLIENT = 3,
	CONFIRMED_ALIVE = 4,
}

export const getMsgToBGSWType = (type: MsgToBGSWType) => {
	return MsgToBGSWType[type];
};

export enum MsgToTabType {
	CONFIRM_IF_MASTER_WS = 1,
	YOU_ARE_MASTER_WS = 2,
	YOU_ARE_CLIENT_WS = 3,
	CONFIRM_IF_ALIVE = 4,
	PASS_ON_TO_EMACS = 5,
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

export type MsgToTab = {
	direction: MsgDirection.TO_NEWTAB;
	type: MsgToTabType;
	data?: EmacsSendMsg;
};

export type AllTagsRecv = string | Array<string | number>;

export type EmacsItemMsg = {
	type: 'ITEM';
	data: {
		ITEM: string;
		ALLTAGS?: AllTagsRecv;
	};
};

export type EmacsTagsMsg = {
	type: 'TAGS';
	data: {
		[key: string]: string | null;
	};
};

export type EmacsRecvMsg = EmacsItemMsg | EmacsTagsMsg | null;

export type EmacsSendMsg = {
	command: 'updateMatchQuery' | 'getItem';
	data: string;
	resid: number;
};

export type sendJsonMessage = (
	jsonMessage: EmacsSendMsg,
	keep?: boolean
) => void;

export type WSCommonProps = {
	sendJsonMessage: sendJsonMessage;
	lastRecvJsonMessage: EmacsRecvMsg;
	amMasterWS: boolean;
};

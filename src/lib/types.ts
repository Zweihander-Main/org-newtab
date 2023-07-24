import { ReadyState } from 'react-use-websocket';

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
	data?: EmacsSendMsg | WSStateMsg;
};

export type AllTagsRecv = string | Array<string | number>;

export type EmacsItemMsg = {
	type: 'ITEM';
	data: {
		ITEM: string;
		ALLTAGS?: AllTagsRecv;
	};
	resid?: number;
};

export type EmacsTagsMsg = {
	type: 'TAGS';
	data: {
		[key: string]: string | null; // TODO: sure about this?
	};
};

export type EmacsRecvMsg = EmacsItemMsg | EmacsTagsMsg | null;

export type WSStateMsg = {
	readyState?: ReadyState;
	responsesWaitingFor?: Array<number>;
};

export type EmacsSendMsg = {
	command: 'updateMatchQuery' | 'getItem';
	data: string;
};

export type EmacsSendMsgWithResid = EmacsSendMsg & {
	resid: number;
};

export type sendJsonMessage = (
	jsonMessage: EmacsSendMsg,
	keep?: boolean
) => void;

export type WSCommonProps = {
	sendJsonMessage: sendJsonMessage;
};

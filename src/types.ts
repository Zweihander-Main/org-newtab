import type { SendJsonMessage } from 'react-use-websocket/dist/lib/types';
import type { ReadyState } from 'react-use-websocket';

export enum MsgNewTabToBGSWType {
	QUERY_STATUS_OF_WS = 1,
	IDENTIFY_AS_MASTER_WS = 2,
	IDENTIFY_AS_WS_CLIENT = 3,
	CONFIRMED_ALIVE = 4,
}

export const getMsgNewTabToBGSWType = (type: MsgNewTabToBGSWType) => {
	return MsgNewTabToBGSWType[type];
};

export enum MsgBGSWToNewTabType {
	CONFIRM_IF_MASTER_WS = 1,
	YOU_ARE_MASTER_WS = 2,
	YOU_ARE_CLIENT_WS = 3,
	CONFIRM_IF_ALIVE = 4,
}
export const getMsgBGSWToNewType = (type: MsgBGSWToNewTabType) => {
	return MsgBGSWToNewTabType[type];
};

export enum MsgDirection {
	TO_BGSW = 1,
	TO_NEWTAB = 2,
}

export type MsgNewTabToBGSW = {
	direction: MsgDirection.TO_BGSW;
	type: MsgNewTabToBGSWType;
};

export type MsgBGSWToNewTab = {
	direction: MsgDirection.TO_NEWTAB;
	type: MsgBGSWToNewTabType;
};

export type AllTagsRecv = string | Array<string | number>;

export type WebSocketItemMessage = {
	type: 'ITEM';
	data: {
		ITEM: string;
		ALLTAGS?: AllTagsRecv;
	};
};

export type WebSocketTagsMessage = {
	type: 'TAGS';
	data: {
		[key: string]: string;
	};
};

export type WebSocketRecvMessage =
	| WebSocketItemMessage
	| WebSocketTagsMessage
	| null;

export type WSCommonProps = {
	sendJsonMessage: SendJsonMessage;
	lastRecvJsonMessage: WebSocketRecvMessage;
	readyState: ReadyState;
};

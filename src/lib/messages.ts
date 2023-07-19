import { LogLoc, LogMsgDir, logMsg } from './logging';
import {
	MsgDirection,
	type MsgToTab,
	MsgToTabType,
	MsgToBGSWType,
	getMsgToBGSWType,
	getMsgToTabType,
	MsgToBGSW,
	EmacsSendMsg,
} from './types';

export type SendResponseType = (message: MsgToBGSW) => unknown;

export const sendMsgToBGSWPort = (
	type: MsgToBGSWType,
	port: chrome.runtime.Port
) => {
	logMsg(
		LogLoc.NEWTAB,
		LogMsgDir.SEND,
		'Sending message to BGSW port',
		getMsgToBGSWType(type)
	);
	port.postMessage({
		type,
		direction: MsgDirection.TO_BGSW,
	});
};

export const sendMsgAsResponse = (
	type: MsgToBGSWType,
	sendResponse: SendResponseType
) => {
	logMsg(
		LogLoc.NEWTAB,
		LogMsgDir.SEND,
		'Sending response to BGSW msg',
		getMsgToBGSWType(type)
	);
	sendResponse({
		type,
		direction: MsgDirection.TO_BGSW,
	});
};

export const sendMsgToTab = (
	type: MsgToTabType,
	tabId: number,
	data?: EmacsSendMsg
) => {
	logMsg(
		LogLoc.NEWTAB,
		LogMsgDir.SEND,
		'Sending request to master tab',
		tabId,
		getMsgToTabType(type),
		data ? data : ''
	);
	void chrome.tabs.sendMessage<MsgToTab>(tabId, {
		direction: MsgDirection.TO_NEWTAB,
		type,
		data,
	});
};

export const handleMasterQueryConfirmation = (
	sendResponse: SendResponseType,
	amMasterWS: boolean
) => {
	if (amMasterWS) {
		sendMsgAsResponse(MsgToBGSWType.IDENTIFY_AS_MASTER_WS, sendResponse);
	} else {
		sendMsgAsResponse(MsgToBGSWType.IDENTIFY_AS_WS_CLIENT, sendResponse);
	}
};

export const handleConfirmingAlive = (sendResponse: SendResponseType) => {
	sendMsgAsResponse(MsgToBGSWType.CONFIRMED_ALIVE, sendResponse);
};

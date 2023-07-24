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
	WSStateMsg,
} from './types';

export type SendResponseType = (message: MsgToBGSW | MsgToTab) => unknown;

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

export const sendMsgToBGSWAsResponse = (
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
	data?: EmacsSendMsg | WSStateMsg
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

export const sendMsgToAllTabs = (type: MsgToTabType, data?: WSStateMsg) => {
	logMsg(
		LogLoc.NEWTAB,
		LogMsgDir.SEND,
		'Sending update to all tabs',
		getMsgToTabType(type),
		data ? data : ''
	);
	void chrome.runtime.sendMessage<MsgToTab>({
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
		sendMsgToBGSWAsResponse(
			MsgToBGSWType.IDENTIFY_ROLE_MASTER,
			sendResponse
		);
	} else {
		sendMsgToBGSWAsResponse(
			MsgToBGSWType.IDENTIFY_ROLE_CLIENT,
			sendResponse
		);
	}
};

export const handleConfirmingAlive = (sendResponse: SendResponseType) => {
	sendMsgToBGSWAsResponse(MsgToBGSWType.CONFIRMING_ALIVE, sendResponse);
};

export const sendUpdateInWSState = (data: WSStateMsg) => {
	sendMsgToAllTabs(MsgToTabType.SET_WS_STATE, data);
};

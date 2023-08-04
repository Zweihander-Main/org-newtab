import { LogLoc, LogMsgDir, logMsg, logMsgErr } from './logging';
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
	SendJsonMessage,
	WSPortMsg,
} from './types';

export type SendResponseType = (message: MsgToBGSW | MsgToTab) => unknown;

/**
 * General messaging functions
 */

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
	data?: EmacsSendMsg | WSStateMsg | WSPortMsg
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

/**
 * BGSW related messaging functions
 */

export const handleConfirmingRoleAsMaster = (
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

export const getMasterWSTabId = async () => {
	const masterWSObject = await chrome.storage.local.get('masterWSTabId');
	const { masterWSTabId } = masterWSObject;
	const masterWSTabAsNumber =
		masterWSTabId && typeof masterWSTabId === 'string'
			? parseInt(masterWSTabId, 10)
			: null;

	return masterWSTabAsNumber;
};
/**
 * WSState related messaging functions
 */

export const sendUpdateInWSState = (data: WSStateMsg) => {
	sendMsgToAllTabs(MsgToTabType.SET_WS_STATE, data);
};

/**
 * Emacs related messaging functions
 */

export const handlePassingMessage = (
	sendJsonMessage: SendJsonMessage,
	message: MsgToTab
) => {
	if (message.data) {
		sendJsonMessage(message.data as EmacsSendMsg);
	} else {
		logMsgErr(
			LogLoc.NEWTAB,
			LogMsgDir.RECV,
			'Bad or no data for updating match query',
			message?.data
		);
	}
};

import { logMsg, logMsgErr } from './logging';
import {
	LogLoc,
	LogMsgDir,
	MsgDirection,
	type MsgToTab,
	MsgToTabType,
	MsgToBGSWType,
	getMsgToBGSWType,
	getMsgToTabType,
	MsgToBGSW,
	WSStateMsg,
	MsgToTabData,
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
	data?: MsgToTabData
) => {
	logMsg(
		LogLoc.NEWTAB,
		LogMsgDir.SEND,
		'Sending request to master tab',
		tabId,
		getMsgToTabType(type),
		data ? data : ''
	);
	chrome.tabs
		.sendMessage<MsgToTab>(tabId, {
			direction: MsgDirection.TO_NEWTAB,
			type,
			data,
		})
		.catch((err) => {
			logMsgErr(
				LogLoc.NEWTAB,
				LogMsgDir.SEND,
				'Error sending message to master tab:',
				err
			);
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
	chrome.runtime
		.sendMessage<MsgToTab>({
			direction: MsgDirection.TO_NEWTAB,
			type,
			data,
		})
		.catch((err) => {
			logMsgErr(
				LogLoc.NEWTAB,
				LogMsgDir.SEND,
				'Error sending message to all tabs:',
				err
			);
		});
};

/**
 * BGSW related messaging functions
 */

export const handleConfirmingRoleAsMaster = (
	sendResponse: SendResponseType,
	amMasterRole: boolean
) => {
	if (amMasterRole) {
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
 * WS (inter-tab) related messaging functions
 */

export const sendUpdateInWSState = (data: WSStateMsg) => {
	sendMsgToAllTabs(MsgToTabType.SET_WS_STATE, data);
};

export const sendToMasterTab = (type: MsgToTabType, data?: MsgToTabData) => {
	void getMasterWSTabId().then((masterWSTabNum) => {
		if (masterWSTabNum) {
			sendMsgToTab(type, masterWSTabNum, data);
		}
	});
};

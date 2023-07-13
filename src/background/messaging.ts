import {
	MsgToBGSWType,
	type MsgToBGSW,
	MsgToTabType,
	MsgDirection,
	type MsgToTab,
	getMsgToTabType,
	getMsgToBGSWType,
} from '../util/types';
import { LogLoc, LogMsgDir, logMsg, log, logMsgErr } from 'util/logging';
import connections from './Connections';
import masterWs from './MasterWS';

const TIME_TO_WAIT_FOR_TAB_ALIVE_RESPONSE = 500;

export const isMsgExpected = (
	message: MsgToBGSW,
	sender?: chrome.runtime.MessageSender
): message is MsgToBGSW => {
	if (!sender?.tab?.id) {
		logMsgErr(LogLoc.BGSW, LogMsgDir.RECV, 'No tab ID found in message');
		return false;
	}
	if (
		!message ||
		typeof message !== 'object' ||
		!('direction' in message) ||
		!('type' in message)
	) {
		logMsgErr(
			LogLoc.BGSW,
			LogMsgDir.RECV,
			'Invalid message recv:',
			message
		);
		return false;
	}
	if (message.direction !== MsgDirection.TO_BGSW) {
		return false;
	}
	logMsg(
		LogLoc.BGSW,
		LogMsgDir.RECV,
		'Data recv from',
		sender.tab.id,
		getMsgToBGSWType(message.type)
	);
	return true;
};

export const sendMsgToTab = async (type: MsgToTabType, tabId: number) => {
	logMsg(
		LogLoc.BGSW,
		LogMsgDir.SEND,
		'Sending message to ',
		tabId,
		getMsgToTabType(type)
	);
	const response = await chrome.tabs.sendMessage<MsgToTab, MsgToBGSW>(tabId, {
		direction: MsgDirection.TO_NEWTAB,
		type,
	});
	if (response) {
		if (response.direction !== MsgDirection.TO_BGSW) {
			throw new Error(
				'[BGSW] <= Invalid response direction',
				response.direction
			);
		}
		if (!response.type) {
			throw new Error('[BGSW] <= Invalid response type', response.type);
		}
		logMsg(
			LogLoc.BGSW,
			LogMsgDir.RECV,
			'Response recv from ',
			tabId,
			getMsgToBGSWType(response.type)
		);
		return response;
	}
	return;
};

const waitForTimeout = (tabId: number) => {
	return new Promise<null>((resolve) => {
		setTimeout(() => {
			log(
				LogLoc.BGSW,
				'Timed out waiting for alive response from',
				tabId
			);
			resolve(null);
		}, TIME_TO_WAIT_FOR_TAB_ALIVE_RESPONSE);
	});
};

export const confirmTabIdAlive = async (tabId: number) => {
	try {
		const tab = await chrome.tabs.get(tabId);
		if (tab.active && !tab.discarded && tab.status === 'complete') {
			const response = await Promise.race([
				waitForTimeout(tabId),
				sendMsgToTab(MsgToTabType.CONFIRM_IF_ALIVE, tabId),
			]);
			if (response && response?.type === MsgToBGSWType.CONFIRMED_ALIVE) {
				return true;
			}
		}
	} catch (err) {
		return false;
	}
	return false;
};

export const sendToGivenTabs = async (
	type: MsgToTabType,
	tabIds: Array<number>
) => {
	await Promise.allSettled(tabIds.map((tabId) => sendMsgToTab(type, tabId)));
};

const setAsClients = async (clientTabIds: Array<number>) => {
	await sendToGivenTabs(MsgToTabType.YOU_ARE_CLIENT_WS, clientTabIds);
};

export const setAsMaster = (masterTabId: number) => {
	log(LogLoc.BGSW, 'Setting master websocket:', masterTabId);
	const clientTabs = connections.tabIds.filter(
		(tabId) => tabId !== masterTabId
	);
	void Promise.all([
		masterWs.set(masterTabId),
		sendMsgToTab(MsgToTabType.YOU_ARE_MASTER_WS, masterTabId),
		setAsClients(clientTabs),
	]);
};

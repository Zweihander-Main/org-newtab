/* eslint-disable no-console */
import {
	MsgNewTabToBGSWType,
	type MsgNewTabToBGSW,
	MsgBGSWToNewTabType,
	MsgDirection,
	type MsgBGSWToNewTab,
} from '../types';

export const isMsgExpected = (
	message: MsgNewTabToBGSW | unknown,
	sender?: chrome.runtime.MessageSender
): message is MsgNewTabToBGSW => {
	if (!sender?.tab?.id) {
		console.error('[BSGW] No tab ID found in message');
		return false;
	}
	if (
		!message ||
		typeof message !== 'object' ||
		!('direction' in message) ||
		!('type' in message)
	) {
		console.error('[BSGW] Invalid message recv:', message);
		return false;
	}
	if (message.direction !== MsgDirection.TO_BGSW) {
		return false;
	}
	return true;
};

export const sendMsgToTab = async (
	type: MsgBGSWToNewTabType,
	tabId: number
) => {
	const response = await chrome.tabs.sendMessage<
		MsgBGSWToNewTab,
		MsgNewTabToBGSW
	>(tabId, {
		direction: MsgDirection.TO_NEWTAB,
		type,
	});
	if (response) {
		if (response.direction !== MsgDirection.TO_BGSW) {
			throw new Error('Invalid response direction', response.direction);
		}
		if (!response.type) {
			throw new Error('Invalid response type', response.type);
		}
		console.log('[BSGW] Got response from %d: %d', tabId, response.type);
		return response;
	}
	return;
};

export const confirmTabIdAlive = async (tabId: number) => {
	try {
		const response = await sendMsgToTab(
			MsgBGSWToNewTabType.CONFIRM_IF_ALIVE,
			tabId
		);
		if (response && response.type === MsgNewTabToBGSWType.CONFIRMED_ALIVE) {
			return true;
		}
	} catch (err) {
		return false;
	}
	return false;
};

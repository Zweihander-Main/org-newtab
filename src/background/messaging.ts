/* eslint-disable no-console */
import {
	MsgNewTabToBGSWType,
	type MsgNewTabToBGSW,
	MsgBGSWToNewTabType,
	MsgDirection,
	type MsgBGSWToNewTab,
	getMsgBGSWToNewType,
	getMsgNewTabToBGSWType,
} from '../types';

export const isMsgExpected = (
	message: MsgNewTabToBGSW | unknown,
	sender?: chrome.runtime.MessageSender
): message is MsgNewTabToBGSW => {
	if (!sender?.tab?.id) {
		console.error('[BSGW] <= No tab ID found in message');
		return false;
	}
	if (
		!message ||
		typeof message !== 'object' ||
		!('direction' in message) ||
		!('type' in message)
	) {
		console.error('[BSGW] <= Invalid message recv:', message);
		return false;
	}
	if (message.direction !== MsgDirection.TO_BGSW) {
		return false;
	}
	console.log(
		'[BSGW] <= Data recv from %d: %s',
		sender.tab.id,
		MsgNewTabToBGSWType[message.type as MsgNewTabToBGSWType]
	);
	return true;
};

export const sendMsgToTab = async (
	type: MsgBGSWToNewTabType,
	tabId: number
) => {
	console.log(
		'[BSGW] => Sending message to %d: %s',
		tabId,
		getMsgBGSWToNewType(type)
	);
	const response = await chrome.tabs.sendMessage<
		MsgBGSWToNewTab,
		MsgNewTabToBGSW
	>(tabId, {
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
		console.log(
			'[BSGW] <= Recv response from %d: %s',
			tabId,
			getMsgNewTabToBGSWType(response.type)
		);
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

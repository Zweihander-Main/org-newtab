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

const TIME_TO_WAIT_FOR_TAB_ALIVE_RESPONSE = 500;

export const isMsgExpected = (
	message: MsgNewTabToBGSW | unknown,
	sender?: chrome.runtime.MessageSender
): message is MsgNewTabToBGSW => {
	if (!sender?.tab?.id) {
		console.error('[BGSW] <= No tab ID found in message');
		return false;
	}
	if (
		!message ||
		typeof message !== 'object' ||
		!('direction' in message) ||
		!('type' in message)
	) {
		console.error('[BGSW] <= Invalid message recv:', message);
		return false;
	}
	if (message.direction !== MsgDirection.TO_BGSW) {
		return false;
	}
	console.log(
		'[BGSW] <= Data recv from %d: %s',
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
		'[BGSW] => Sending message to %d: %s',
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
			'[BGSW] <= Recv response from %d: %s',
			tabId,
			getMsgNewTabToBGSWType(response.type)
		);
		return response;
	}
	return;
};

export const confirmTabIdAlive = async (tabId: number) => {
	try {
		const tab = await chrome.tabs.get(tabId);
		if (tab.active && !tab.discarded && tab.status === 'complete') {
			const response = await Promise.race([
				new Promise((resolve) =>
					setTimeout(resolve, TIME_TO_WAIT_FOR_TAB_ALIVE_RESPONSE)
				),
				(async () => {
					return await sendMsgToTab(
						MsgBGSWToNewTabType.CONFIRM_IF_ALIVE,
						tabId
					);
				})(),
			]);
			if (
				response &&
				(response as MsgNewTabToBGSW)?.type ===
					MsgNewTabToBGSWType.CONFIRMED_ALIVE
			) {
				return true;
			}
		}
	} catch (err) {
		return false;
	}
	return false;
};

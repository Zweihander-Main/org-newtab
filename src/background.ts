/* eslint-disable no-console */
import { Storage } from '@plasmohq/storage';
import {
	MsgNewTabToBGSWType,
	type MsgNewTabToBGSW,
	MsgBGSWToNewTabType,
	MsgDirection,
} from './types';

/**
 * Some notes about this background script:
 * 1. It was written when the Manifest V3 spec was still being fixed up. In
 *    particular, it assumes there's no way to have a persistent background
 *    websocket connection. Therefore, it can be unloaded by the browser at any
 *    time and has to attempt to persist app state using storage.
 * 2. Because the websocket can't live here, the background script has to act as
 *    a message broker between the different new tabs, attempting to keep no
 *    more than one websocket connection alive at any given time.
 * 3. It was written when the Manifest v3 spec still had odd edge cases. Hence
 *    the very over the top management of listeners and ports.
 * 4. It uses ports to communicate most queries from new tabs to the background,
 *    regular messages back and forth for the master websocket query flow.
 */

//NEXT: confirm tab exists before sending message
//NEXT: confirm tab exists when using masterWSTabId
//NEXT: store tabids in storage
//NEXT: store masterWSTabId in storage
//NEXT: Delete max wait time for all client replies if not needed

const storage = new Storage({ area: 'local' });
const connectedTabIds: Set<number> = new Set([]);
const connectedPorts: Set<chrome.runtime.Port> = new Set([]);
const MAX_WAIT_TIME_FOR_ALL_CLIENT_REPLIES = 2000;
let masterWSTabId: number | null = null;

const errorSendingCatch = (err: unknown) => {
	console.error('[BSGW] Error sending message from background:', err);
};

const isMsgExpected = (
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

const sendMsgToTab = (type: MsgBGSWToNewTabType, tabId: number) => {
	return chrome.tabs
		.sendMessage(tabId, {
			direction: MsgDirection.TO_NEWTAB,
			type,
		})
		.catch(errorSendingCatch);
};

const handleQueryFlowCaseSingleConnectionBecomesMaster = async (
	singleRequestingTabId: number
) => {
	masterWSTabId = singleRequestingTabId;
	console.log('[BSGW] First connection, assuming %d master', masterWSTabId);
	await sendMsgToTab(MsgBGSWToNewTabType.YOU_ARE_MASTER_WS, masterWSTabId);
};

const handleQueryFlowCaseTellOthersTheyAreClients = async (
	tabIdsInvolved: Array<number>
) => {
	const tabIdsToInform = tabIdsInvolved.filter(
		(tabId) => tabId !== masterWSTabId
	);
	await Promise.all(
		tabIdsToInform.map((tabId) =>
			sendMsgToTab(MsgBGSWToNewTabType.YOU_ARE_CLIENT_WS, tabId)
		)
	);
};

const handleQueryFlowIdentifiedMaster = (masterTabId: number) => {
	masterWSTabId = masterTabId;
	console.log('[BSGW] Identified master WS as %d', masterWSTabId);
};

const handleQueryFlowIdentifiedClient = (clientTabId: number) => {
	console.log('[BSGW] Identified client to WS as %d', clientTabId);
};

const handleQueryFlowRequestBecomesMaster = async (requestingTabId: number) => {
	masterWSTabId = requestingTabId;
	console.log(
		'[BSGW] No master identified, letting %d be master',
		masterWSTabId
	);
	await sendMsgToTab(MsgBGSWToNewTabType.YOU_ARE_MASTER_WS, masterWSTabId);
};

const handleQueryFlowCaseFindMaster = async (requestingTabId: number) => {
	masterWSTabId = null; // Will be set if one identifies as master
	const answeredPorts = await Promise.all(
		Array.from(connectedTabIds).map((connectedTabId) => {
			return sendMsgToTab(
				MsgBGSWToNewTabType.CONFIRM_IF_MASTER_WS,
				connectedTabId
			).then((response: MsgNewTabToBGSW) => {
				console.log(
					'[BSGW] Got response from %d: %d',
					connectedTabId,
					response.type
				);
				switch (response.type) {
					case MsgNewTabToBGSWType.IDENTIFY_AS_MASTER_WS:
						handleQueryFlowIdentifiedMaster(connectedTabId);
						break;
					case MsgNewTabToBGSWType.IDENTIFY_AS_WS_CLIENT:
						handleQueryFlowIdentifiedClient(connectedTabId);
						break;
				}
				return connectedTabId;
			});
		})
	);
	// If all of them think they're clients, set the original requestor as master
	if (masterWSTabId === null) {
		await handleQueryFlowRequestBecomesMaster(requestingTabId);
	}
	await handleQueryFlowCaseTellOthersTheyAreClients(answeredPorts);
};

const handleQueryFlow = async (requestingTabId: number) => {
	console.log('[BSGW] connectedTabIds: ', connectedTabIds);
	if (connectedTabIds.size === 1) {
		await handleQueryFlowCaseSingleConnectionBecomesMaster(requestingTabId);
	} else {
		await handleQueryFlowCaseFindMaster(requestingTabId);
	}
};

const handlePortMessage = (
	message: MsgNewTabToBGSW,
	port: chrome.runtime.Port
) => {
	if (!isMsgExpected(message, port?.sender)) return;
	const tabId = port?.sender?.tab?.id as number;
	console.log(
		'[BSGW] handlePortMessage -- data recv from %d: %d',
		tabId,
		message.type
	);
	switch (message.type) {
		case MsgNewTabToBGSWType.QUERY_STATUS_OF_WS: {
			handleQueryFlow(tabId).catch((err) => {
				console.error('[BSGW] Error handling query flow:', err);
			});
			break;
		}
	}
};

const addPort = (port: chrome.runtime.Port) => {
	connectedPorts.add(port);
	connectedTabIds.add(port?.sender?.tab?.id as number);
};

const removePort = (port: chrome.runtime.Port) => {
	connectedPorts.delete(port);
	connectedTabIds.delete(port?.sender?.tab?.id as number);
};

const handlePortDisconnect = (port: chrome.runtime.Port) => {
	console.log('[BGSW] Disconnecting port:', port?.sender?.tab?.id);
	port.onDisconnect.removeListener(handlePortDisconnect);
	removePort(port);
	if (port?.sender?.tab?.id === masterWSTabId) {
		masterWSTabId = null;
		if (connectedTabIds.size >= 1) {
			const newRequestingTabId = Array.from(connectedTabIds)[0];
			if (connectedTabIds.size === 1) {
				handleQueryFlowCaseSingleConnectionBecomesMaster(
					newRequestingTabId
				).catch((err) => {
					console.error(
						'[BSGW] Error handling single tab connection:',
						err
					);
				});
			} else if (connectedTabIds.size > 1) {
				handleQueryFlowCaseFindMaster(newRequestingTabId).catch(
					(err) => {
						console.error(
							'[BSGW] Error handling selecting new master:',
							err
						);
					}
				);
			}
		}
	}
};

const handlePortConnect = (port: chrome.runtime.Port) => {
	if (
		port.name === 'ws' &&
		port?.sender?.tab?.id &&
		!connectedPorts.has(port) &&
		!connectedTabIds.has(port.sender.tab.id)
	) {
		console.log('[BSGW] Connecting port:', port.sender.tab.id);
		addPort(port);
		if (!port.onMessage.hasListener(handlePortMessage)) {
			port.onMessage.addListener(handlePortMessage);
			port.onDisconnect.addListener(handlePortDisconnect);
		}
	}
	return true;
};

if (!chrome.runtime.onConnect.hasListener(handlePortConnect)) {
	chrome.runtime.onConnect.addListener(handlePortConnect);
}

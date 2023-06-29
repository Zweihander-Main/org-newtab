/* eslint-disable no-console */
import { Storage } from '@plasmohq/storage';
import {
	MsgNewTabToBGSWType,
	type MsgNewTabToBGSW,
	type MsgBGSWToNewTab,
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
 * 4. It avoid using ports due to bugs that pop up with multiple simultaneous
 *    connections.
 */

const storage = new Storage({ area: 'local' });
const connectedTabIds: Set<number> = new Set([]);
const connectedPorts: Set<chrome.runtime.Port> = new Set([]);
const MAX_WAIT_TIME_FOR_ALL_CLIENT_REPLIES = 2000;
let masterWSTabId: number | null = null;

const clientIdentifyPromises = () => {
	return Array.from(connectedTabIds).map((connectedTabId) => {
		return new Promise<void>((resolve) => {
			chrome.tabs
				.sendMessage(connectedTabId, {
					type: MsgBGSWToNewTabType.CONFIRM_IF_MASTER_WS,
				} as MsgBGSWToNewTab)
				.then((response: MsgNewTabToBGSW) => {
					switch (response.type) {
						case MsgNewTabToBGSWType.IDENTIFY_AS_MASTER_WS: {
							console.log('[BSGW] Identified master websocket');
							masterWSTabId = connectedTabId;
							// Tell all other tabs they're clients
							connectedTabIds.forEach((savedTabId) => {
								if (savedTabId !== connectedTabId) {
									chrome.tabs
										.sendMessage(savedTabId, {
											type: MsgBGSWToNewTabType.YOU_ARE_CLIENT_WS,
										} as MsgBGSWToNewTab)
										.catch(errorSendingCatch);
								}
							});
							break;
						}
						case MsgNewTabToBGSWType.IDENTIFY_AS_WS_CLIENT: {
							console.log('[BSGW] Identified client');
							break;
						}
					}
					resolve();
				})
				.catch(errorSendingCatch);
		});
	});
};

// Listen for messages from the new tab page
// const onMessage = (message: MsgNewTabToBGSW, sender: chrome.runtime.MessageSender) => {
// 	storage
// 		.get<Array<number>>('connectedTabIds')
// 		.then((connectedTabIdsFromStorage) => {
// 			console.log(
// 				'[BSGW] connectedTabIdsFromStorage:',
// 				connectedTabIdsFromStorage
// 			);
// 			connectedTabIdsFromStorage.forEach((connectedTabId) =>
// 				connectedTabIds.add(connectedTabId)
// 			);
// 			if (sender?.tab?.id) {
// 				const portTabId = port.sender.tab.id;
// 				console.log('[BSGW] Data recv on bg end:', message.type);
// 				connectedTabIds.add(portTabId);
// 				storage
// 					.set('connectedTabIds', Array.from(connectedTabIds))
// 					.catch((err) => {
// 						console.error(
// 							'[BSGW] Error saving connectedTabIds:',
// 							err
// 						);
// 					});
// 			}
// 			console.log('[BSGW] Connections:', connectedTabIds);

// 			switch (message.type) {
// 				case MsgNewTabToBGSWType.QUERY_STATUS_OF_WS: {
// 					// TODO use masterWs to check if it's still connected
// 					// First connection, this is master
// 					if (connectedTabIds.size === 1) {
// 						console.log('[BSGW] First connection, assuming master');
// 						masterWSTabId = connectedTabIds.values().next()
// 							.value as number;
// 						chrome.tabs
// 							.sendMessage(masterWSTabId, {
// 								type: MsgBGSWToNewTabType.YOU_ARE_MASTER_WS,
// 							} as MsgBGSWToNewTab)
// 							.catch(errorSendingCatch);
// 					} else {
// 						// Ask other tabs to identify themselves
// 						console.log(
// 							'[BSGW] Asking other tabs to identify themselves, num connections:',
// 							connectedTabIds.size
// 						);

// 						Promise.race([
// 							Promise.all(clientIdentifyPromises()),
// 							new Promise<void>((resolve) =>
// 								setTimeout(() => {
// 									console.log('[BSGW] Timeout occurred');
// 									resolve();
// 								}, MAX_WAIT_TIME_FOR_ALL_CLIENT_REPLIES)
// 							),
// 						])
// 							.then(() => {
// 								// All client tabs replied or the timeout occurred
// 								if (
// 									masterWSTabId === null &&
// 									port?.sender?.tab?.id
// 								) {
// 									// No master WebSocket identified, so make this connection the master
// 									masterWSTabId = port.sender.tab.id;
// 									chrome.tabs
// 										.sendMessage(masterWSTabId, {
// 											type: MsgBGSWToNewTabType.YOU_ARE_MASTER_WS,
// 										} as MsgBGSWToNewTab)
// 										.catch(errorSendingCatch);
// 								}
// 							})
// 							.catch(() => {
// 								console.error('[BSGW] Timeout occurred');
// 								return;
// 							});
// 					}

// 					break;
// 				}
// 			}
// 		})
// 		.catch((err) => {
// 			console.error('[BSGW] Error getting connectedTabIds:', err);
// 		});
// 	return true;
// };

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

const handleQueryFlow = async (
	message: MsgNewTabToBGSW,
	port: chrome.runtime.Port
) => {
	switch (message.type) {
		case MsgNewTabToBGSWType.QUERY_STATUS_OF_WS: {
			// TODO use masterWs to check if it's still connected
			console.log('[BSGW] connectedPorts: ', connectedPorts);
			if (connectedPorts.size === 1) {
				masterWSTabId = port?.sender?.tab?.id as number;
				console.log(
					'[BSGW] First connection, assuming %d master',
					masterWSTabId
				);
				await sendMsgToTab(
					MsgBGSWToNewTabType.YOU_ARE_MASTER_WS,
					masterWSTabId
				);
			} else {
				await Promise.all(
					Array.from(connectedPorts).map((connectedPort) => {
						return sendMsgToTab(
							MsgBGSWToNewTabType.CONFIRM_IF_MASTER_WS,
							connectedPort?.sender?.tab?.id as number
						).then((response) => {
							if (
								response ===
								MsgNewTabToBGSWType.IDENTIFY_AS_MASTER_WS
							) {
								masterWSTabId = connectedPort?.sender?.tab
									?.id as number;
								console.log(
									'[BSGW] Identified master WS as %d',
									masterWSTabId
								);
							} else if (
								response ===
								MsgNewTabToBGSWType.IDENTIFY_AS_WS_CLIENT
							) {
								console.log(
									'[BSGW] Identified client to WS as %d',
									connectedPort?.sender?.tab?.id as number
								);
								return;
							}
						});
					})
				);
				// TODO NEXT: If all of them think they're clients, set the original as master
			}
			break;
		}
	}
};

const handlePortMessage = (
	message: MsgNewTabToBGSW,
	port: chrome.runtime.Port
) => {
	if (!isMsgExpected(message, port?.sender)) return;
	console.log(
		'[BSGW] handleMessage -- data recv from %d: %d',
		port?.sender?.tab?.id as number,
		message.type
	);
	switch (message.type) {
		case MsgNewTabToBGSWType.QUERY_STATUS_OF_WS: {
			handleQueryFlow(message, port).catch((err) => {
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

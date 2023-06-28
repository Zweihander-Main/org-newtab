/* eslint-disable no-console */
import { Storage } from '@plasmohq/storage';

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
 * 4. It uses ports to receive communications and tab message flow to send them.
 */

export enum MsgNewTabToBGSWType {
	QUERY_STATUS_OF_WS = 1,
	IDENTIFY_AS_MASTER_WS = 2,
	IDENTIFY_AS_WS_CLIENT = 3,
}

export enum MsgBGSWToNewTabType {
	CONFIRM_IF_MASTER_WS = 1,
	YOU_ARE_MASTER_WS = 2,
	YOU_ARE_CLIENT_WS = 3,
}

export type MsgNewTabToBGSW = {
	type: MsgNewTabToBGSWType;
};

export type MsgBGSWToNewTab = {
	type: MsgBGSWToNewTabType;
};

const storage = new Storage({ area: 'local' });
const connectedTabsByPort: Record<string, chrome.runtime.Port> = {};
const connectedTabIds: Set<number> = new Set([]);
const MAX_WAIT_TIME_FOR_ALL_CLIENT_REPLIES = 2000;
let masterWSTabId: number | null = null;

const errorSendingCatch = (err: unknown) => {
	console.error('[BSGW] Error sending message from background:', err);
};

const clientIdentifyPromises = () => {
	const returnPromises: Array<Promise<void>> = [];
	connectedTabIds.forEach((connectedTabId) => {
		const connectedTabPort = connectedTabsByPort[connectedTabId];
		if (connectedTabPort) {
			returnPromises.push(
				new Promise<void>((resolve) => {
					const identifyAsClientListener = (
						message: MsgBGSWToNewTab
					) => {
						if (
							message.type ===
							MsgBGSWToNewTabType.YOU_ARE_CLIENT_WS
						) {
							console.log('[BSGW] Identified as client');
							resolve();
						}
						connectedTabPort.onMessage.removeListener(
							identifyAsClientListener
						);
						return true;
					};
					connectedTabPort.onMessage.addListener(
						identifyAsClientListener
					);
					chrome.tabs
						.sendMessage(connectedTabId, {
							type: MsgBGSWToNewTabType.CONFIRM_IF_MASTER_WS,
						} as MsgBGSWToNewTab)
						.catch(errorSendingCatch);
				})
			);
		} else {
			console.log(
				'[BSGW] No connected tab id port found to identify as client'
			);
			returnPromises.push(Promise.resolve());
		}
	});
	return returnPromises;
};

// Listen for messages from the new tab page
const onMessage = (message: MsgNewTabToBGSW, port: chrome.runtime.Port) => {
	storage
		.get<Array<number>>('connectedTabIds')
		.then((connectedTabIdsFromStorage) => {
			console.log(
				'[BSGW] connectedTabIdsFromStorage:',
				connectedTabIdsFromStorage
			);
			connectedTabIdsFromStorage.forEach((connectedTabId) =>
				connectedTabIds.add(connectedTabId)
			);
			if (port?.sender?.tab?.id) {
				const portTabId = port.sender.tab.id;
				console.log('[BSGW] Data recv on bg end:', message.type);
				connectedTabIds.add(portTabId);
				storage
					.set('connectedTabIds', Array.from(connectedTabIds))
					.catch((err) => {
						console.error(
							'[BSGW] Error saving connectedTabIds:',
							err
						);
					});
				if (!connectedTabsByPort[portTabId]) {
					connectedTabsByPort[portTabId] = port;
				}
			}
			console.log('[BSGW] Connections:', connectedTabIds);

			switch (message.type) {
				case MsgNewTabToBGSWType.QUERY_STATUS_OF_WS: {
					// TODO use masterWs to check if it's still connected
					// First connection, this is master
					if (connectedTabIds.size === 1) {
						console.log('[BSGW] First connection, assuming master');
						masterWSTabId = connectedTabIds.values().next()
							.value as number;
						chrome.tabs
							.sendMessage(masterWSTabId, {
								type: MsgBGSWToNewTabType.YOU_ARE_MASTER_WS,
							} as MsgBGSWToNewTab)
							.catch(errorSendingCatch);
					} else {
						// Ask other tabs to identify themselves
						console.log(
							'[BSGW] Asking other tabs to identify themselves, num connections:',
							connectedTabIds.size
						);

						Promise.race([
							Promise.all(clientIdentifyPromises()),
							new Promise<void>((resolve) =>
								setTimeout(() => {
									console.log('[BSGW] Timeout occurred');
									resolve();
								}, MAX_WAIT_TIME_FOR_ALL_CLIENT_REPLIES)
							),
						])
							.then(() => {
								// All client tabs replied or the timeout occurred
								if (
									masterWSTabId === null &&
									port?.sender?.tab?.id
								) {
									// No master WebSocket identified, so make this connection the master
									masterWSTabId = port.sender.tab.id;
									chrome.tabs
										.sendMessage(masterWSTabId, {
											type: MsgBGSWToNewTabType.YOU_ARE_MASTER_WS,
										} as MsgBGSWToNewTab)
										.catch(errorSendingCatch);
								}
							})
							.catch(() => {
								console.error('[BSGW] Timeout occurred');
								return;
							});
					}

					break;
				}
				case MsgNewTabToBGSWType.IDENTIFY_AS_MASTER_WS: {
					// Tell all other tabs they're clients
					connectedTabIds.forEach((connectedTabId) => {
						if (connectedTabId !== port?.sender?.tab?.id) {
							chrome.tabs
								.sendMessage(connectedTabId, {
									type: MsgBGSWToNewTabType.YOU_ARE_CLIENT_WS,
								} as MsgBGSWToNewTab)
								.catch(errorSendingCatch);
						}
					});
					break;
				}
			}
		})
		.catch((err) => {
			console.error('[BSGW] Error getting connectedTabIds:', err);
		});
	return true;
};

// Remove the connection when the new tab page is closed
const onDisconnect = (port: chrome.runtime.Port) => {
	console.log('[BGSW] Disconnecting port:', port?.sender?.tab?.id);
	port.onDisconnect.removeListener(onDisconnect);
	if (port?.sender?.tab?.id) {
		connectedTabIds.delete(port.sender.tab.id);
		storage
			.set('connectedTabIds', Array.from(connectedTabIds))
			.catch((err) => {
				console.error('[BSGW] Error saving connectedTabIds:', err);
			});
		connectedTabsByPort[port.sender.tab.id].onDisconnect.removeListener(
			onDisconnect
		);
		delete connectedTabsByPort[port.sender.tab.id];
	}
};

// Extra checking to make sure we're only listening once due to Chrome MW3 bug
const onConnect = (port: chrome.runtime.Port) => {
	if (
		port.name === 'ws' &&
		port?.sender?.tab?.id &&
		!connectedTabsByPort[port.sender.tab.id]
	) {
		console.log('[BSGW] Connecting port:', port?.sender?.tab?.id);
		if (!port.onMessage.hasListener(onMessage)) {
			port.onMessage.addListener(onMessage);
		}
		if (!port.onDisconnect.hasListener(onDisconnect)) {
			port.onDisconnect.addListener(onDisconnect);
		}
	}
	return true;
};

if (!chrome.runtime.onConnect.hasListener(onConnect)) {
	chrome.runtime.onConnect.addListener(onConnect);
}

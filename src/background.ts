/* eslint-disable no-console */

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

const connectedTabsById: Record<string, chrome.runtime.Port> = {};
const MAX_WAIT_TIME_FOR_ALL_CLIENT_REPLIES = 2000;
let masterWs: chrome.runtime.Port | null = null;

const clientIdentifyPromises = () =>
	Object.values(connectedTabsById).map((tabConnectionPort) => {
		return new Promise<void>((resolve) => {
			const identifyAsClientListener = (message: MsgBGSWToNewTab) => {
				if (message.type === MsgBGSWToNewTabType.YOU_ARE_CLIENT_WS) {
					console.log('[BSGW] Identified as client');
					resolve();
				}
				tabConnectionPort.onMessage.removeListener(
					identifyAsClientListener
				);
				return true;
			};
			tabConnectionPort.onMessage.addListener(identifyAsClientListener);

			tabConnectionPort.postMessage({
				type: MsgBGSWToNewTabType.CONFIRM_IF_MASTER_WS,
			} as MsgBGSWToNewTab);
		});
	});

// Listen for messages from the new tab page
const onMessage = (message: MsgNewTabToBGSW, port: chrome.runtime.Port) => {
	console.log('[BSGW] Data recv on bg end:', message.type);
	console.log('[BSGW] Connections:', connectedTabsById);

	switch (message.type) {
		case MsgNewTabToBGSWType.QUERY_STATUS_OF_WS: {
			// TODO use masterWs to check if it's still connected
			// First connection, this is master
			if (Object.keys(connectedTabsById).length === 1) {
				chrome.tabs
					.sendMessage(
						parseInt(Object.keys(connectedTabsById)[0], 10),
						{
							message: 'Hello from your new tab page!',
						}
					)
					.then((response) => {
						console.log('Response from background:', response);
					})
					.catch((err: unknown) => {
						console.error(
							'Error sending message to background:',
							err
						);
					});

				console.log('[BSGW] First connection, assuming master');
				masterWs = port;
				port.postMessage({
					type: MsgBGSWToNewTabType.YOU_ARE_MASTER_WS,
				} as MsgBGSWToNewTab);
			} else {
				// Ask other tabs to identify themselves
				console.log(
					'[BSGW] Asking other tabs to identify themselves, num connections:',
					Object.keys(connectedTabsById).length
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
						if (masterWs === null) {
							// No master WebSocket identified, so make this connection the master
							masterWs = port;
							port.postMessage({
								type: MsgBGSWToNewTabType.YOU_ARE_MASTER_WS,
							} as MsgBGSWToNewTab);
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
			Object.values(connectedTabsById).forEach((tabConnectionPort) => {
				if (tabConnectionPort !== port) {
					tabConnectionPort.postMessage({
						type: MsgBGSWToNewTabType.YOU_ARE_CLIENT_WS,
					} as MsgBGSWToNewTab);
				}
			});
			break;
		}
	}
	return true;
};

// Remove the connection when the new tab page is closed
const onDisconnect = (port: chrome.runtime.Port) => {
	console.log('[BGSW] Disconnecting port:', port, port.sender);
	console.dir(port);
	port.onDisconnect.removeListener(onDisconnect);
	if (port?.sender?.tab?.id) {
		connectedTabsById[port.sender.tab.id].onDisconnect.removeListener(
			onDisconnect
		);
		delete connectedTabsById[port.sender.tab.id];
	}
};

// Extra checking to make sure we're only listening once due to Chrome MW3 bug
const onConnect = (port: chrome.runtime.Port) => {
	if (port.name === 'ws') {
		if (port?.sender?.tab?.id && !connectedTabsById[port.sender.tab.id]) {
			connectedTabsById[port.sender.tab.id] = port;
			if (!port.onMessage.hasListener(onMessage)) {
				port.onMessage.addListener(onMessage);
			}
			if (!port.onDisconnect.hasListener(onDisconnect)) {
				port.onDisconnect.addListener(onDisconnect);
			}
		}
	}
	return true;
};

if (!chrome.runtime.onConnect.hasListener(onConnect)) {
	chrome.runtime.onConnect.addListener(onConnect);
}

export {};

/* eslint-disable no-console */
import {
	MsgNewTabToBGSWType,
	type MsgNewTabToBGSW,
	MsgBGSWToNewTabType,
} from '../types';
import { isMsgExpected, sendMsgToTab, setAsMaster } from './messaging';
import connections from './Connections';
import masterWs from './MasterWS';

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

const searchAndFindMaster = async (requestingTabId: number) => {
	await masterWs.set(null);
	let alreadyExistingMaster;
	await Promise.all(
		connections.tabIds.map(async (connectedTabId) => {
			const response = await sendMsgToTab(
				MsgBGSWToNewTabType.CONFIRM_IF_MASTER_WS,
				connectedTabId
			);
			if (response) {
				switch (response.type) {
					case MsgNewTabToBGSWType.IDENTIFY_AS_MASTER_WS:
						alreadyExistingMaster = connectedTabId;
						console.log(
							'[BGSW] Identified master WS as %d',
							alreadyExistingMaster
						);
						break;
					case MsgNewTabToBGSWType.IDENTIFY_AS_WS_CLIENT:
						console.log(
							'[BGSW] Identified client to WS as %d',
							connectedTabId
						);
						break;
				}
				return connectedTabId;
			}
			return null;
		})
	);
	if (alreadyExistingMaster) {
		setAsMaster(alreadyExistingMaster);
	} else {
		setAsMaster(requestingTabId);
	}
};

const figureOutMaster = async (requestingTabId: number) => {
	console.log(
		'[BGSW] Figuring out master, current connections: ',
		connections.tabIds
	);
	if (connections.size === 1) {
		setAsMaster(requestingTabId);
	} else {
		await searchAndFindMaster(requestingTabId);
	}
};

const handlePortMessage = (
	message: MsgNewTabToBGSW,
	port: chrome.runtime.Port
) => {
	if (!isMsgExpected(message, port?.sender)) return;
	const tabId = port?.sender?.tab?.id as number;
	switch (message.type) {
		case MsgNewTabToBGSWType.QUERY_STATUS_OF_WS: {
			void figureOutMaster(tabId);
			break;
		}
	}
};

const handlePortDisconnect = (port: chrome.runtime.Port) => {
	console.log('[BGSW] Disconnecting port:', port?.sender?.tab?.id);
	port.onMessage.removeListener(handlePortMessage);
	port.onDisconnect.removeListener(handlePortDisconnect);
	void connections.remove(port);
	if (port?.sender?.tab?.id === masterWs.val) {
		void masterWs.set(null);
		if (connections.size >= 1) {
			const newRequestingTabId = connections.tabIds[0];
			void setAsMaster(newRequestingTabId);
		}
	}
};

const handlePortConnect = (port: chrome.runtime.Port) => {
	if (port.name === 'ws' && port?.sender?.tab?.id && !connections.has(port)) {
		console.log('[BGSW] Connecting port:', port.sender.tab.id);
		if (!port.onMessage.hasListener(handlePortMessage)) {
			port.onMessage.addListener(handlePortMessage);
		}
		if (!port.onDisconnect.hasListener(handlePortDisconnect)) {
			port.onDisconnect.addListener(handlePortDisconnect);
		}
		void connections.add(port);
	}
	return true;
};

if (!chrome.runtime.onConnect.hasListener(handlePortConnect)) {
	chrome.runtime.onConnect.addListener(handlePortConnect);
}

// Load connections from storage, should be run if the BGSW is reloaded
void connections.loadFromStorage();
void masterWs.loadFromStorage();

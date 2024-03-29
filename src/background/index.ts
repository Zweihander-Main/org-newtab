import {
	MsgToBGSWType,
	type MsgToBGSW,
	MsgToTabType,
	LogLoc,
} from '../lib/types';
import { isMsgExpected, sendMsgToTab, setAsMaster } from './messaging';
import connections from './Connections';
import masterWs from './MasterWS';
import { log } from 'lib/logging';

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
	await Promise.allSettled(
		connections.tabIds.map(async (connectedTabId) => {
			const response = await sendMsgToTab(
				MsgToTabType.CONFIRM_YOUR_ROLE_IS_MASTER,
				connectedTabId
			);
			if (response) {
				switch (response.type) {
					case MsgToBGSWType.IDENTIFY_ROLE_MASTER:
						alreadyExistingMaster = connectedTabId;
						log(LogLoc.BGSW, 'Found master WS as', connectedTabId);
						break;
					case MsgToBGSWType.IDENTIFY_ROLE_CLIENT:
						log(LogLoc.BGSW, 'Found client WS as', connectedTabId);
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
	log(
		LogLoc.BGSW,
		'Figuring out master, current connections:',
		connections.tabIds
	);
	/**
	 * If masterWs is null, either it fired onDisconnect or the background
	 * script was reloaded and it didn't answer as alive when loaded from
	 * storage.
	 */
	if (masterWs.val) {
		setAsMaster(masterWs.val);
	} else if (connections.size === 1) {
		setAsMaster(requestingTabId);
	} else {
		await searchAndFindMaster(requestingTabId);
	}
};

const handlePortMessage = (message: MsgToBGSW, port: chrome.runtime.Port) => {
	if (!isMsgExpected(message, port?.sender)) return;
	const tabId = port?.sender?.tab?.id as number;
	switch (message.type) {
		case MsgToBGSWType.QUERY_WS_ROLE: {
			void figureOutMaster(tabId);
			break;
		}
	}
};

const handlePortDisconnect = (port: chrome.runtime.Port) => {
	log(LogLoc.BGSW, 'Disconnecting port:', port?.sender?.tab?.id);
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
		log(LogLoc.BGSW, 'Connecting port:', port.sender.tab.id);
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

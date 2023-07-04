/* eslint-disable no-console */
import { Storage } from '@plasmohq/storage';
import {
	MsgNewTabToBGSWType,
	type MsgNewTabToBGSW,
	MsgBGSWToNewTabType,
} from '../types';
import { isMsgExpected, sendMsgToTab } from './messaging';
import MasterWSTabId from './MasterWsTabId';
import Connections from './Connections';

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

const storage = new Storage({ area: 'local' });
const connections = Connections.getInstance(storage);
const masterWSTabId = MasterWSTabId.getInstance(storage);

const handleQueryFlowCaseSingleConnectionBecomesMaster = async (
	singleRequestingTabId: number
) => {
	await masterWSTabId.set(singleRequestingTabId);
	console.log('[BSGW] First connection, assuming %d master', masterWSTabId);
	await sendMsgToTab(
		MsgBGSWToNewTabType.YOU_ARE_MASTER_WS,
		singleRequestingTabId
	);
};

const handleQueryFlowCaseTellOthersTheyAreClients = async (
	tabIdsInvolved: Array<number>
) => {
	const tabIdsToInform = tabIdsInvolved.filter(
		(tabId) => tabId !== masterWSTabId.get()
	);
	await Promise.all(
		tabIdsToInform.map((tabId) =>
			sendMsgToTab(MsgBGSWToNewTabType.YOU_ARE_CLIENT_WS, tabId)
		)
	);
};

const handleQueryFlowIdentifiedMaster = async (masterTabId: number) => {
	await masterWSTabId.set(masterTabId);
	console.log('[BSGW] Identified master WS as %d', masterWSTabId);
};

const handleQueryFlowIdentifiedClient = (clientTabId: number) => {
	console.log('[BSGW] Identified client to WS as %d', clientTabId);
};

const handleQueryFlowRequestBecomesMaster = async (requestingTabId: number) => {
	await masterWSTabId.set(requestingTabId);
	console.log(
		'[BSGW] No master identified, letting %d be master',
		masterWSTabId
	);
	await sendMsgToTab(MsgBGSWToNewTabType.YOU_ARE_MASTER_WS, requestingTabId);
};

const handleQueryFlowCaseFindMaster = async (requestingTabId: number) => {
	await masterWSTabId.set(null); // Will be set if one identifies as master
	const answeredTabIds = await Promise.all(
		connections.tabIds.map(async (connectedTabId) => {
			const response = await sendMsgToTab(
				MsgBGSWToNewTabType.CONFIRM_IF_MASTER_WS,
				connectedTabId
			);
			if (response) {
				switch (response.type) {
					case MsgNewTabToBGSWType.IDENTIFY_AS_MASTER_WS:
						await handleQueryFlowIdentifiedMaster(connectedTabId);
						break;
					case MsgNewTabToBGSWType.IDENTIFY_AS_WS_CLIENT:
						handleQueryFlowIdentifiedClient(connectedTabId);
						break;
				}
				return connectedTabId;
			}
			return null;
		})
	);
	const aliveAnsweredTabIds = answeredTabIds.filter(
		(tabId): tabId is number => tabId !== null
	);
	// If all of them think they're clients, set the original requestor as master
	if (masterWSTabId === null) {
		await handleQueryFlowRequestBecomesMaster(requestingTabId);
	}
	await handleQueryFlowCaseTellOthersTheyAreClients(aliveAnsweredTabIds);
};

const handleQueryFlow = async (requestingTabId: number) => {
	console.log('[BSGW] connectedTabIds: ', connections.tabIds);
	if (connections.size === 1) {
		await handleQueryFlowCaseSingleConnectionBecomesMaster(requestingTabId);
	} else {
		await handleQueryFlowCaseFindMaster(requestingTabId);
	}
};

const handlePortMessage = (
	message: MsgNewTabToBGSW,
	port: chrome.runtime.Port
) => {
	(async () => {
		if (!isMsgExpected(message, port?.sender)) return;
		const tabId = port?.sender?.tab?.id as number;
		console.log(
			'[BSGW] handlePortMessage -- data recv from %d: %d',
			tabId,
			message.type
		);
		switch (message.type) {
			case MsgNewTabToBGSWType.QUERY_STATUS_OF_WS: {
				await handleQueryFlow(tabId);
				break;
			}
		}
	})().catch((err) => {
		console.error(
			'[BSGW] Error handling port message %o from port %o (tabId: %s):',
			message,
			port,
			port?.sender?.tab?.id,
			err
		);
	});
};

const handlePortDisconnect = (port: chrome.runtime.Port) => {
	(async () => {
		console.log('[BGSW] Disconnecting port:', port?.sender?.tab?.id);
		port.onDisconnect.removeListener(handlePortDisconnect);
		await connections.remove(port);
		if (port?.sender?.tab?.id === masterWSTabId.get()) {
			await masterWSTabId.set(null);
			if (connections.size >= 1) {
				const newRequestingTabId = connections.tabIds[0];
				if (connections.size === 1) {
					await handleQueryFlowCaseSingleConnectionBecomesMaster(
						newRequestingTabId
					);
				} else if (connections.size > 1) {
					await handleQueryFlowCaseFindMaster(newRequestingTabId);
				}
			}
		}
	})().catch((err) => {
		console.error(
			'[BSGW] Error handling port disconnect %o (tabId: %s):',
			port,
			port?.sender?.tab?.id,
			err
		);
	});
};

const handlePortConnect = (port: chrome.runtime.Port) => {
	(async () => {
		if (
			port.name === 'ws' &&
			port?.sender?.tab?.id &&
			!connections.has(port)
		) {
			console.log('[BSGW] Connecting port:', port.sender.tab.id);
			await connections.add(port);
			if (!port.onMessage.hasListener(handlePortMessage)) {
				port.onMessage.addListener(handlePortMessage);
				port.onDisconnect.addListener(handlePortDisconnect);
			}
		}
	})().catch((err) => {
		console.error(
			'[BSGW] Error handling port connect %o (tabId: %s):',
			port,
			port?.sender?.tab?.id,
			err
		);
	});
	return true;
};

if (!chrome.runtime.onConnect.hasListener(handlePortConnect)) {
	chrome.runtime.onConnect.addListener(handlePortConnect);
}

// Load connectedTabIds from storage, should be run if the BGSW is reloaded
void connections.loadFromStorage();
void masterWSTabId.loadFromStorage();

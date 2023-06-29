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
	singleRequestingPort: chrome.runtime.Port
) => {
	masterWSTabId = singleRequestingPort?.sender?.tab?.id as number;
	console.log('[BSGW] First connection, assuming %d master', masterWSTabId);
	await sendMsgToTab(MsgBGSWToNewTabType.YOU_ARE_MASTER_WS, masterWSTabId);
};

const handleQueryFlowCaseTellOthersTheyAreClients = async (
	portsInvolved: Array<chrome.runtime.Port>
) => {
	const portsToInform = portsInvolved.filter(
		(port) => port?.sender?.tab?.id !== masterWSTabId
	);
	await Promise.all(
		portsToInform.map((port) =>
			sendMsgToTab(
				MsgBGSWToNewTabType.YOU_ARE_CLIENT_WS,
				port?.sender?.tab?.id as number
			)
		)
	);
};

const handleQueryFlowIdentifiedMaster = (masterPort: chrome.runtime.Port) => {
	masterWSTabId = masterPort?.sender?.tab?.id as number;
	console.log('[BSGW] Identified master WS as %d', masterWSTabId);
};

const handleQueryFlowIdentifiedClient = (clientPort: chrome.runtime.Port) => {
	console.log(
		'[BSGW] Identified client to WS as %d',
		clientPort?.sender?.tab?.id as number
	);
};

const handleQueryFlowRequestBecomesMaster = async (
	requestingPort: chrome.runtime.Port
) => {
	masterWSTabId = requestingPort?.sender?.tab?.id as number;
	console.log(
		'[BSGW] No master identified, letting %d be master',
		masterWSTabId
	);
	await sendMsgToTab(MsgBGSWToNewTabType.YOU_ARE_MASTER_WS, masterWSTabId);
};

const handleQueryFlowCaseFindMaster = async (
	requestingPort: chrome.runtime.Port
) => {
	masterWSTabId = null; // Will be set if one identifies as master
	const answeredPorts = await Promise.all(
		Array.from(connectedPorts).map((connectedPort) => {
			return sendMsgToTab(
				MsgBGSWToNewTabType.CONFIRM_IF_MASTER_WS,
				connectedPort?.sender?.tab?.id as number
			).then((response: MsgNewTabToBGSW) => {
				console.log(
					'[BSGW] Got response from %d: %d',
					connectedPort?.sender?.tab?.id,
					response.type
				);
				switch (response.type) {
					case MsgNewTabToBGSWType.IDENTIFY_AS_MASTER_WS:
						handleQueryFlowIdentifiedMaster(connectedPort);
						break;
					case MsgNewTabToBGSWType.IDENTIFY_AS_WS_CLIENT:
						handleQueryFlowIdentifiedClient(connectedPort);
						break;
				}
				return connectedPort;
			});
		})
	);
	// If all of them think they're clients, set the original requestor as master
	if (masterWSTabId === null) {
		await handleQueryFlowRequestBecomesMaster(requestingPort);
	}
	await handleQueryFlowCaseTellOthersTheyAreClients(answeredPorts);
};

const handleQueryFlow = async (requestingPort: chrome.runtime.Port) => {
	// TODO use masterWs to check if it's still connected
	console.log('[BSGW] connectedPorts: ', connectedPorts);
	if (connectedPorts.size === 1) {
		await handleQueryFlowCaseSingleConnectionBecomesMaster(requestingPort);
	} else {
		await handleQueryFlowCaseFindMaster(requestingPort);
	}
};

const handlePortMessage = (
	message: MsgNewTabToBGSW,
	port: chrome.runtime.Port
) => {
	if (!isMsgExpected(message, port?.sender)) return;
	console.log(
		'[BSGW] handlePortMessage -- data recv from %d: %d',
		port?.sender?.tab?.id as number,
		message.type
	);
	switch (message.type) {
		case MsgNewTabToBGSWType.QUERY_STATUS_OF_WS: {
			handleQueryFlow(port).catch((err) => {
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
		if (connectedPorts.size >= 1) {
			const newRequestingPort = Array.from(connectedPorts)[0];
			if (connectedPorts.size === 1) {
				handleQueryFlowCaseSingleConnectionBecomesMaster(
					newRequestingPort
				).catch((err) => {
					console.error(
						'[BSGW] Error handling single tab connection:',
						err
					);
				});
			} else if (connectedPorts.size > 1) {
				handleQueryFlowCaseFindMaster(newRequestingPort).catch(
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

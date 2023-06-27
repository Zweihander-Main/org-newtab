/* eslint-disable no-console */

export enum MsgNewTabToBSGWType {
	QUERY_STATUS_OF_WS = 1,
	IDENTIFY_AS_MASTER_WS = 2,
	IDENTIFY_AS_WS_CLIENT = 3,
}

export enum MsgBGSWToNewTabType {
	CONFIRM_IF_MASTER_WS = 1,
	YOU_ARE_CLIENT_WS = 2,
	YOU_ARE_MASTER_WS = 3,
}

export type MsgNewTabToBGSW = {
	type: MsgNewTabToBSGWType;
};

export type MsgBGSWToNewTab = {
	type: MsgBGSWToNewTabType;
};

const connections: Array<chrome.runtime.Port> = [];

let i = 0;

// Listen for messages from the new tab page
const onMessage = (message: MsgNewTabToBGSW, port: chrome.runtime.Port) => {
	console.log('Data recv on bg end:', message);

	// Broadcast the message to other new tab pages
	connections.forEach((connection) => {
		if (connection !== port && i === 0) {
			connection.postMessage({
				type: MsgBGSWToNewTabType.CONFIRM_IF_MASTER_WS,
			} as MsgBGSWToNewTab);
			i++;
		}
	});
};

// Remove the connection when the new tab page is closed
const onDisconnect = (port: chrome.runtime.Port) => {
	const index = connections.indexOf(port);
	if (index !== -1) {
		connections.splice(index, 1);
	}
};

chrome.runtime.onConnect.addListener((port) => {
	if (typeof port.sender?.tab?.id === 'number') {
		// console.assert(
		// 	port.name === 'ws',
		// 	'Port name: expecting ws, got %s',
		// 	port.name
		// ); // Plasmo dev vs build?
		connections.push(port);
		port.onMessage.addListener(onMessage);
		port.onDisconnect.addListener(onDisconnect);
	} else {
		console.error(
			'Got unexpected data from port:',
			port,
			'With port.sender: ',
			port.sender
		);
	}
});

export {};

/* eslint-disable no-console */

const connections: Array<chrome.runtime.Port> = [];

// Listen for messages from the new tab page
const onMessage = (message: string, port: chrome.runtime.Port) => {
	console.log('Data recv on bg end:', message);

	// Broadcast the message to other new tab pages
	connections.forEach((connection) => {
		if (connection !== port) {
			connection.postMessage('Hello from the background script!');
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

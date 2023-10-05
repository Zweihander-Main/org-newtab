/* eslint-disable no-console */
/* eslint-disable @typescript-eslint/no-base-to-string */
import { Page } from '@playwright/test';
import { test, expect } from './fixture';
import fs from 'fs';
import { OptionCategories } from 'modules/ui/uiSlice';
import WebSocket from 'ws';
import net from 'net';
import { DEFAULT_WEBSOCKET_PORT } from 'lib/constants';

function loadMessagesJson(locale: string): Record<string, Message> {
	const filePath = `locales/${locale}/messages.json`;
	const jsonData = fs.readFileSync(filePath, 'utf8');
	return JSON.parse(jsonData) as Record<string, Message>;
}

const getMessage = (id: string): string => {
	const messages = loadMessagesJson(LOCALE);
	return (messages[id] && messages[id]?.message) || '';
};

export const HOW_LONG_TO_WAIT_FOR_STORAGE = 20000;
export const HOW_LONG_TO_WAIT_FOR_WEBSOCKET = 15000;
export const HOW_LONG_TO_WAIT_FOR_RESPONSE = 20000;
export const RETRIES_FOR_WEBSOCKET = 0;

export const LOCALE = 'en';

export const MASTER_MESSAGE = getMessage('masterRole');
export const CLIENT_MESSAGE = getMessage('clientRole');
export const MATCH_QUERY_LABEL = getMessage('matchQuery');
export const WS_PORT_LABEL = getMessage('wsPort');
export const INITIAL_STATE_RESOLVED = getMessage('storageResolved');
export const CONNECTION_STATUS_OPEN = getMessage('connectionStatusOpen');

export const GET_ITEM_COMMAND = 'getItem';
export const WSS_TEST_TEXT = 'WSS test message';

export const ROLE_LOCATOR = 'websocket-role';
export const ITEM_TEXT_LOCATOR = 'item-text';
export const INITIAL_STATE_LOCATOR = 'initial-state';
export const CONNECTION_STATUS_LOCATOR = 'connection-status';
export const LOADING_BAR_LOCATOR = 'loading-bar';
export const OPTIONS_OPEN_BUTTON_LOCATOR = 'options-open-button';
export const OPTIONS_CLOSE_BUTTON_LOCATOR = 'options-close-button';
export const BEHAVIOR_BUTTON_LOCATOR = 'behavior-button';
export const LAYOUT_BUTTON_LOCATOR = 'layout-button';
export const THEMING_BUTTON_LOCATOR = 'theming-button';
export const DEBUG_BUTTON_LOCATOR = 'debug-button';

type Message = {
	message: string;
	description: string;
};

export const openOptions = async (page: Page) => {
	await test.step('Open menu', async () => {
		await page.emulateMedia({ reducedMotion: 'reduce' });
		const openButton = page.getByTestId(OPTIONS_OPEN_BUTTON_LOCATOR);
		await openButton.click();
	});
};

export const closeOptions = async (page: Page) => {
	await test.step('Close menu', async () => {
		const closeButton = page.getByTestId(OPTIONS_CLOSE_BUTTON_LOCATOR);
		await closeButton.click();
	});
};

export const gotoOptPanel = async (page: Page, panel: OptionCategories) => {
	await test.step(`Go to ${panel} entry`, async () => {
		await openOptions(page);
		const button = page.getByTestId(`${panel.toLowerCase()}-button`);
		await button.click({ force: true });
	});
};

export const roleIs = async (page: Page, role: 'master' | 'client') => {
	await test.step(`Check if the websocket role is ${role}`, async () => {
		await gotoOptPanel(page, 'Debug');
		const roleLocator = page.getByTestId(ROLE_LOCATOR);
		await expect(roleLocator).toContainText(
			role === 'master' ? MASTER_MESSAGE : CLIENT_MESSAGE
		);
		await closeOptions(page);
	});
};

export const storageIsResolved = async (page: Page) => {
	await test.step('Check if the storage is resolved', async () => {
		const initialStateLocator = page.getByTestId(INITIAL_STATE_LOCATOR);
		await gotoOptPanel(page, 'Debug');
		await expect(initialStateLocator).toContainText(
			INITIAL_STATE_RESOLVED,
			{ timeout: HOW_LONG_TO_WAIT_FOR_STORAGE }
		);
		await closeOptions(page);
	});
};

export function startTestWebSocketServer(port: number) {
	const wss = new WebSocket.Server({ port: port });

	wss.on('connection', (ws) => {
		ws.on('message', (message) => {
			console.log('Received message from client: %s', message);
			let resid = -1;
			try {
				const parsed = JSON.parse(message.toString()) as {
					command: string;
					data: string;
					resid: number;
				};
				resid = parsed?.resid || -1;
			} catch {
				console.log('Could not parse message, not JSON.');
			}
			const toSend = JSON.stringify({
				type: 'ITEM',
				data: { ITEM: WSS_TEST_TEXT },
				resid,
			});
			console.log('Sending response', toSend);
			setTimeout(() => {
				ws.send(toSend);
			}, 500);
		});

		ws.on('close', () => {
			console.log('Client disconnected');
		});

		ws.on('error', console.error);
	});
	return wss;
}

export async function isPortInUse(port: number) {
	return new Promise((resolve) => {
		const server = net.createServer();
		server.once('error', (err: Error & { code: string }) => {
			if (err.code === 'EADDRINUSE') {
				resolve(true);
			} else {
				resolve(false);
			}
		});
		server.once('listening', () => {
			server.close();
			resolve(false);
		});
		server.listen(port);
	});
}

export async function pickARandomPort() {
	const port = Math.floor(Math.random() * (55000 - 10000 + 1)) + 10000;
	if (!(await isPortInUse(port)) && port !== DEFAULT_WEBSOCKET_PORT) {
		return port;
	} else {
		return pickARandomPort();
	}
}

export async function openSocketConnection() {
	const port = await pickARandomPort();
	const wss = startTestWebSocketServer(port);
	return { port, wss };
}

export async function setupWebsocketPort(
	conn: Awaited<ReturnType<typeof openSocketConnection>>,
	tab: Page
) {
	await test.step('Setup websocket port', async () => {
		await gotoOptPanel(tab, 'Behavior');
		const portInput = tab.getByLabel(WS_PORT_LABEL);
		await portInput.fill(conn.port.toString());
		await portInput.press('Enter');
		await expect(portInput).toHaveValue(conn.port.toString());
		await closeOptions(tab);
	});
}

export function webSocketURL(
	conn: Awaited<ReturnType<typeof openSocketConnection>>
) {
	return `ws://localhost:${conn.port}/`;
}

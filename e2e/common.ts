/* eslint-disable no-console */
/* eslint-disable @typescript-eslint/no-base-to-string */
import { promises as fs } from 'fs';
import { spawn } from 'child_process';
import { Page } from '@playwright/test';
import { test, expect } from './fixture';
import { OptionCategories } from 'modules/ui/uiSlice';
import WebSocket from 'ws';
import net from 'net';
import tmp from 'tmp';
import { DEFAULT_WEBSOCKET_PORT } from 'lib/constants';
import {
	CLIENT_MESSAGE,
	HOW_LONG_TO_WAIT_FOR_STORAGE,
	INITIAL_STATE_LOCATOR,
	INITIAL_STATE_RESOLVED,
	MASTER_MESSAGE,
	MAX_RETRIES_FOR_EMACS_CONNECTION,
	OPTIONS_CLOSE_BUTTON_LOCATOR,
	OPTIONS_OPEN_BUTTON_LOCATOR,
	ROLE_LOCATOR,
	WSS_TEST_TEXT,
	WS_PORT_LABEL,
} from './constants';

/**
 * Variables
 */

export const baseDir = process.cwd();

/**
 * Navigation
 */

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

/**
 * State assertions
 */

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

/**
 * Network handling
 */

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

export async function setupWebsocketPort(
	conn: Awaited<ReturnType<typeof openSocketConnection>> | { port: number },
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

/**
 * Mock websocket server
 */

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

export async function openSocketConnection() {
	const port = await pickARandomPort();
	const wss = startTestWebSocketServer(port);
	return { port, wss };
}

export function webSocketURL(
	conn: Awaited<ReturnType<typeof openSocketConnection>> | { port: number }
) {
	return `ws://localhost:${conn.port}/`;
}

/**
 *  Running Emacs Process
 */

export const startEmacsProcess = (port: number, dir: string, retries = 0) => {
	let emacs = spawn('emacs', [
		'--batch',
		'--quick',
		'--eval',
		`(setq org-newtab-ws-port ${port})`,
		'-l',
		`${dir}/init.el`,
		'-l',
		`${dir}/setup-mode.el`,
	]);

	/* prin1, princ, print to stdout
	    	message and error both to stderr :| */
	emacs.stdout.on('data', (data) => {
		console.log(`stdout: ${data}`);
	});

	emacs.stderr.on('data', (data) => {
		console.log(`stderr: ${data}`);
	});

	emacs.on('close', (code) => {
		console.log(`emacs running on port ${port} exited with code ${code}`);
		// 255 may occur due to file locks
		if (code === 255 && retries < MAX_RETRIES_FOR_EMACS_CONNECTION) {
			console.log('emacs exited with code 255, retrying');
			emacs = startEmacsProcess(port, dir, retries + 1);
		}
	});

	return emacs;
};

export const setupEmacs = async () => {
	const port = await pickARandomPort();
	tmp.setGracefulCleanup();
	const tmpDir = tmp.dirSync({ unsafeCleanup: true }).name;
	await fs.mkdir(`${tmpDir}/org`);
	await fs.copyFile(
		`${baseDir}/e2e/emacs/setup/init.el`,
		`${tmpDir}/init.el`
	);
	await fs.copyFile(
		`${baseDir}/e2e/emacs/setup/setup-mode.el`,
		`${tmpDir}/setup-mode.el`
	);
	const emacs = startEmacsProcess(port, tmpDir);
	return { port, emacs, tmpDir };
};

export const teardownEmacs = (emacs: ReturnType<typeof startEmacsProcess>) => {
	emacs.kill();
};

export const setupClockLisp = async (file: string, tmpDir: string) => {
	await fs.copyFile(
		`${baseDir}/e2e/emacs/clock/${file}`,
		`${tmpDir}/extra-testing-code.el`
	);
};

export const setupChangeLisp = async (file: string, tmpDir: string) => {
	await fs.copyFile(
		`${baseDir}/e2e/emacs/change/${file}`,
		`${tmpDir}/extra-testing-code.el`
	);
};

export const setupOrgFile = async (
	file: string,
	tmpDir: string,
	tag?: string
) => {
	const origFileContents = await fs.readFile(
		`${baseDir}/e2e/emacs/org/${file}`,
		'utf8'
	);

	const newFileContents = tag
		? origFileContents.replace(/:TOCHANGE/g, tag)
		: origFileContents;

	await fs.writeFile(`${tmpDir}/org/${file}`, newFileContents);
};

/**
 * Misc
 */

export const toRGB = (color: string) => {
	const colorArray = /^#?([a-f\d]{2})([a-f\d]{2})([a-f\d]{2})$/i.exec(color);
	const colorObj = colorArray
		? {
				r: parseInt(colorArray[1], 16),
				g: parseInt(colorArray[2], 16),
				b: parseInt(colorArray[3], 16),
			}
		: null;
	return colorObj ? `rgb(${colorObj.r}, ${colorObj.g}, ${colorObj.b})` : '';
};

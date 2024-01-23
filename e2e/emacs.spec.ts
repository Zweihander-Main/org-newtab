/* eslint-disable no-console */
import { spawn } from 'child_process';
import { test, expect } from './fixture';
import {
	CONNECTION_STATUS_LOCATOR,
	CONNECTION_STATUS_OPEN,
	HOW_LONG_TO_WAIT_FOR_RESPONSE,
	ITEM_TEXT_LOCATOR,
	setupWebsocketPort,
} from './common';

const baseDir = process.cwd();

function emacsProcess() {
	const emacs = spawn('emacs', [
		'--batch',
		'--quick',
		'-l',
		`${baseDir}/e2e/emacs/init.el`,
		'-l',
		`${baseDir}/e2e/emacs/setup-mode.el`,
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
		console.log(`emacs exited with code ${code}`);
	});

	return emacs;
}

test('pulls agenda item', async ({ context, extensionId }) => {
	const emacs = emacsProcess();
	const tabMaster = await context.newPage();
	await tabMaster.goto(`chrome-extension://${extensionId}/newtab.html`);
	await setupWebsocketPort({ port: 35943 }, tabMaster);
	await expect(
		tabMaster.getByTestId(CONNECTION_STATUS_LOCATOR)
	).toContainText(CONNECTION_STATUS_OPEN);
	await expect(tabMaster.getByTestId(ITEM_TEXT_LOCATOR)).toContainText(
		'Sample todo item',
		{ timeout: HOW_LONG_TO_WAIT_FOR_RESPONSE }
	);
	emacs.kill();
});

// TODO: select random port

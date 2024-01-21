import { exec } from 'child_process';
import { test, expect } from '@playwright/test';

async function runEmacs(sexp: string) {
	return new Promise((resolve, reject) => {
		exec(
			[
				'emacs',
				'--batch',
				'-l',
				`${process.cwd()}/lisp/org-newtab-agenda.el`,
				'--eval',
				'"(progn',
				`(setq org-agenda-files (list \\"${process.cwd()}/e2e/emacs/\\"))`,
				`${sexp})"`,
			].join(' '),
			(error, stdout, stderr) => {
				if (error || stderr) {
					reject(error);
				}
				resolve(stdout);
			}
		);
	});
}

test('pulls agenda item', async () => {
	const emacsOut = await runEmacs(
		'(prin1 (org-newtab--get-one-agenda-item \\"TODO=\\\\\\"TODO\\\\\\"\\"))'
	);
	expect(emacsOut).toEqual(
		`(("CATEGORY" . "agenda") ("BLOCKED" . "") ("FILE" . "${process.cwd()}/e2e/emacs/agenda.org") ("PRIORITY" . "B") ("TODO" . "TODO") ("ITEM" . "Sample todo item"))`
	);
});

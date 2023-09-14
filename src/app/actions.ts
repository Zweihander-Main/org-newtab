import { createAction } from '@reduxjs/toolkit';
import { listenerMiddleware } from './middleware';

/** Resets all data in all slices to initial state */
export const resetData = createAction('root/reset');

/** Clear local storage, reload the window, log errors */
listenerMiddleware.startListening({
	actionCreator: resetData,
	effect: () => {
		chrome.storage.local
			.clear()
			.then(() => {
				// Get new tag information
				window.location.reload();
			})
			.catch((err) => {
				console.error(err);
			});
	},
});

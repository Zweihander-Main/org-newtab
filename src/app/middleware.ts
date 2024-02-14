import {
	Middleware,
	UnknownAction,
	createListenerMiddleware,
	isAction,
} from '@reduxjs/toolkit';
import { ENABLE_REDUX_LOGGING } from 'lib/constants';
import type { RootState, AppDispatch, PersistorClass } from './store';
import { flushData, resetData } from './actions';

export const listenerMiddleware = createListenerMiddleware<RootState>();

/**
 * Clear local storage, reload the window, log errors
 */
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

/**
 * Log all actions and the next state when ENABLE_REDUX_LOGGING is true
 */
const loggerMiddleware: Middleware<undefined, RootState, AppDispatch> =
	(store) => (next) => (action) => {
		if (isAction(action)) {
			// eslint-disable-next-line no-console
			console.log(
				'Action:',
				action.type,
				(action as UnknownAction)?.payload
			);
		}
		const result = next(action);
		// eslint-disable-next-line no-console
		console.log('Next state:', store.getState());
		return result;
	};

const getMiddleware = (persistor: PersistorClass) => {
	const middlewares: Array<Middleware> = [listenerMiddleware.middleware];

	if (ENABLE_REDUX_LOGGING) {
		middlewares.push(loggerMiddleware);
	}

	/**
	 * Flush (write to storage) data.
	 */
	const persistorMiddleware: Middleware<undefined, RootState, AppDispatch> =
		() => (next) => (action) => {
			if (flushData.match(action)) {
				void persistor.flush();
			}
			return next(action);
		};

	middlewares.push(persistorMiddleware);

	return middlewares;
};

export default getMiddleware;

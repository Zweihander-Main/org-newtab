import {
	Middleware,
	UnknownAction,
	createListenerMiddleware,
	isAction,
} from '@reduxjs/toolkit';
import { ENABLE_REDUX_LOGGING } from 'lib/constants';
import type { RootState, AppDispatch } from './store';
import { flushData, resetData } from './actions';
import Persistor from 'lib/Persistor';
import { REHYDRATE } from '@plasmohq/redux-persist';

const middlewares: Array<Middleware> = [];

/**
 * Used extensively to manage websocket and background messaging
 */
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

middlewares.push(listenerMiddleware.middleware);

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

if (ENABLE_REDUX_LOGGING) {
	middlewares.push(loggerMiddleware);
}

/**
 * Flush (write to storage) data.
 */
const persistorMiddleware: Middleware<undefined, RootState, AppDispatch> =
	(state) => (next) => (action) => {
		if (flushData.match(action)) {
			void Persistor.flush();
		} else if (
			isAction(action) &&
			action.type === REHYDRATE &&
			Persistor.isFlushing
		) {
			// Ignore rehydration when flushing to ensure no data loss from Emacs
			// Rehydration will automatically happen when flushing is complete
			return state;
		}
		return next(action);
	};

middlewares.push(persistorMiddleware);

export default middlewares;

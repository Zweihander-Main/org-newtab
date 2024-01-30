import { createListenerMiddleware } from '@reduxjs/toolkit';
import { RootState } from './store';

export const listenerMiddleware = createListenerMiddleware<RootState>();

// Log all actions
// listenerMiddleware.startListening({
// 	predicate: () => true,
// 	effect: (action) => {
// 		// eslint-disable-next-line no-console
// 		console.log(action.type, action.payload);
// 	},
// });

export default listenerMiddleware.middleware;

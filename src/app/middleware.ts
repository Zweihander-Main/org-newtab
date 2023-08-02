import { createListenerMiddleware } from '@reduxjs/toolkit';
import { RootState } from './store';

export const listenerMiddleware = createListenerMiddleware<RootState>();

export default listenerMiddleware.middleware;

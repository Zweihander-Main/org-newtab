import { createAction } from '@reduxjs/toolkit';

/** Resets all data in all slices to initial state. */
export const resetData = createAction('root/reset');

/** Flush (write to storage) data. Used in middleware with persistor. */
export const flushData = createAction('root/flush');

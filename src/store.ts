import { configureStore } from '@reduxjs/toolkit';
import { localStorage } from 'redux-persist-webextension-storage';
import {
	FLUSH,
	PAUSE,
	PERSIST,
	persistReducer,
	persistStore,
	PURGE,
	REGISTER,
	REHYDRATE,
	RESYNC,
} from '@plasmohq/redux-persist';
import type { Storage as StorageType } from '@plasmohq/redux-persist/lib/types';
import { Storage } from '@plasmohq/storage';

import rootReducer from './reducers';

const persistConfig = {
	key: 'root',
	version: 1,
	storage: localStorage as StorageType,
};

const persistedReducer = persistReducer(persistConfig, rootReducer);

// Until persistReducer is fixed, we need to use this mock store to get the types
export const mockStore = configureStore({
	reducer: rootReducer,
});

export const store = configureStore({
	reducer: persistedReducer,
	middleware: (getDefaultMiddleware) =>
		getDefaultMiddleware({
			serializableCheck: {
				ignoredActions: [
					FLUSH,
					REHYDRATE,
					PAUSE,
					PERSIST,
					PURGE,
					REGISTER,
					RESYNC,
				],
			},
		}),
});

export const persistor = persistStore(store);

// This is what makes Redux sync properly with multiple pages
new Storage({
	area: 'local',
}).watch({
	[`persist:${persistConfig.key}`]: () => {
		void persistor.resync();
	},
});

export type RootState = ReturnType<typeof mockStore.getState>;

export type AppDispatch = typeof mockStore.dispatch;

export default store;

import { configureStore } from '@reduxjs/toolkit';
import { devToolsEnhancer } from '@redux-devtools/remote';
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

import stateReducer from './stateReducer';
import middleware from './middleware';

const persistConfig = {
	key: 'root',
	version: 1,
	storage: localStorage as StorageType,
	blacklist: ['amMasterWS', 'readyState', 'responsesWaitingFor'],
};

const persistedReducer = persistReducer(persistConfig, stateReducer);

// Until persistReducer is fixed, we need to use this mock store to get the types
export const mockStore = configureStore({
	reducer: stateReducer,
});

const enhancers = [];
if (process.env.NODE_ENV === 'development') {
	enhancers.push(devToolsEnhancer({}));
}

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
		}).prepend(middleware),
	enhancers,
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

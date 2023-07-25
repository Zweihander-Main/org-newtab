import { combineReducers, configureStore } from '@reduxjs/toolkit';
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

import appReducer from './stateReducer';
import middleware from './middleware';

const enhancers = [];
if (process.env.NODE_ENV === 'development') {
	enhancers.push(devToolsEnhancer({}));
}

const appPersistConfig = {
	key: 'app',
	version: 1,
	storage: localStorage as StorageType,
	blacklist: ['amMasterWS', 'readyState', 'responsesWaitingFor'],
};

const persistedAppReducer = persistReducer(appPersistConfig, appReducer);

const rootReducer = combineReducers({
	app: persistedAppReducer,
});

const rootPersistConfig = {
	key: 'root',
	version: 1,
	storage: localStorage as StorageType,
	blacklist: ['app'],
};

const persistedRootReducer = persistReducer(rootPersistConfig, rootReducer);

// Until persistReducer is fixed, we need to use this mock store to get the types
export const mockStore = configureStore({
	reducer: combineReducers({
		app: appReducer,
	}),
});

export const store = configureStore({
	reducer: persistedRootReducer,
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
	[`persist:${rootPersistConfig.key}`]: () => {
		void persistor.resync();
	},
	[`persist:${appPersistConfig.key}`]: () => {
		void persistor.resync();
	},
});

export type RootState = ReturnType<typeof mockStore.getState>;

export type AppDispatch = typeof mockStore.dispatch;

export default store;

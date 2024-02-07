import { configureStore, StoreEnhancer } from '@reduxjs/toolkit';
import { devToolsEnhancer } from '@redux-devtools/remote';
import {
	FLUSH,
	PAUSE,
	PERSIST,
	persistStore,
	PURGE,
	REGISTER,
	REHYDRATE,
	RESYNC,
} from '@plasmohq/redux-persist';
import { Storage, StorageCallbackMap } from '@plasmohq/storage';

import middleware from './middleware';
import persistedRootReducer, {
	mockRootReducer,
	persistKeys,
} from './rootReducer';

const enhancers: Array<StoreEnhancer> = [];
if (process.env.NODE_ENV === 'development') {
	enhancers.push(devToolsEnhancer({}));
}

// Until persistReducer is fixed, we need to use this mock store to get the types
export const mockStore = configureStore({
	reducer: mockRootReducer,
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
	enhancers: (getDefaultEnhancers) => getDefaultEnhancers().concat(enhancers),
});

export const persistor = persistStore(store);

// This is what makes Redux sync properly with multiple pages

const watchKeys = persistKeys.map((key) => `persist:${key}`);

/**
 * Assumption: Items nested past the first level are strings. Therefore shallow
 * comparison is sufficient to determine if the value has changed.
 */
interface ReduxChangeObject extends chrome.storage.StorageChange {
	oldValue?: Record<string, string>;
	newValue?: Record<string, string>;
}

/**
 * Manually confirming values have changed as Firefox and Chrome differ in
 * triggering onChanged events. Firefox triggers it for every setItem call,
 * whereas Chrome/Safari only trigger it when values have changed.
 */
const watchObject = watchKeys.reduce((acc, key) => {
	acc[key] = (change: ReduxChangeObject) => {
		const { oldValue, newValue } = change;
		const updatedKeys = [];
		for (const key in oldValue) {
			if (oldValue[key] !== newValue?.[key]) {
				updatedKeys.push(key);
			}
		}
		for (const key in newValue) {
			if (oldValue?.[key] !== newValue[key]) {
				updatedKeys.push(key);
			}
		}
		if (updatedKeys.length > 0) {
			void persistor.resync();
		}
	};
	return acc;
}, {} as StorageCallbackMap);

new Storage({
	area: 'local',
}).watch(watchObject);

export type RootState = ReturnType<typeof mockStore.getState>;

export type AppDispatch = typeof mockStore.dispatch;

export default store;

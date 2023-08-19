import { configureStore } from '@reduxjs/toolkit';
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
import { Storage } from '@plasmohq/storage';

import middleware from './middleware';
import persistedRootReducer, {
	mockRootReducer,
	persistKeys,
} from './rootReducer';

const enhancers = [];
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
	enhancers,
});

export const persistor = persistStore(store);

// This is what makes Redux sync properly with multiple pages

const watchKeys = persistKeys.map((key) => `persist:${key}`);

const watchObject = watchKeys.reduce(
	(acc, key) => {
		acc[key] = () => {
			void persistor.resync();
		};
		return acc;
	},
	{} as Record<string, () => void>
);

new Storage({
	area: 'local',
}).watch(watchObject);

export type RootState = ReturnType<typeof mockStore.getState>;

export type AppDispatch = typeof mockStore.dispatch;

export default store;

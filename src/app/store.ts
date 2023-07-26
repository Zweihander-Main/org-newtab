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
	emacsPersistConfig,
	mockRootReducer,
	rootPersistConfig,
	wsPersistConfig,
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
new Storage({
	area: 'local',
}).watch({
	[`persist:${rootPersistConfig.key}`]: () => {
		void persistor.resync();
	},
	[`persist:${emacsPersistConfig.key}`]: () => {
		void persistor.resync();
	},
	[`persist:${wsPersistConfig.key}`]: () => {
		void persistor.resync();
	},
});

export type RootState = ReturnType<typeof mockStore.getState>;

export type AppDispatch = typeof mockStore.dispatch;

export default store;

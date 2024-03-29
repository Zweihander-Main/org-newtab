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
import middlewares from './middleware';
import persistedRootReducer, { mockRootReducer } from './rootReducer';
import Persistor from 'lib/Persistor';

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
		}).prepend(middlewares),
	enhancers: (getDefaultEnhancers) => getDefaultEnhancers().concat(enhancers),
});

Persistor.setStore(persistStore(store));

export type RootState = ReturnType<typeof mockStore.getState>;

export type AppDispatch = typeof mockStore.dispatch;

export default store;

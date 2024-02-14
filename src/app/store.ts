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
import getMiddleware from './middleware';
import persistedRootReducer, { mockRootReducer } from './rootReducer';
import type { Persistor } from '@plasmohq/redux-persist/lib/types';

const enhancers: Array<StoreEnhancer> = [];
if (process.env.NODE_ENV === 'development') {
	enhancers.push(devToolsEnhancer({}));
}

// Until persistReducer is fixed, we need to use this mock store to get the types
export const mockStore = configureStore({
	reducer: mockRootReducer,
});

export class PersistorClass {
	#persistedStore: Persistor | undefined = undefined;

	get isDefined() {
		return this.#persistedStore !== undefined;
	}

	get() {
		return this.#persistedStore;
	}

	set(p: Persistor) {
		this.#persistedStore = p;
	}

	async flush() {
		await this.#persistedStore?.flush();
	}

	async resync() {
		await this.#persistedStore?.resync();
	}
}

export const persistor = new PersistorClass();

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
		}).prepend(getMiddleware(persistor)),
	enhancers: (getDefaultEnhancers) => getDefaultEnhancers().concat(enhancers),
});

persistor.set(persistStore(store));

export type RootState = ReturnType<typeof mockStore.getState>;

export type AppDispatch = typeof mockStore.dispatch;

export default store;

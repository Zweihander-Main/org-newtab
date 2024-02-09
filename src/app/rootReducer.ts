import { AnyAction, combineReducers } from '@reduxjs/toolkit';
import { localStorage } from 'redux-persist-webextension-storage';
import { createMigrate, persistReducer } from '@plasmohq/redux-persist';
import type { Storage as StorageType } from '@plasmohq/redux-persist/lib/types';
import wsReducer, {
	WSState,
	name as wsSliceName,
	persistenceBlacklist as wsSlicePersistenceBlacklist,
} from '../modules/ws/wsSlice';
import emacsReducer, {
	EmacsState,
	name as emacsSliceName,
	persistenceBlacklist as emacsSlicePersistenceBlacklist,
	persistenceVersion as emacsPersistenceVersion,
	persistenceMigrations as emacsPersistenceMigrations,
} from '../modules/emacs/emacsSlice';
import uiReducer, {
	UIState,
	name as uiSliceName,
	persistenceBlacklist as uiSlicePersistenceBlacklist,
} from '../modules/ui/uiSlice';
import layoutReducer, {
	LayoutState,
	name as layoutSliceName,
	persistenceBlacklist as layoutSlicePersistenceBlacklist,
} from '../modules/layout/layoutSlice';
import roleReducer, { name as roleSliceName } from '../modules/role/roleSlice';
import msgReducer, { name as msgSliceName } from '../modules/msg/msgSlice';

export const wsPersistConfig = {
	key: wsSliceName,
	version: 1,
	storage: localStorage as StorageType,
	blacklist: wsSlicePersistenceBlacklist,
};

const persistedWSReducer = persistReducer<WSState, AnyAction>(
	wsPersistConfig,
	wsReducer
);

export const emacsPersistConfig = {
	key: emacsSliceName,
	version: emacsPersistenceVersion,
	storage: localStorage as StorageType,
	blacklist: emacsSlicePersistenceBlacklist,
	migrate: createMigrate(emacsPersistenceMigrations),
};

const persistedEmacsReducer = persistReducer<EmacsState, AnyAction>(
	emacsPersistConfig,
	emacsReducer
);

export const layoutPersistConfig = {
	key: layoutSliceName,
	version: 1,
	storage: localStorage as StorageType,
	blacklist: layoutSlicePersistenceBlacklist,
};

const persistedLayoutReducer = persistReducer<LayoutState, AnyAction>(
	layoutPersistConfig,
	layoutReducer
);

export const uiPersistConfig = {
	key: uiSliceName,
	version: 1,
	storage: localStorage as StorageType,
	blacklist: uiSlicePersistenceBlacklist,
};

const persistedUiReducer = persistReducer<UIState, AnyAction>(
	uiPersistConfig,
	uiReducer
);

const rootReducer = combineReducers({
	[msgSliceName]: msgReducer,
	[roleSliceName]: roleReducer,
	[wsSliceName]: persistedWSReducer,
	[emacsSliceName]: persistedEmacsReducer,
	[layoutSliceName]: persistedLayoutReducer,
	[uiSliceName]: persistedUiReducer,
});

export const rootPersistConfig = {
	key: 'root',
	version: 1,
	storage: localStorage as StorageType,
	blacklist: [
		msgSliceName,
		roleSliceName,
		wsSliceName,
		emacsSliceName,
		uiSliceName,
		layoutSliceName,
	],
};

const persistedRootReducer = persistReducer(rootPersistConfig, rootReducer);

export const persistKeys = [
	rootPersistConfig.key,
	wsPersistConfig.key,
	emacsPersistConfig.key,
	uiPersistConfig.key,
	layoutPersistConfig.key,
];

export const mockRootReducer = combineReducers({
	[msgSliceName]: msgReducer,
	[roleSliceName]: roleReducer,
	[wsSliceName]: wsReducer,
	[emacsSliceName]: emacsReducer,
	[uiSliceName]: uiReducer,
	[layoutSliceName]: layoutReducer,
});

export default persistedRootReducer;

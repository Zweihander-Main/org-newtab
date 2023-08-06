import { AnyAction, combineReducers } from '@reduxjs/toolkit';
import { localStorage } from 'redux-persist-webextension-storage';
import { persistReducer } from '@plasmohq/redux-persist';
import type { Storage as StorageType } from '@plasmohq/redux-persist/lib/types';
import autoMergeLevel2 from '@plasmohq/redux-persist/lib/stateReconciler/autoMergeLevel2';
import wsReducer, {
	WSState,
	name as wsSliceName,
	persistenceBlacklist as wsSlicePersistenceBlacklist,
} from '../modules/ws/wsSlice';
import emacsReducer, {
	EmacsState,
	name as emacsSliceName,
	persistenceBlacklist as emacsSlicePersistenceBlacklist,
} from '../modules/emacs/emacsSlice';
import roleReducer, {
	RoleState,
	name as roleSliceName,
	persistenceBlacklist as roleSlicePersistenceBlacklist,
} from '../modules/role/roleSlice';

// TODO: Reduce persistence to only what is necessary

export const rolePersistConfig = {
	key: roleSliceName,
	version: 1,
	storage: localStorage as StorageType,
	blacklist: roleSlicePersistenceBlacklist,
};

const persistedRoleReducer = persistReducer<RoleState, AnyAction>(
	rolePersistConfig,
	roleReducer
);

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
	version: 1,
	storage: localStorage as StorageType,
	blacklist: emacsSlicePersistenceBlacklist,
	stateReconciler: autoMergeLevel2,
};

const persistedEmacsReducer = persistReducer<EmacsState, AnyAction>(
	emacsPersistConfig,
	emacsReducer
);

const rootReducer = combineReducers({
	role: persistedRoleReducer,
	ws: persistedWSReducer,
	emacs: persistedEmacsReducer,
});

export const rootPersistConfig = {
	key: 'root',
	version: 1,
	storage: localStorage as StorageType,
	blacklist: ['role', 'ws', 'emacs'],
};

const persistedRootReducer = persistReducer(rootPersistConfig, rootReducer);

export const mockRootReducer = combineReducers({
	role: roleReducer,
	ws: wsReducer,
	emacs: emacsReducer,
});

export default persistedRootReducer;

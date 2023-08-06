import { AnyAction, combineReducers } from '@reduxjs/toolkit';
import { localStorage } from 'redux-persist-webextension-storage';
import { persistReducer } from '@plasmohq/redux-persist';
import type { Storage as StorageType } from '@plasmohq/redux-persist/lib/types';
import autoMergeLevel2 from '@plasmohq/redux-persist/lib/stateReconciler/autoMergeLevel2';
import wsReducer, { WSState } from '../modules/ws/wsSlice';
import emacsReducer, { EmacsState } from '../modules/emacs/emacsSlice';
import roleReducer, { RoleState } from '../modules/role/roleSlice';

// TODO: DRY this stuff

export const rolePersistConfig = {
	key: 'role',
	version: 1,
	storage: localStorage as StorageType,
	blacklist: ['amMasterRole', 'stateResolved'],
};

const persistedRoleReducer = persistReducer<RoleState, AnyAction>(
	rolePersistConfig,
	roleReducer
);

export const wsPersistConfig = {
	key: 'ws',
	version: 1,
	storage: localStorage as StorageType,
	blacklist: ['readyState', 'responsesWaitingFor'],
};

const persistedWSReducer = persistReducer<WSState, AnyAction>(
	wsPersistConfig,
	wsReducer
);

export const emacsPersistConfig = {
	key: 'emacs',
	version: 1,
	storage: localStorage as StorageType,
	blacklist: [],
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

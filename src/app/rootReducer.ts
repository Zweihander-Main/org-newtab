import { combineReducers } from '@reduxjs/toolkit';
import { localStorage } from 'redux-persist-webextension-storage';
import { persistReducer } from '@plasmohq/redux-persist';
import type { Storage as StorageType } from '@plasmohq/redux-persist/lib/types';

import wsReducer from '../modules/ws/wsSlice';
import emacsReducer from '../modules/emacs/emacsSlice';
import roleReducer from '../modules/role/roleSlice';

export const rolePersistConfig = {
	key: 'role',
	version: 1,
	storage: localStorage as StorageType,
	blacklist: ['amMasterWS'],
};

const persistedRoleReducer = persistReducer(rolePersistConfig, roleReducer);

export const wsPersistConfig = {
	key: 'ws',
	version: 1,
	storage: localStorage as StorageType,
	blacklist: ['readyState', 'responsesWaitingFor'],
};

const persistedWSReducer = persistReducer(wsPersistConfig, wsReducer);

export const emacsPersistConfig = {
	key: 'emacs',
	version: 1,
	storage: localStorage as StorageType,
	blacklist: [],
};

const persistedEmacsReducer = persistReducer(emacsPersistConfig, emacsReducer);

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

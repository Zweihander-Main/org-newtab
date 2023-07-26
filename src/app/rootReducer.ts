import { combineReducers } from '@reduxjs/toolkit';
import { localStorage } from 'redux-persist-webextension-storage';
import { persistReducer } from '@plasmohq/redux-persist';
import type { Storage as StorageType } from '@plasmohq/redux-persist/lib/types';

import wsReducer from '../modules/ws/wsSlice';
import emacsReducer from '../modules/emacs/emacsSlice';

export const wsPersistConfig = {
	key: 'ws',
	version: 1,
	storage: localStorage as StorageType,
	blacklist: ['amMasterWS', 'readyState', 'responsesWaitingFor'],
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
	ws: persistedWSReducer,
	emacs: persistedEmacsReducer,
});

export const rootPersistConfig = {
	key: 'root',
	version: 1,
	storage: localStorage as StorageType,
	blacklist: ['ws', 'emacs'],
};

const persistedRootReducer = persistReducer(rootPersistConfig, rootReducer);

export const mockRootReducer = combineReducers({
	ws: wsReducer,
	emacs: emacsReducer,
});

export default persistedRootReducer;

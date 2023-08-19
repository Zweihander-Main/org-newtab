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
import layoutReducer, {
	LayoutState,
	name as layoutSliceName,
	persistenceBlacklist as layoutSlicePersistenceBlacklist,
} from '../modules/layout/layoutSlice';
import roleReducer, { name as roleSliceName } from '../modules/role/roleSlice';
import msgReducer, { name as msgSliceName } from '../modules/msg/msgSlice';
import uiReducer, { name as uiSliceName } from '../modules/ui/uiSlice';

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

export const layoutPersistConfig = {
	key: layoutSliceName,
	version: 1,
	storage: localStorage as StorageType,
	blacklist: layoutSlicePersistenceBlacklist,
	stateReconciler: autoMergeLevel2,
};

const persistedLayoutReducer = persistReducer<LayoutState, AnyAction>(
	layoutPersistConfig,
	layoutReducer
);

const rootReducer = combineReducers({
	[msgSliceName]: msgReducer,
	[roleSliceName]: roleReducer,
	[wsSliceName]: persistedWSReducer,
	[emacsSliceName]: persistedEmacsReducer,
	[layoutSliceName]: persistedLayoutReducer,
	[uiSliceName]: uiReducer,
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

export const mockRootReducer = combineReducers({
	[msgSliceName]: msgReducer,
	[roleSliceName]: roleReducer,
	[wsSliceName]: wsReducer,
	[emacsSliceName]: emacsReducer,
	[uiSliceName]: uiReducer,
	[layoutSliceName]: layoutReducer,
});

export default persistedRootReducer;

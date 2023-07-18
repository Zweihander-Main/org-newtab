import { createChromeStorageStateHookLocal } from 'use-chrome-storage';
import { ReadyState } from 'react-use-websocket';
import type { EmacsItemMsg } from '../lib/types';

const STATE_KEY = 'appState';
const INITIAL_VALUE = {
	matchQuery: 'TODO="TODO"',
	tagsData: {},
	orgItem: null,
	readyState: ReadyState.UNINSTANTIATED,
};

export interface AppState {
	matchQuery: string | undefined;
	tagsData: { [key: string]: string | null };
	orgItem: EmacsItemMsg['data'] | null;
	readyState: ReadyState;
}

const useAppState = createChromeStorageStateHookLocal<AppState>(
	STATE_KEY,
	INITIAL_VALUE
);

export default useAppState;

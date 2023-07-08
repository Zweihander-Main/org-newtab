import { createChromeStorageStateHookLocal } from 'use-chrome-storage';
import { ReadyState } from 'react-use-websocket';
import type { EmacsItemMsg } from '../util/types';

const STATE_KEY = 'appState';
const INITIAL_VALUE = {
	matchQuery: 'TODO="TODO"',
	tagsData: {},
	orgItem: null,
	readyState: ReadyState.UNINSTANTIATED,
};

export interface AppState {
	matchQuery: string | undefined;
	tagsData: { [key: string]: string };
	orgItem: EmacsItemMsg['data'] | null;
	readyState: ReadyState;
}

const useAppState = createChromeStorageStateHookLocal<AppState>(
	STATE_KEY,
	INITIAL_VALUE
);

export default useAppState;

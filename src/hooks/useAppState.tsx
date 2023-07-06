import { createChromeStorageStateHookLocal } from 'use-chrome-storage';
import { ReadyState } from 'react-use-websocket';

const STATE_KEY = 'appState';
const INITIAL_VALUE = {
	matchQuery: undefined,
	tagsData: {},
	itemText: null,
	readyState: ReadyState.UNINSTANTIATED,
};

export interface AppState {
	matchQuery: string | undefined;
	tagsData: { [key: string]: string };
	itemText: string | null;
	readyState: ReadyState;
}

const useAppState = createChromeStorageStateHookLocal<AppState>(
	STATE_KEY,
	INITIAL_VALUE
);

export default useAppState;

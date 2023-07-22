import type { EmacsItemMsg } from '../lib/types';
import { createContext, useCallback, useEffect, useState } from 'react';

export interface AppState {
	matchQuery: string | undefined;
	tagsData: { [key: string]: string | null };
	orgItem: EmacsItemMsg['data'] | null;
}

export const STATE_KEY = 'appState';

export const storage = {
	get: (defaultValue?: typeof INITIAL_VALUE) => {
		const keyObj = { [STATE_KEY]: defaultValue };
		return new Promise<AppState>((resolve, reject) => {
			chrome.storage.local.get(keyObj, (items) => {
				const error = chrome.runtime.lastError;
				if (error) return reject(error);
				resolve(items[STATE_KEY] as AppState);
			});
		});
	},
	set: (value: AppState) => {
		return new Promise<void>((resolve, reject) => {
			chrome.storage.local.set({ [STATE_KEY]: value }, () => {
				const error = chrome.runtime.lastError;
				error ? reject(error) : resolve();
			});
		});
	},
};

export type StateContextProps = {
	state: AppState;
	setState: (
		newValue: AppState | ((prevValue: AppState) => AppState)
	) => void;
	isInitialStateResolved: boolean;
};

export const INITIAL_VALUE: AppState = {
	matchQuery: 'TODO="TODO"',
	tagsData: {},
	orgItem: null,
};

const StateContext = createContext<StateContextProps>({
	state: INITIAL_VALUE,
	setState: () => {},
	isInitialStateResolved: false,
});

export default StateContext;

export const StateProvider: React.FC<{ children?: React.ReactNode }> = ({
	children,
}) => {
	const [cachedState, setCachedState] = useState<AppState>(INITIAL_VALUE);
	const [error, setError] = useState<undefined | string>(undefined);
	const [isInitialStateResolved, setIsInitialStateResolved] = useState(false);

	useEffect(() => {
		storage
			.get(INITIAL_VALUE)
			.then((res) => {
				// TODO: don't set if the same
				setCachedState(res);
				setError(undefined);
				setIsInitialStateResolved(true);
			})
			.catch((error: chrome.runtime.LastError) => {
				setError(error.message);
				setIsInitialStateResolved(true);
			});
	}, []);

	// NEXT: cache the state more aggressively, updates which are the same shouldn't be saved

	const setState: StateContextProps['setState'] = useCallback((newValue) => {
		// const toStore =
		// 	typeof newValue === 'function'
		// 		? newValue(cachedState)
		// 		: newValue;
		// TODO: no way to bail out of newValue function set
		// if (isEqual(newValue, cachedValue)) return;
		setCachedState(newValue);
		storage
			.set(newValue as AppState)
			.catch((error: chrome.runtime.LastError) => {
				setError(error.message);
			});
	}, []);

	useEffect(() => {
		if (error && error !== '') {
			console.error('Error setting storage', error);
		}
	}, [error]);

	useEffect(() => {
		const onChange: Parameters<
			typeof chrome.storage.onChanged.addListener
		>[0] = (changes, areaName) => {
			if (areaName === 'local' && STATE_KEY in changes) {
				const { newValue /**,prevValue**/ } = changes[STATE_KEY]
					.newValue as {
					newValue: AppState;
					prevValue: AppState;
				};
				// if (!JSON.stringify(newValue) !== JSON.stringify(prevValue))) {
				setCachedState(newValue);
				setError(undefined);
				// }
			}
		};
		if (!chrome.storage.onChanged.hasListener(onChange)) {
			chrome.storage.onChanged.addListener(onChange);
		}
		return () => {
			chrome.storage.onChanged.removeListener(onChange);
		};
	}, []);

	return (
		<StateContext.Provider
			value={{ state: cachedState, setState, isInitialStateResolved }}
		>
			{children}
		</StateContext.Provider>
	);
};

export const { Consumer: StateConsumer } = StateContext;

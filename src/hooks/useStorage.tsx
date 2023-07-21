import { ReadyState } from 'react-use-websocket';
import type { EmacsItemMsg } from '../lib/types';
import { useCallback, useEffect, useState } from 'react';

export interface AppState {
	matchQuery: string | undefined;
	tagsData: { [key: string]: string | null };
	orgItem: EmacsItemMsg['data'] | null;
	readyState: ReadyState;
	isWaitingForResponse: boolean;
}

const INITIAL_VALUE: AppState = {
	matchQuery: 'TODO="TODO"',
	tagsData: {},
	orgItem: null,
	readyState: ReadyState.UNINSTANTIATED,
	isWaitingForResponse: false,
};

export const storage = {
	get: <T extends keyof AppState>(
		key: T,
		defaultValue?: (typeof INITIAL_VALUE)[T]
	) => {
		const keyObj =
			defaultValue === undefined ? key : { [key]: defaultValue };
		return new Promise<AppState[T]>((resolve, reject) => {
			chrome.storage.local.get(keyObj, (items) => {
				const error = chrome.runtime.lastError;
				if (error) return reject(error);
				resolve(items[key] as AppState[T]);
			});
		});
	},
	set: <T extends keyof AppState>(key: T, value: AppState[T]) => {
		return new Promise<void>((resolve, reject) => {
			chrome.storage.local.set({ [key]: value }, () => {
				const error = chrome.runtime.lastError;
				error ? reject(error) : resolve();
			});
		});
	},
};

const useStorage = <T extends keyof AppState>(key: T) => {
	const [cachedValue, setCachedValue] = useState<AppState[T]>(
		INITIAL_VALUE[key]
	);
	const [isPersistent, setIsPersistent] = useState(true);
	const [error, setError] = useState<undefined | string>(undefined);
	const [isInitialStateResolved, setIsInitialStateResolved] = useState(false);

	useEffect(() => {
		storage
			.get(key, INITIAL_VALUE[key])
			.then((res) => {
				setCachedValue(res);
				setIsPersistent(true);
				setError(undefined);
				setIsInitialStateResolved(true);
			})
			.catch((error: chrome.runtime.LastError) => {
				setIsPersistent(false);
				setError(error.message);
				setIsInitialStateResolved(true);
			});
	}, [key]);

	const setValue = useCallback(
		(newValue: AppState[T]) => {
			setCachedValue(newValue);
			storage
				.set(key, newValue)
				.catch((error: chrome.runtime.LastError) => {
					setIsPersistent(false);
					setError(error.message);
				});
		},
		[key]
	);

	useEffect(() => {
		const onChange: Parameters<
			typeof chrome.storage.onChanged.addListener
		>[0] = (changes, areaName) => {
			if (areaName === 'local' && key in changes) {
				setCachedValue(changes[key].newValue as AppState[T]);
				setIsPersistent(true);
				setError(undefined);
			}
		};
		chrome.storage.onChanged.addListener(onChange);
		return () => {
			chrome.storage.onChanged.removeListener(onChange);
		};
	}, [key]);

	return {
		value: cachedValue,
		setValue,
		isPersistent,
		error,
		isInitialStateResolved,
	};
};

export default useStorage;

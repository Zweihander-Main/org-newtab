import { useCallback, useEffect, useState } from 'react';
import useAppState, { type AppState } from './useAppState';

/**
 * @param key which key to query
 * @returns {value, setValue, isPersistent, isInitialStateResolved}
 */
const useValue = <T extends keyof AppState>(
	key: T
): {
	value: AppState[T];
	setValue: (newValue: AppState[T]) => void;
	isPersistent: boolean;
	isInitialStateResolved: boolean;
} => {
	const [appState, setAppState, isPersistent, error, isInitialStateResolved] =
		useAppState();

	// NEXT: create a state variable about what sort of value it is if possible
	// NEXT: Write test for useValue, mock the storage and confirm it's being hit how many times

	const [value, setValue] = useState(appState[key]);

	if (
		(typeof value === 'object' &&
			JSON.stringify(value) !== JSON.stringify(appState[key])) ||
		(typeof value !== 'object' && value !== appState[key])
	) {
		setValue(appState[key]);
	}

	const [isPersistentValue, setIsPersistentValue] = useState(isPersistent);

	if (isPersistentValue !== isPersistent) {
		setIsPersistentValue(isPersistent);
	}

	const [isInitialStateResolvedValue, setIsInitialStateResolvedValue] =
		useState(isInitialStateResolved);

	if (isInitialStateResolvedValue !== isInitialStateResolved) {
		setIsInitialStateResolvedValue(isInitialStateResolved);
	}

	/**
	 * Note that the imported library will always call set.storage if you call
	 * the set function. There's no returning null path here.
	 */
	const setStorageValue = useCallback(
		(newValue: AppState[T]) => {
			if (
				typeof newValue === 'object' &&
				JSON.stringify(newValue) === JSON.stringify(value)
			) {
				return;
			} else if (typeof newValue !== 'object' && value === newValue) {
				return;
			}
			setAppState((prevState) => {
				return { ...prevState, [key]: newValue };
			});
		},
		[key, setAppState, value]
	);

	useEffect(() => {
		if (error && error !== '') {
			console.error('Error setting storage for', key, error);
		}
	}, [error, key]);

	return {
		value,
		setValue: setStorageValue,
		isPersistent: isPersistentValue,
		isInitialStateResolved: isInitialStateResolvedValue,
	};
};

export default useValue;

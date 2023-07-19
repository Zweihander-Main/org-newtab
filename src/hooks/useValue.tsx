import { useCallback, useEffect, useMemo } from 'react';
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

	const value = useMemo(() => appState[key], [appState, key]);
	/**
	 * Note that the imported library will always call set.storage if you call
	 * the set function. There's no returning null path here.
	 */
	const setValue = useCallback(
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

	return { value, setValue, isPersistent, isInitialStateResolved };
};

export default useValue;

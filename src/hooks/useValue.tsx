import { useCallback, useMemo } from 'react';
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
	const [appState, setAppState, isPersistent, , isInitialStateResolved] =
		useAppState();

	const value = useMemo(() => appState[key], [appState, key]);
	const setValue = useCallback(
		(newValue: AppState[T]) => {
			setAppState((prevState) => {
				return { ...prevState, [key]: newValue };
			});
		},
		[key, setAppState]
	);

	return { value, setValue, isPersistent, isInitialStateResolved };
};

export default useValue;

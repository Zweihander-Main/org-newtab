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

	const value = appState[key];
	const setValue = (newValue: AppState[T]) => {
		setAppState((prevState) => {
			return { ...prevState, [key]: newValue };
		});
	};

	return { value, setValue, isPersistent, isInitialStateResolved };
};

export default useValue;

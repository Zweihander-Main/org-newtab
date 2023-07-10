import useAppState, { type AppState } from './useAppState';

/**
 *
 * @param key which key to query
 * @returns [value, setValue, isPersistent, isInitialStateResolved]
 */
const useValue = <T extends keyof AppState>(
	key: T
): [AppState[T], (newValue: AppState[T]) => void, boolean, boolean] => {
	const [appState, setAppState, isPersistent, , isInitialStateResolved] =
		useAppState();

	const curValue = appState[key];
	const setNewValue = (newValue: AppState[T]) => {
		setAppState((prevState) => {
			return { ...prevState, [key]: newValue };
		});
	};

	return [curValue, setNewValue, isPersistent, isInitialStateResolved];
};

export default useValue;

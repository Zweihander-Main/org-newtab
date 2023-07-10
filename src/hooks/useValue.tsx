import useAppState, { type AppState } from './useAppState';

const useValue = <T extends keyof AppState>(
	key: T
): [AppState[T], (newValue: AppState[T]) => void, boolean] => {
	const [appState, setAppState, , , isInitialStateResolved] = useAppState();

	const curValue = appState[key];
	const setNewValue = (newValue: AppState[T]) => {
		setAppState((prevState) => {
			return { ...prevState, [key]: newValue };
		});
	};

	return [curValue, setNewValue, isInitialStateResolved];
};

export default useValue;

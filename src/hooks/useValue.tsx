import { useCallback, useContext } from 'react';
import StateContext, { AppState } from '../contexts/state';

const isEqual = <T extends keyof AppState>(a: AppState[T], b: AppState[T]) => {
	if (typeof a === 'object') {
		return JSON.stringify(a) === JSON.stringify(b);
	}
	return a === b;
};

/**
 * @param key which key to query
 * @returns {value, setValue, isInitialStateResolved}
 */
const useValue = <T extends keyof AppState>(key: T) => {
	const { state, setState } = useContext(StateContext);

	const value = state[key];

	const setValue = useCallback(
		(newValue: AppState[T]) => {
			if (isEqual(newValue, value)) return;
			setState((prevValue) => ({ ...prevValue, [key]: newValue }));
		},
		[key, setState, value]
	);

	return {
		value,
		setValue,
	};
};

export default useValue;

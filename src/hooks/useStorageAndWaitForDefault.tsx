import type { BaseStorage } from '@plasmohq/storage';
import { useCallback, useEffect, useRef, useState } from 'react';

/**
 * Simplified storage hook that returns undefined until a value is retrieved
 * from storage.
 *
 * @param key Key in storage to use
 * @param defaultValue Default value to use if key is not set
 * @returns
 */

const useStorageAndWaitForDefault = (
	key: string,
	storageRef: React.MutableRefObject<BaseStorage>,
	defaultValue?: string
) => {
	const isInitialRender = useRef(true);
	const [renderValue, setRenderValue] = useState<string | undefined>(
		undefined
	);

	const getStoreValue = useCallback(async () => {
		const value = await storageRef.current.get(key);
		if (value === undefined) {
			return defaultValue;
		}
		return value;
	}, [defaultValue, key, storageRef]);

	useEffect(() => {
		if (isInitialRender.current) {
			isInitialRender.current = false;
			getStoreValue()
				.then((value) => {
					setRenderValue(value);
				})
				.catch((err) => {
					console.error(
						'[NewTab] Error in useStorage hook getting initial value',
						err
					);
				});
		}
	}, [getStoreValue]);

	const setStoreValue = useCallback(
		(newValue?: string) => {
			setRenderValue(newValue);
			storageRef.current.set(key, newValue).catch((err) => {
				console.error(
					'[NewTab] Error in useStorage hook setting value',
					err
				);
			});
		},
		[key, storageRef]
	);

	return [renderValue, setStoreValue] as const;
};

export default useStorageAndWaitForDefault;

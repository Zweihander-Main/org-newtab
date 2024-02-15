import { Storage, StorageCallbackMap } from '@plasmohq/storage';
import { persistKeys } from './rootReducer';
import { persistor } from './store';
import { ENABLE_STORAGE_LOGGING } from 'lib/constants';

// This is what makes Redux sync properly with multiple pages
const watchKeys = persistKeys.map((key) => `persist:${key}`);

/**
 * Assumption: Items nested past the first level are strings. Therefore shallow
 * comparison is sufficient to determine if the value has changed.
 */
interface ReduxChangeObject extends chrome.storage.StorageChange {
	oldValue?: Record<string, string>;
	newValue?: Record<string, string>;
}

/**
 * Manually confirming values have changed as Firefox and Chrome differ in
 * triggering onChanged events. Firefox triggers it for every setItem call,
 * whereas Chrome/Safari only trigger it when values have changed.
 */
const watchObj = watchKeys.reduce((acc, key) => {
	acc[key] = (change: ReduxChangeObject) => {
		const { oldValue, newValue } = change;
		const updatedKeys = [];
		for (const key in oldValue) {
			if (oldValue[key] !== newValue?.[key]) {
				updatedKeys.push(key);
			}
		}
		for (const key in newValue) {
			if (oldValue?.[key] !== newValue[key]) {
				updatedKeys.push(key);
			}
		}
		if (updatedKeys.length > 0 && persistor) {
			void persistor.resync();
			if (ENABLE_STORAGE_LOGGING) {
				// eslint-disable-next-line no-console
				console.log(
					'Storage keys updated:',
					updatedKeys,
					'oldValue:',
					oldValue,
					'newValue:',
					newValue
				);
			}
		}
	};
	return acc;
}, {} as StorageCallbackMap);

new Storage({
	area: 'local',
}).watch(watchObj);

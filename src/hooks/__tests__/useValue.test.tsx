import { renderHook, waitFor } from '@testing-library/react';
import useValue from '../useValue';

describe('useValue', () => {
	const { result } = renderHook(() => useValue('readyState'));

	it('should ask storage for result', async () => {
		expect(result.current.value).toBe(-1);
		await waitFor(() => {
			expect(chrome.storage.local.get).toHaveBeenCalled();
		});
	});
	// it('should set storage', async () => {
	// 	result.current.setValue(1);
	// 	expect(chrome.storage.local.set).toHaveBeenCalled();
	// });
});

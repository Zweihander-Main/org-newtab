import { renderHook } from '@testing-library/react';
import useStorage from '../useStorage';

describe('useValue', () => {
	const { result } = renderHook(() => useStorage('readyState'));

	it('should use initial value', () => {
		expect(result.current.value).toBe(-1);
	});
	it('should ask storage for initial value', () => {
		expect(chrome.storage.local.get).toHaveBeenCalledWith(
			{
				readyState: -1,
			},
			expect.any(Function)
		);
	});
});

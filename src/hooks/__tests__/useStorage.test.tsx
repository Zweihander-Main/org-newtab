import { act, renderHook, waitFor } from '@testing-library/react';
import useStorage from '../useStorage';

describe('useValue', () => {
	afterEach(async () => {
		await chrome.storage.local.clear();
		jest.clearAllMocks();
	});

	it('should use initial value', async () => {
		const { result } = renderHook(() => useStorage('readyState'));
		await waitFor(() => {
			expect(result.current.value).toBe(-1);
		});
	});

	it('should ask storage for initial value', async () => {
		renderHook(() => useStorage('readyState'));
		await waitFor(() => {
			expect(chrome.storage.local.get).toHaveBeenCalledWith(
				{
					readyState: -1,
				},
				expect.any(Function)
			);
		});
	});

	it('should set value', async () => {
		const { result } = renderHook(() => useStorage('readyState'));
		act(() => {
			result.current.setValue(1);
		});
		await waitFor(() => {
			expect(result.current.value).toBe(1);
		});
	});

	it('should set value in storage', async () => {
		const { result } = renderHook(() => useStorage('readyState'));
		act(() => {
			result.current.setValue(2);
		});
		await waitFor(() => {
			expect(chrome.storage.local.set).toHaveBeenCalledWith(
				{
					readyState: 2,
				},
				expect.any(Function)
			);
		});
	});
});

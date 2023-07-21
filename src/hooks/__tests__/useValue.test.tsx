import { act, renderHook, waitFor } from '@testing-library/react';
import useValue from '../useValue';

describe('useValue', () => {
	afterEach(async () => {
		await chrome.storage.local.clear();
		jest.clearAllMocks();
	});

	it('should use initial value', async () => {
		const { result } = renderHook(() => useValue('readyState'));
		await waitFor(() => {
			expect(result.current.value).toBe(-1);
		});
	});

	it('should ask storage for initial value', async () => {
		renderHook(() => useValue('readyState'));
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
		const { result } = renderHook(() => useValue('readyState'));
		act(() => {
			result.current.setValue(1);
		});
		await waitFor(() => {
			expect(result.current.value).toBe(1);
		});
	});

	it('should set value in storage', async () => {
		const { result } = renderHook(() => useValue('readyState'));
		act(() => {
			result.current.setValue(1);
		});
		await waitFor(() => {
			expect(chrome.storage.local.set).toHaveBeenCalledWith(
				{
					readyState: 1,
				},
				expect.any(Function)
			);
		});
	});

	it('should subscribe to storage listener', async () => {
		expect(chrome.storage.onChanged.addListener).not.toHaveBeenCalled();
		renderHook(() => useValue('readyState'));
		await waitFor(() => {
			expect(chrome.storage.onChanged.addListener).toHaveBeenCalled();
		});
	});
});

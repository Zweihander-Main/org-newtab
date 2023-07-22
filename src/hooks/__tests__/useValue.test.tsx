import { renderHook, waitFor } from '@testing-library/react';
import useValue from '../useValue';

describe('useValue', () => {
	it('should use initial value', async () => {
		const { result } = renderHook(() => useValue('readyState'));
		await waitFor(() => {
			expect(result.current.value).toBe(-1);
		});
	});
});

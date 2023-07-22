import { renderHook, waitFor } from '@testing-library/react';
import useValue from '../useValue';

describe('useValue', () => {
	it('should use initial value', async () => {
		const { result } = renderHook(() => useValue('matchQuery'));
		await waitFor(() => {
			expect(result.current.value).toBe('TODO="TODO"');
		});
	});
});

import { renderHook } from '@testing-library/react';
import useSingleWebSocket from '../useSingleWS';

describe('AppContext', () => {
	const { result } = renderHook(() => useSingleWebSocket());

	it('should return readyState of -1', () => {
		expect(result.current.amMasterWS).toBe(false);
	});
});

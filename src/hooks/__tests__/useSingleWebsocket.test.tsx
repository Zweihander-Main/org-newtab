import { renderHook } from '@testing-library/react';
import useSingleWebSocket from '../useSingleWebsocket';

describe('AppContext', () => {
	const { result } = renderHook(() => useSingleWebSocket(false));

	it('should return readyState of -1', () => {
		expect(result.current.readyState).toBe(-1);
	});
});

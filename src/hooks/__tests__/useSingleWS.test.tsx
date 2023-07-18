import { renderHook } from '@testing-library/react';
import useSingleWebSocket from '../useSingleWS';

describe('AppContext', () => {
	const { result } = renderHook(() => useSingleWebSocket());

	it('should start as client ws', () => {
		expect(result.current.amMasterWS).toBe(false);
	});
});

import { render, screen, waitFor } from '@testing-library/react';
import {
	StateProvider,
	INITIAL_VALUE,
	STATE_KEY,
	StateConsumer,
	// AppState,
} from '../state';

describe('useValue', () => {
	afterEach(async () => {
		await chrome.storage.local.clear();
		jest.clearAllMocks();
	});

	it('should ask storage for initial value', async () => {
		render(<StateProvider>{null}</StateProvider>);
		await waitFor(() => {
			expect(chrome.storage.local.get).toHaveBeenCalledWith(
				{ [STATE_KEY]: INITIAL_VALUE },
				expect.any(Function)
			);
		});
	});

	it('Consumer shows defaults', () => {
		render(
			<StateConsumer>
				{({ state }) => (
					<span data-testid="test">{JSON.stringify(state)}</span>
				)}
			</StateConsumer>
		);
		expect(screen.getByTestId('test').textContent).toBe(
			JSON.stringify({ ...INITIAL_VALUE })
		);
	});

	// it('should set cached value', async () => {
	// 	render(
	// 		<StateProvider>
	// 			<StateConsumer>
	// 				{({ state, setState }) => {
	// 					setState({ ...state, readyState: 1 });
	// 					return null;
	// 				}}
	// 			</StateConsumer>
	// 		</StateProvider>
	// 	);
	// 	await waitFor(() => {
	// 		expect(chrome.storage.local.set).toHaveBeenCalledWith(
	// 			{
	// 				readyState: 1,
	// 			},
	// 			expect.any(Function)
	// 		);
	// 	});
	// });

	// it('should set value in storage', async () => {
	// 	const { result } = renderHook(() => useValue('readyState'));
	// 	act(() => {
	// 		result.current.setValue(1);
	// 	});
	// });

	// it('should subscribe to storage listener', async () => {
	// 	expect(chrome.storage.onChanged.addListener).not.toHaveBeenCalled();
	// 	renderHook(() => useValue('readyState'));
	// 	await waitFor(() => {
	// 		expect(chrome.storage.onChanged.addListener).toHaveBeenCalled();
	// 	});
	// });
});

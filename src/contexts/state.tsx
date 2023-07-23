import { createContext } from 'react';

export type StateContextProps = {
	isInitialStateResolved: boolean;
};

const StateContext = createContext<StateContextProps>({
	isInitialStateResolved: false,
});

export default StateContext;

export const StateProvider: React.FC<{
	children?: React.ReactNode;
	isInitialStateResolved: boolean;
}> = ({ children, isInitialStateResolved }) => {
	return (
		<StateContext.Provider value={{ isInitialStateResolved }}>
			{children}
		</StateContext.Provider>
	);
};

export const { Consumer: StateConsumer } = StateContext;

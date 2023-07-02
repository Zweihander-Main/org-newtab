/* eslint-disable no-console */
import { createContext } from 'react';
import useStorageAndWaitForDefault from '../hooks/useStorageAndWaitForDefault';

export type StorageContext = {
	matchQuery: string | undefined;
	setMatchQuery: (newValue?: string | undefined) => void;
};

const StorageContext = createContext<StorageContext>({
	matchQuery: undefined,
	setMatchQuery: () => {
		return;
	},
});

export default StorageContext;

export const StorageProvider: React.FC<{ children?: React.ReactNode }> = ({
	children,
}) => {
	const [matchQuery, setMatchQuery] = useStorageAndWaitForDefault(
		'matchQuery',
		'TODO="TODO"'
	);

	return (
		<StorageContext.Provider value={{ matchQuery, setMatchQuery }}>
			{children}
		</StorageContext.Provider>
	);
};

export const { Consumer: StorageConsumer } = StorageContext;

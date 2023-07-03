/* eslint-disable no-console */
import { createContext, useRef } from 'react';
import { Storage } from '@plasmohq/storage';
import { useStorage } from '@plasmohq/storage/hook';
import useStorageAndWaitForDefault from '../hooks/useStorageAndWaitForDefault';

export type StorageContext = {
	matchQuery: string | undefined;
	setMatchQuery: (newValue?: string | undefined) => void;
	tagsData: Record<string, string>;
	setTagsData: ReturnType<typeof useStorage<Record<string, string>>>[1];
};

const StorageContext = createContext<StorageContext>({
	matchQuery: undefined,
	setMatchQuery: () => {
		return;
	},
	tagsData: {},
	setTagsData: () => Promise.resolve(),
});

export default StorageContext;

export const StorageProvider: React.FC<{ children?: React.ReactNode }> = ({
	children,
}) => {
	const storageRef = useRef(new Storage({ area: 'local' }));
	const [matchQuery, setMatchQuery] = useStorageAndWaitForDefault(
		'matchQuery',
		storageRef,
		'TODO="TODO"'
	);
	const [tagsData, setTagsData] = useStorage<Record<string, string>>(
		'tagsData',
		(v) => (v === undefined ? {} : v)
	);

	return (
		<StorageContext.Provider
			value={{ matchQuery, setMatchQuery, tagsData, setTagsData }}
		>
			{children}
		</StorageContext.Provider>
	);
};

export const { Consumer: StorageConsumer } = StorageContext;

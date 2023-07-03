/* eslint-disable no-console */
import { useCallback, useContext, useEffect, useState } from 'react';
import { usePrevious } from '@react-hookz/web';
import StorageContext, { StorageProvider } from 'contexts/StorageContext';
import WSContext, { WSProvider } from 'contexts/WSContext';
import '@fontsource/public-sans/700.css';
import './newtab.css';
import type { AllTagsRecv } from './types';
import ConnectionStatusIndicator from 'components/ConnectionStatusIndicator';
import OptionsMenu from 'components/OptionsMenu';
import OrgItem from 'components/OrgItem';

const IndexNewtab: React.FC = () => {
	const { sendJsonMessage, lastRecvJsonMessage } = useContext(WSContext);
	const { matchQuery, tagsData, setTagsData } = useContext(StorageContext);
	const previousMatchQuery = usePrevious(matchQuery);
	const [itemText, setItemText] = useState<string | null>(null);
	const [foregroundColor, setForegroundColor] = useState<string | undefined>(
		undefined
	);

	const sanitizeTagsAndMatchData = useCallback(
		(allTagsData?: AllTagsRecv) => {
			let allTags: Array<string> | string | undefined;
			if (Array.isArray(allTagsData)) {
				allTags = [];
				allTagsData
					.filter(
						(tag): tag is string =>
							typeof tag === 'string' && tag !== ''
					)
					.forEach((tag) => {
						const splitTags = tag
							.split(':')
							.filter((tag) => tag !== '');
						(allTags as Array<string>).push(...splitTags);
					});
			} else {
				allTags = allTagsData?.split(':').filter((tag) => tag !== '');
			}
			const foundTag = allTags
				?.map((tag) => tag.replace(/^:(.*):$/i, '$1'))
				?.find((tag) => Object.keys(tagsData).includes(tag));
			return foundTag || undefined;
		},
		[tagsData]
	);

	useEffect(() => {
		switch (lastRecvJsonMessage?.type) {
			case 'ITEM':
				setItemText(lastRecvJsonMessage?.data?.ITEM || null);
				setForegroundColor(
					tagsData[
						sanitizeTagsAndMatchData(
							lastRecvJsonMessage?.data?.ALLTAGS
						) || ''
					]
				);
				break;
			case 'TAGS':
				setTagsData(lastRecvJsonMessage?.data || {}).catch((err) => {
					console.error(
						'[NewTab] Error setting tags data in storage: ',
						err
					);
				});
				break;
			default:
				console.error(
					'[NewTab] Unknown message type: ',
					lastRecvJsonMessage
				);
				break;
		}
	}, [
		lastRecvJsonMessage,
		tagsData,
		setTagsData,
		setItemText,
		setForegroundColor,
		sanitizeTagsAndMatchData,
	]);

	useEffect(() => {
		if (
			matchQuery &&
			previousMatchQuery &&
			previousMatchQuery !== matchQuery
		) {
			sendJsonMessage({
				command: 'updateMatchQuery',
				data: matchQuery,
			});
		} else {
			sendJsonMessage({
				command: 'getItem',
				data: matchQuery,
			});
		}
	}, [matchQuery, previousMatchQuery, sendJsonMessage]);

	return (
		<div className="app">
			<OptionsMenu />
			<ConnectionStatusIndicator />
			<OrgItem foregroundColor={foregroundColor} itemText={itemText} />
		</div>
	);
};

// NEXT: This all gets remounted every time the WS switches which is not what we want.
// There's got to be a way to achieve this -- the ws should not surround the indexnewtab page. You need to put the interface
// in a component that's to the side of it that talks to the parent and the parent holds the state.
// Therefore, move the state into WSContext and have the two contexts interact with it using props. Render them next to the index.

const RootContextWrapper: React.FC = () => {
	return (
		<StorageProvider>
			<WSProvider>
				<IndexNewtab />
			</WSProvider>
		</StorageProvider>
	);
};

export default RootContextWrapper;

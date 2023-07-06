/* eslint-disable no-console */
import { useCallback, useContext, useEffect, useState } from 'react';
import { usePrevious } from '@react-hookz/web';
import WSContext, { WSProvider } from 'contexts/WSContext';
import '@fontsource/public-sans/700.css';
import './newtab.css';
import type { AllTagsRecv } from './types';
import ConnectionStatusIndicator from 'components/ConnectionStatusIndicator';
import OptionsMenu from 'components/OptionsMenu';
import OrgItem from 'components/OrgItem';
import { useChromeStorageLocal } from 'use-chrome-storage';
import type { State } from 'storage/state';

const IndexNewtab: React.FC = () => {
	const { sendJsonMessage, lastRecvJsonMessage } = useContext(WSContext);
	const [matchQuery, setMatchQuery] = useChromeStorageLocal<
		State['matchQuery']
	>('matchQuery', 'TODO="TODO"');
	const [tagsData, setTagsData] = useChromeStorageLocal<State['tagsData']>(
		'tagsData',
		{}
	);
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
		if (lastRecvJsonMessage === null) {
			return;
		}
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
				setTagsData(lastRecvJsonMessage?.data || {});
				break;
			default:
				console.error(
					'[NewTab] Unknown message: ',
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
			<OptionsMenu
				matchQuery={matchQuery}
				setMatchQuery={setMatchQuery}
			/>
			<ConnectionStatusIndicator />
			<OrgItem foregroundColor={foregroundColor} itemText={itemText} />
		</div>
	);
};

const RootContextWrapper: React.FC = () => {
	return (
		<WSProvider>
			<IndexNewtab />
		</WSProvider>
	);
};

export default RootContextWrapper;

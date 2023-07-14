import { useCallback, useContext, useEffect, useRef } from 'react';
import WSContext, { WSProvider } from 'contexts/WSContext';
import '@fontsource/public-sans/700.css';
import './newtab.css';
import ConnectionStatusIndicator from 'components/ConnectionStatusIndicator';
import OptionsMenu from 'components/OptionsMenu';
import OrgItem from 'components/OrgItem';
import useValue from 'hooks/useValue';

const IndexNewtab: React.FC = () => {
	const { sendJsonMessage, lastRecvJsonMessage, amMasterWS } =
		useContext(WSContext);
	const {
		value: matchQuery,
		isInitialStateResolved: isInitialMatchQueryStateResolved,
	} = useValue('matchQuery');
	const { value: tagsData, setValue: setTagsData } = useValue('tagsData');
	const { setValue: setOrgItem } = useValue('orgItem');
	const hasSentInitialQuery = useRef(false);
	useEffect(() => {
		if (lastRecvJsonMessage === null) {
			return;
		}
		switch (lastRecvJsonMessage?.type) {
			case 'ITEM':
				setOrgItem(lastRecvJsonMessage?.data || null);
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
	}, [lastRecvJsonMessage, tagsData, setTagsData, setOrgItem]);

	const updateMatchQuery = useCallback(
		(newMatchQuery: string) =>
			sendJsonMessage({
				command: 'updateMatchQuery',
				data: newMatchQuery,
			}),
		[sendJsonMessage]
	);

	useEffect(() => {
		if (
			!hasSentInitialQuery.current &&
			amMasterWS &&
			isInitialMatchQueryStateResolved
		) {
			sendJsonMessage({
				command: 'getItem',
				data: matchQuery,
			});
			hasSentInitialQuery.current = true;
		}
	}, [
		sendJsonMessage,
		matchQuery,
		amMasterWS,
		isInitialMatchQueryStateResolved,
	]);

	return (
		<main className="app">
			<OptionsMenu updateMatchQuery={updateMatchQuery} />
			<ConnectionStatusIndicator />
			<OrgItem />
		</main>
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

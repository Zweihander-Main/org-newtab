import './lib/wdyr';
import { useContext, useEffect, useRef } from 'react';
import '@fontsource/public-sans/700.css';
import './newtab.css';
import WSContext, { WSProvider } from 'contexts/ws';
import ConnectionStatusIndicator from 'components/ConnectionStatusIndicator';
import OptionsMenu from 'components/OptionsMenu';
import OrgItem from 'components/OrgItem';
import useValue from 'hooks/useValue';
import LoadingBar from 'components/LoadingBar';

const IndexNewtab: React.FC = () => {
	const { lastRecvJsonMessage, amMasterWS, getItem } = useContext(WSContext);
	const {
		value: matchQuery,
		isInitialStateResolved: isInitialMatchQueryStateResolved,
	} = useValue('matchQuery');
	const { setValue: setTagsData } = useValue('tagsData');
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
	}, [lastRecvJsonMessage, setTagsData, setOrgItem]);

	useEffect(() => {
		if (
			!hasSentInitialQuery.current &&
			amMasterWS &&
			isInitialMatchQueryStateResolved &&
			matchQuery
		) {
			getItem(matchQuery);
			hasSentInitialQuery.current = true;
		}
	}, [matchQuery, amMasterWS, isInitialMatchQueryStateResolved, getItem]);

	return (
		<main className="app">
			<LoadingBar animationDuration={200} />
			<OptionsMenu />
			<ConnectionStatusIndicator />
			<OrgItem />
		</main>
	);
};

// IndexNewtab.whyDidYouRender = true;

const RootContextWrapper: React.FC = () => {
	return (
		<WSProvider>
			<IndexNewtab />
		</WSProvider>
	);
};

export default RootContextWrapper;

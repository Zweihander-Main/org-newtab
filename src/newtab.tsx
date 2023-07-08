import { useContext, useEffect, useRef } from 'react';
import { usePrevious } from '@react-hookz/web';
import WSContext, { WSProvider } from 'contexts/WSContext';
import '@fontsource/public-sans/700.css';
import './newtab.css';
import ConnectionStatusIndicator from 'components/ConnectionStatusIndicator';
import OptionsMenu from 'components/OptionsMenu';
import OrgItem from 'components/OrgItem';
import useValue from 'hooks/useValue';

const IndexNewtab: React.FC = () => {
	const { sendJsonMessage, lastRecvJsonMessage } = useContext(WSContext);
	const [matchQuery] = useValue('matchQuery');
	const [tagsData, setTagsData] = useValue('tagsData');
	const previousMatchQuery = usePrevious(matchQuery);
	const [, setOrgItem] = useValue('orgItem');
	const isInitialRender = useRef(true);
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
		}
	}, [matchQuery, previousMatchQuery, sendJsonMessage]);

	useEffect(() => {
		if (isInitialRender.current) {
			sendJsonMessage({
				command: 'getItem',
			});
			isInitialRender.current = false;
		}
	}, [sendJsonMessage]);

	return (
		<div className="app">
			<OptionsMenu />
			<ConnectionStatusIndicator />
			<OrgItem />
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

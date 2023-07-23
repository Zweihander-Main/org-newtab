import '../lib/wdyr';
import { useContext, useEffect, useRef } from 'react';
import '@fontsource/public-sans/700.css';
import './index.css';
import WSContext, { WSProvider } from 'contexts/ws';
import StateContext, { StateProvider } from 'contexts/state';
import ConnectionStatusIndicator from 'components/ConnectionStatusIndicator';
import OptionsMenu from 'components/OptionsMenu';
import OrgItem from 'components/OrgItem';
import LoadingBar from 'components/LoadingBar';
import { ReadyState } from 'react-use-websocket';
import { Provider } from 'react-redux';
import store from '../store';
import { useAppDispatch, useAppSelector } from '../hooks';
import { setOrgItemTo, setTagsDataTo } from '../reducers';

const IndexNewtab: React.FC = () => {
	const { lastRecvJsonMessage, amMasterWS, getItem, readyState } =
		useContext(WSContext);
	const { isInitialStateResolved } = useContext(StateContext);
	const matchQuery = useAppSelector((state) => state.matchQuery);
	const dispatch = useAppDispatch();
	const hasSentInitialQuery = useRef(false);
	useEffect(() => {
		if (lastRecvJsonMessage === null) {
			return;
		}
		switch (lastRecvJsonMessage?.type) {
			case 'ITEM':
				dispatch(setOrgItemTo(lastRecvJsonMessage?.data || null));
				break;
			case 'TAGS':
				dispatch(setTagsDataTo(lastRecvJsonMessage?.data || {}));
				break;
			default:
				console.error(
					'[NewTab] Unknown message: ',
					lastRecvJsonMessage
				);
				break;
		}
	}, [dispatch, lastRecvJsonMessage]);

	useEffect(() => {
		if (
			!hasSentInitialQuery.current &&
			amMasterWS &&
			isInitialStateResolved &&
			matchQuery &&
			readyState === ReadyState.OPEN
		) {
			getItem(matchQuery);
			hasSentInitialQuery.current = true;
		}
	}, [matchQuery, amMasterWS, isInitialStateResolved, getItem, readyState]);

	return (
		<main className="app">
			<LoadingBar animationDuration={200} />
			<OptionsMenu />
			<ConnectionStatusIndicator />
			<OrgItem />
		</main>
	);
};

const RootContextWrapper: React.FC = () => {
	// TODO: Strict Mode
	return (
		<Provider store={store}>
			<StateProvider>
				<WSProvider>
					<IndexNewtab />
				</WSProvider>
			</StateProvider>
		</Provider>
	);
};

export default RootContextWrapper;

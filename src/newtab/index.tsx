import '../lib/wdyr';
import { useContext, useEffect, useRef } from 'react';
import { Provider } from 'react-redux';
import { PersistGate } from '@plasmohq/redux-persist/integration/react';
import '@fontsource/public-sans/700.css';
import './index.css';
import WSContext, { WSProvider } from 'contexts/ws';
import StateContext, { StateProvider } from 'contexts/state';
import ConnectionStatusIndicator from 'components/ConnectionStatusIndicator';
import OptionsMenu from 'components/OptionsMenu';
import OrgItem from 'components/OrgItem';
import LoadingBar from 'components/LoadingBar';
import store, { persistor } from '../app/store';
import { useAppSelector } from '../app/hooks';
import { WSReadyState } from 'lib/types';
import { selectedAmMasterWs, selectedReadyState } from 'modules/ws/wsSlice';
import { selectedMatchQuery } from 'modules/emacs/emacsSlice';

const IndexNewtab: React.FC = () => {
	const { getItem } = useContext(WSContext);
	const { isInitialStateResolved } = useContext(StateContext);
	const amMasterWS = useAppSelector(selectedAmMasterWs);
	const readyState = useAppSelector(selectedReadyState);
	const matchQuery = useAppSelector(selectedMatchQuery);
	const hasSentInitialQuery = useRef(false);

	useEffect(() => {
		if (
			!hasSentInitialQuery.current &&
			amMasterWS &&
			isInitialStateResolved &&
			matchQuery &&
			readyState === WSReadyState.OPEN
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
			<PersistGate persistor={persistor}>
				{(isInitialStateResolved) => (
					<StateProvider
						isInitialStateResolved={isInitialStateResolved}
					>
						<WSProvider>
							<IndexNewtab />
						</WSProvider>
					</StateProvider>
				)}
			</PersistGate>
		</Provider>
	);
};

export default RootContextWrapper;

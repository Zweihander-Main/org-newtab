import '../lib/wdyr';
import { useEffect, useRef } from 'react';
import { Provider } from 'react-redux';
import { PersistGate } from '@plasmohq/redux-persist/integration/react';
import '@fontsource/public-sans/700.css';
import './index.css';
import ConnectionStatusIndicator from 'components/ConnectionStatusIndicator';
import OptionsMenu from 'components/OptionsMenu';
import OrgItem from 'components/OrgItem';
import LoadingBar from 'components/LoadingBar';
import store, { persistor } from '../app/store';
import { useAppDispatch } from '../app/hooks';
import { setStateAsResolved } from 'modules/role/roleSlice';
import { initMessaging } from 'modules/msg/msgSlice';

const IndexNewtab: React.FC = () => {
	const dispatch = useAppDispatch();
	const isInitialRender = useRef(true);

	useEffect(() => {
		if (isInitialRender.current) {
			isInitialRender.current = false;
			dispatch(initMessaging());
		}
	}, [dispatch]);

	useEffect(() => {
		document.title = chrome.i18n.getMessage('title');
	}, []);

	return (
		<main className="app">
			<LoadingBar animationDuration={200} />
			<OptionsMenu />
			<ConnectionStatusIndicator />
			<OrgItem />
		</main>
	);
};

const StateResolver: React.FC<{ isInitialStateResolved: boolean }> = ({
	isInitialStateResolved,
}) => {
	const dispatch = useAppDispatch();

	useEffect(() => {
		if (isInitialStateResolved) {
			dispatch(setStateAsResolved());
		}
	}, [dispatch, isInitialStateResolved]);

	return null;
};

const RootContextWrapper: React.FC = () => {
	return (
		<Provider store={store}>
			<PersistGate persistor={persistor}>
				{(isInitialStateResolved) => {
					return (
						<StateResolver
							isInitialStateResolved={isInitialStateResolved}
						/>
					);
				}}
			</PersistGate>
			<IndexNewtab />
		</Provider>
	);
};

export default RootContextWrapper;

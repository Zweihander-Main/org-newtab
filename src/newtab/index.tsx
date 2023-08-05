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
import { setStateAsResolved, establishRole } from 'modules/role/roleSlice';

const IndexNewtab: React.FC = () => {
	const dispatch = useAppDispatch();
	const isInitialRender = useRef(true);

	useEffect(() => {
		if (isInitialRender.current) {
			isInitialRender.current = false;
			dispatch(establishRole());
		}
	}, [dispatch]);

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
				{(isInitialStateResolved) => {
					if (isInitialStateResolved) {
						store.dispatch(setStateAsResolved());
					}
					return null;
				}}
			</PersistGate>
			<IndexNewtab />
		</Provider>
	);
};

export default RootContextWrapper;

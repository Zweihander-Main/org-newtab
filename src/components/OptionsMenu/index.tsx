import * as styles from './style.module.css';
import { useCallback, useEffect, useRef, useState } from 'react';
import { useAppDispatch, useAppSelector } from '../../app/hooks';
import {
	selectedMatchQuery,
	setMatchQueryTo,
} from '../../modules/emacs/emacsSlice';
import {
	selectedAmMasterRole,
	selectedStateResolved,
} from 'modules/role/roleSlice';
import { selectedWSPort, setWSPortTo } from 'modules/ws/wsSlice';

const OptionsMenu: React.FC = () => {
	const dispatch = useAppDispatch();
	const matchQuery = useAppSelector(selectedMatchQuery);
	const amMasterRole = useAppSelector(selectedAmMasterRole);
	const wsPort = useAppSelector(selectedWSPort);
	const isInitialStateResolved = useAppSelector(selectedStateResolved);
	const [optionsVisible, setOptionsVisible] = useState(false);

	const matchQueryInputRef = useRef<HTMLInputElement>(null);
	const wsPortInputRef = useRef<HTMLInputElement>(null);

	const masterStatus = amMasterRole
		? chrome.i18n.getMessage('masterRole')
		: chrome.i18n.getMessage('clientRole');

	const handleFormSubmit = useCallback(
		(event: React.FormEvent<HTMLFormElement>) => {
			event.preventDefault();
			const { currentTarget } = event;
			const data = new FormData(currentTarget);
			const formMatchQuery = data.get('matchQuery');
			if (formMatchQuery && typeof formMatchQuery === 'string') {
				dispatch(setMatchQueryTo(formMatchQuery));
			}
			const formWSPort = data.get('wsPort');
			if (formWSPort && typeof formWSPort === 'string') {
				const portNumber = parseInt(formWSPort, 10);
				if (
					!isNaN(portNumber) &&
					portNumber > 0 &&
					portNumber < 65536 &&
					portNumber !== wsPort
				) {
					dispatch(setWSPortTo(portNumber));
				}
			}
		},
		[dispatch, wsPort]
	);

	useEffect(() => {
		if (matchQueryInputRef.current && matchQuery) {
			matchQueryInputRef.current.value = matchQuery;
		}
	}, [isInitialStateResolved, matchQuery]);

	useEffect(() => {
		if (wsPortInputRef.current && wsPort) {
			wsPortInputRef.current.value = wsPort.toString();
		}
	}, [isInitialStateResolved, wsPort]);

	const toggleMenu = useCallback(() => {
		setOptionsVisible(!optionsVisible);
	}, [optionsVisible]);

	const optionsMenuButtonClass = [
		styles.button,
		optionsVisible ? styles.active : '',
	].join(' ');

	const optionsMenuClass = [
		styles.menu,
		optionsVisible ? styles.active : '',
	].join(' ');

	// TODO: break out button in its own component
	return (
		<>
			<button
				aria-label={chrome.i18n.getMessage('optionsMenu')}
				className={optionsMenuButtonClass}
				onClick={toggleMenu}
			>
				<div className={styles['button-bar1']}></div>
				<div className={styles['button-bar2']}></div>
				<div className={styles['button-bar3']}></div>
			</button>
			<nav className={optionsMenuClass}>
				<form
					className={styles.form}
					method="post"
					onSubmit={handleFormSubmit}
				>
					<label htmlFor="matchQuery">
						{chrome.i18n.getMessage('matchQuery')}:{' '}
					</label>
					<input
						type="text"
						name="matchQuery"
						defaultValue={matchQuery}
						ref={matchQueryInputRef}
						aria-label={chrome.i18n.getMessage('matchQuery')}
					/>
					<label htmlFor="wsPort">
						{chrome.i18n.getMessage('wsPort')}:
					</label>
					<input
						type="number"
						name="wsPort"
						defaultValue={wsPort}
						ref={wsPortInputRef}
						aria-label={chrome.i18n.getMessage('wsPort')}
					/>
					<button type="submit" disabled={false}>
						{chrome.i18n.getMessage('saveOptions')}
					</button>
				</form>
				<div
					data-testid="initial-state"
					className={styles['initial-state']}
				>
					{chrome.i18n.getMessage('storageStatus')}:{' '}
					{isInitialStateResolved
						? chrome.i18n.getMessage('storageResolved')
						: chrome.i18n.getMessage('storageUnresolved')}
				</div>
				<div
					data-testid="websocket-role"
					className={styles['websocket-role']}
				>
					{chrome.i18n.getMessage('websocketRole')}: {masterStatus}
				</div>
			</nav>
		</>
	);
};

export default OptionsMenu;

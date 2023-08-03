import StateContext from 'contexts/state';
import * as styles from './style.module.css';
import { useCallback, useContext, useEffect, useRef, useState } from 'react';
import { useAppDispatch, useAppSelector } from '../../app/hooks';
import {
	selectedMatchQuery,
	setMatchQueryTo,
} from '../../modules/emacs/emacsSlice';
import { selectedAmMasterWs } from 'modules/role/roleSlice';
import { selectedWSPort, setWSPortTo } from 'modules/ws/wsSlice';

const OptionsMenu: React.FC = () => {
	const dispatch = useAppDispatch();
	const matchQuery = useAppSelector(selectedMatchQuery);
	const amMasterWS = useAppSelector(selectedAmMasterWs);
	const wsPort = useAppSelector(selectedWSPort);
	const { isInitialStateResolved } = useContext(StateContext);
	const [optionsVisible, setOptionsVisible] = useState(false);

	const matchQueryInputRef = useRef<HTMLInputElement>(null);
	const wsPortInputRef = useRef<HTMLInputElement>(null);

	const masterStatus = amMasterWS ? 'Master' : 'Client';

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
		if (
			isInitialStateResolved &&
			matchQueryInputRef.current &&
			matchQuery
		) {
			matchQueryInputRef.current.value = matchQuery;
		}
	}, [isInitialStateResolved, matchQuery]);

	useEffect(() => {
		if (isInitialStateResolved && wsPortInputRef.current && wsPort) {
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

	return (
		<>
			<button
				aria-label="Options Menu"
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
					<label htmlFor="matchQuery">Match Query: </label>
					<input
						type="text"
						name="matchQuery"
						defaultValue={matchQuery}
						ref={matchQueryInputRef}
						aria-label="Match Query"
					/>
					<label htmlFor="wsPort">WebSocket Port:</label>
					<input
						type="number"
						name="wsPort"
						defaultValue={wsPort}
						ref={wsPortInputRef}
						aria-label="WebSocket Port"
					/>
					<button type="submit">Update</button>
				</form>
				{/* // TODO: fix */}
				{/* {lastRecvJsonMessage ? (
					<>
						Last message:
						<pre className={styles.json}>
							{JSON.stringify(lastRecvJsonMessage, null, 2)}
						</pre>
					</>
				) : null} */}
				<div
					data-testid="websocket-role"
					className={styles['websocket-role']}
				>
					WebSocket Role: {masterStatus}
				</div>
			</nav>
		</>
	);
};

export default OptionsMenu;

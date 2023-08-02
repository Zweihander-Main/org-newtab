import StateContext from 'contexts/state';
import * as styles from './style.module.css';
import { useCallback, useContext, useEffect, useRef, useState } from 'react';
import { useAppDispatch, useAppSelector } from '../../app/hooks';
import {
	selectedMatchQuery,
	setMatchQueryTo,
} from '../../modules/emacs/emacsSlice';
import { selectedAmMasterWs } from 'modules/role/roleSlice';

const OptionsMenu: React.FC = () => {
	const dispatch = useAppDispatch();
	const matchQuery = useAppSelector(selectedMatchQuery);
	const amMasterWS = useAppSelector(selectedAmMasterWs);
	const { isInitialStateResolved } = useContext(StateContext);
	const [optionsVisible, setOptionsVisible] = useState(false);

	const masterStatus = amMasterWS ? 'Master' : 'Client';

	const handleMatchQuerySubmit = useCallback(
		(event: React.FormEvent<HTMLFormElement>) => {
			event.preventDefault();
			const { currentTarget } = event;
			const data = new FormData(currentTarget);
			const formMatchQuery = data.get('matchQuery');
			if (formMatchQuery && typeof formMatchQuery === 'string') {
				dispatch(setMatchQueryTo(formMatchQuery));
			}
		},
		[dispatch]
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

	const matchQueryInputRef = useRef<HTMLInputElement>(null);

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
				<form method="post" onSubmit={handleMatchQuerySubmit}>
					<input
						type="text"
						name="matchQuery"
						defaultValue={matchQuery}
						ref={matchQueryInputRef}
						aria-label="Match Query"
					/>
					<button type="submit">Pull data</button>
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

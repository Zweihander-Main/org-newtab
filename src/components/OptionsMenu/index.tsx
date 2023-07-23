import StateContext from 'contexts/state';
import * as styles from './style.module.css';
import WSContext from 'contexts/ws';
import { useCallback, useContext, useEffect, useRef, useState } from 'react';
import { useAppDispatch, useAppSelector } from '../../hooks';
import { setMatchQueryTo } from '../../stateReducer';

const OptionsMenu: React.FC = () => {
	const dispatch = useAppDispatch();
	const matchQuery = useAppSelector((state) => state.matchQuery);
	const amMasterWS = useAppSelector((state) => state.amMasterWS);
	const { isInitialStateResolved } = useContext(StateContext);
	const { lastRecvJsonMessage, updateMatchQuery } = useContext(WSContext);
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
				void updateMatchQuery(formMatchQuery);
				// TODO combine
			}
		},
		[updateMatchQuery, dispatch]
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
				{lastRecvJsonMessage ? (
					<>
						Last message:
						<pre className={styles.json}>
							{JSON.stringify(lastRecvJsonMessage, null, 2)}
						</pre>
					</>
				) : null}
				<div
					data-testid="websocket-status"
					className={styles['websocket-status']}
				>
					WebSocket Status: {masterStatus}
				</div>
			</nav>
		</>
	);
};

export default OptionsMenu;

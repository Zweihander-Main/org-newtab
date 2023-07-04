import * as styles from './style.module.css';
import StorageContext from 'contexts/StorageContext';
import WSContext from 'contexts/WSContext';
import { useCallback, useContext, useEffect, useRef, useState } from 'react';

const OptionsMenu: React.FC = () => {
	const { matchQuery, setMatchQuery } = useContext(StorageContext);
	const { lastRecvJsonMessage } = useContext(WSContext);
	const [optionsVisible, setOptionsVisible] = useState(false);

	const handleMatchQuerySubmit = useCallback(
		(event: React.FormEvent<HTMLFormElement>) => {
			event.preventDefault();
			const { currentTarget } = event;
			const data = new FormData(currentTarget);
			const formMatchQuery = data.get('matchQuery');
			if (formMatchQuery && typeof formMatchQuery === 'string') {
				setMatchQuery(formMatchQuery);
			}
		},
		[setMatchQuery]
	);

	const matchQueryInputRef = useRef<HTMLInputElement>(null);

	useEffect(() => {
		if (matchQueryInputRef.current && matchQuery !== undefined) {
			matchQueryInputRef.current.value = matchQuery;
		}
	}, [matchQuery, matchQueryInputRef]);

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
			<button className={optionsMenuButtonClass} onClick={toggleMenu}>
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
					/>
					<button type="submit">Pull data</button>
				</form>
				{lastRecvJsonMessage ? (
					<>
						Last message:
						<pre className={styles.json}>
							{JSON.stringify(lastRecvJsonMessage, null, 2)}
							{/*TODO: Display null*/}
						</pre>
					</>
				) : null}
			</nav>
		</>
	);
};

export default OptionsMenu;
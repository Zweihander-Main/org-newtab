import * as styles from './style.module.css';
import { useAppDispatch, useAppSelector } from 'app/hooks';
import { selectedMatchQuery, setMatchQueryTo } from 'modules/emacs/emacsSlice';
import { selectedStateResolved } from 'modules/role/roleSlice';
import { selectedWSPort, setWSPortTo } from 'modules/ws/wsSlice';
import { useCallback, useEffect, useRef } from 'react';

const BehaviorPanel: React.FC = () => {
	const dispatch = useAppDispatch();
	const matchQuery = useAppSelector(selectedMatchQuery);
	const wsPort = useAppSelector(selectedWSPort);
	const matchQueryInputRef = useRef<HTMLInputElement>(null);
	const wsPortInputRef = useRef<HTMLInputElement>(null);
	const isInitialStateResolved = useAppSelector(selectedStateResolved);

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

	return (
		<form className={styles.form} method="post" onSubmit={handleFormSubmit}>
			<label className={styles.label} htmlFor="matchQuery">
				{chrome.i18n.getMessage('matchQuery')}:{' '}
			</label>
			<input
				className={styles.input}
				type="text"
				name="matchQuery"
				defaultValue={matchQuery}
				ref={matchQueryInputRef}
				aria-label={chrome.i18n.getMessage('matchQuery')}
			/>
			<label className={styles.label} htmlFor="wsPort">
				{chrome.i18n.getMessage('wsPort')}:
			</label>
			<input
				className={styles.input}
				type="number"
				name="wsPort"
				defaultValue={wsPort}
				ref={wsPortInputRef}
				aria-label={chrome.i18n.getMessage('wsPort')}
			/>
			<button className={styles.button} type="submit" disabled={false}>
				{chrome.i18n.getMessage('saveOptions')}
			</button>
		</form>
	);
};

export default BehaviorPanel;

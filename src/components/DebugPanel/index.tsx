import { useAppDispatch, useAppSelector } from 'app/hooks';
import * as styles from './style.module.css';
import {
	selectedAmMasterRole,
	selectedStateResolved,
} from 'modules/role/roleSlice';
import { selectedTagsData } from 'modules/emacs/emacsSlice';
import Button from '../Button';
import { useCallback } from 'react';
import { resetData } from 'app/actions';

const DebugPanel: React.FC = () => {
	const isInitialStateResolved = useAppSelector(selectedStateResolved);
	const amMasterRole = useAppSelector(selectedAmMasterRole);
	const tagsData = useAppSelector(selectedTagsData);
	const dispatch = useAppDispatch();
	const masterStatus = amMasterRole
		? chrome.i18n.getMessage('masterRole')
		: chrome.i18n.getMessage('clientRole');

	const onResetClick = useCallback(() => {
		dispatch(resetData());
	}, [dispatch]);

	return (
		<div className={styles.panel}>
			<div data-testid="initial-state" className={styles.item}>
				<span className={styles.name}>
					{chrome.i18n.getMessage('storageStatus')}:{' '}
				</span>
				{isInitialStateResolved
					? chrome.i18n.getMessage('storageResolved')
					: chrome.i18n.getMessage('storageUnresolved')}
			</div>
			<div data-testid="websocket-role" className={styles.item}>
				<span className={styles.name}>
					{chrome.i18n.getMessage('websocketRole')}:{' '}
				</span>
				{masterStatus}
			</div>
			<div data-testid="tags-data" className={styles.item}>
				<span className={styles.name}>
					{chrome.i18n.getMessage('tagsData')}:{' '}
				</span>
				<pre>{JSON.stringify(tagsData, null, 2)}</pre>
			</div>
			<Button
				className={styles.reset}
				styleType="reset"
				onClick={onResetClick}
			>
				Reset App and Storage
			</Button>
			<div
				className={styles.pitch}
				dangerouslySetInnerHTML={{
					__html: chrome.i18n.getMessage('pitch'),
				}}
			></div>
		</div>
	);
};

export default DebugPanel;

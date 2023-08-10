import { useAppSelector } from 'app/hooks';
import * as styles from './style.module.css';
import {
	selectedAmMasterRole,
	selectedStateResolved,
} from 'modules/role/roleSlice';

const DebugPanel: React.FC = () => {
	const isInitialStateResolved = useAppSelector(selectedStateResolved);
	const amMasterRole = useAppSelector(selectedAmMasterRole);
	const masterStatus = amMasterRole
		? chrome.i18n.getMessage('masterRole')
		: chrome.i18n.getMessage('clientRole');
	return (
		<>
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
		</>
	);
};

export default DebugPanel;

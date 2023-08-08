import * as styles from './style.module.css';
import { useAppSelector } from '../../app/hooks';
import { WSReadyState } from '../../lib/types';
import { selectedReadyState } from 'modules/ws/wsSlice';

const ConnectionStatusIndicator: React.FC = () => {
	const readyState = useAppSelector(selectedReadyState);
	const connectionStatus = {
		[WSReadyState.CONNECTING]: chrome.i18n.getMessage(
			'connectionStatusConnecting'
		),
		[WSReadyState.OPEN]: chrome.i18n.getMessage('connectionStatusOpen'),
		[WSReadyState.CLOSING]: chrome.i18n.getMessage(
			'connectionStatusClosing'
		),
		[WSReadyState.CLOSED]: chrome.i18n.getMessage('connectionStatusClosed'),
		[WSReadyState.UNINSTANTIATED]: chrome.i18n.getMessage(
			'connectionStatusUninstantiated'
		),
	}[readyState];

	return (
		<footer data-testid="connection-status" className={styles.status}>
			{connectionStatus}
		</footer>
	);
};

export default ConnectionStatusIndicator;

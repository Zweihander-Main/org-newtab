import * as styles from './style.module.css';
import { useAppSelector } from '../../app/hooks';
import { WSReadyState } from '../../lib/types';
import { selectedReadyState } from 'modules/ws/wsSlice';

const ConnectionStatusIndicator: React.FC = () => {
	const readyState = useAppSelector(selectedReadyState);
	const connectionStatus = {
		[WSReadyState.CONNECTING]: 'Connecting',
		[WSReadyState.OPEN]: 'Connected',
		[WSReadyState.CLOSING]: 'Closing',
		[WSReadyState.CLOSED]: 'Not Connected',
		[WSReadyState.UNINSTANTIATED]: 'Initializing',
	}[readyState];

	return (
		<footer data-testid="connection-status" className={styles.status}>
			{connectionStatus}
		</footer>
	);
};

export default ConnectionStatusIndicator;

import * as styles from './style.module.css';
import { useAppSelector } from '../../app/hooks';
import { WSReadyState } from '../../lib/types';

const ConnectionStatusIndicator: React.FC = () => {
	const readyState = useAppSelector((state) => state.ws.readyState);
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

import * as styles from './style.module.css';
import useValue from 'hooks/useValue';
import { ReadyState } from 'react-use-websocket';

const ConnectionStatusIndicator: React.FC = () => {
	const [readyState] = useValue('readyState');
	const connectionStatus = {
		[ReadyState.CONNECTING]: 'Connecting',
		[ReadyState.OPEN]: 'Connected',
		[ReadyState.CLOSING]: 'Closing',
		[ReadyState.CLOSED]: 'Not Connected',
		[ReadyState.UNINSTANTIATED]: 'Initializing',
	}[readyState];

	return (
		<p data-testid="connection-status" className={styles.status}>
			{connectionStatus}
		</p>
	);
};

export default ConnectionStatusIndicator;

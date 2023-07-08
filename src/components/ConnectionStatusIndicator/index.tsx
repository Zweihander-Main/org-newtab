import * as styles from './style.module.css';
import useValue from 'hooks/useValue';
import { ReadyState } from 'react-use-websocket';

const ConnectionStatusIndicator: React.FC = () => {
	const [readyState] = useValue('readyState');
	const connectionStatus = {
		[ReadyState.CONNECTING]: 'Connecting',
		[ReadyState.OPEN]: 'Open',
		[ReadyState.CLOSING]: 'Closing',
		[ReadyState.CLOSED]: 'Closed',
		[ReadyState.UNINSTANTIATED]: 'Uninstantiated',
	}[readyState];

	return (
		<p data-testid="connection-status" className={styles.status}>
			{connectionStatus}
		</p>
	);
};

export default ConnectionStatusIndicator;

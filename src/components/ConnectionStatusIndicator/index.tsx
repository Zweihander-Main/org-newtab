import { useContext } from 'react';
import * as styles from './style.module.css';
import { ReadyState } from 'react-use-websocket';
import WSContext from 'contexts/ws';

const ConnectionStatusIndicator: React.FC = () => {
	const { readyState } = useContext(WSContext);
	const connectionStatus = {
		[ReadyState.CONNECTING]: 'Connecting',
		[ReadyState.OPEN]: 'Connected',
		[ReadyState.CLOSING]: 'Closing',
		[ReadyState.CLOSED]: 'Not Connected',
		[ReadyState.UNINSTANTIATED]: 'Initializing',
	}[readyState];

	return (
		<footer data-testid="connection-status" className={styles.status}>
			{connectionStatus}
		</footer>
	);
};

export default ConnectionStatusIndicator;

import * as styles from './style.module.css';
import WSContext from 'contexts/WSContext';
import { useContext } from 'react';
import { ReadyState } from 'react-use-websocket';

const ConnectionStatusIndicator: React.FC = () => {
	const { amMasterWS, readyState } = useContext(WSContext);
	const connectionStatus = {
		[ReadyState.CONNECTING]: 'Connecting',
		[ReadyState.OPEN]: 'Open',
		[ReadyState.CLOSING]: 'Closing',
		[ReadyState.CLOSED]: 'Closed',
		[ReadyState.UNINSTANTIATED]: 'Uninstantiated',
	}[readyState];

	const masterStatus = amMasterWS ? 'Master' : 'Client';

	return (
		<p data-testid="connection-status" className={styles.status}>
			{connectionStatus} - {masterStatus}
		</p>
	);
};

export default ConnectionStatusIndicator;

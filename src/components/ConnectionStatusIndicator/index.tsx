import * as styles from './style.module.css';
import { useWSContext } from 'contexts/WSContext';
import { ReadyState } from 'react-use-websocket';

const ConnectionStatusIndicator: React.FC = () => {
	const { amMasterWS, readyState } = useWSContext();
	const connectionStatus = {
		[ReadyState.CONNECTING]: 'Connecting',
		[ReadyState.OPEN]: 'Open',
		[ReadyState.CLOSING]: 'Closing',
		[ReadyState.CLOSED]: 'Closed',
		[ReadyState.UNINSTANTIATED]: 'Uninstantiated',
	}[readyState];

	const masterStatus = amMasterWS ? 'Master' : 'Client';

	return (
		<p className={styles.status}>
			{connectionStatus} - {masterStatus}
		</p>
	);
};

export default ConnectionStatusIndicator;

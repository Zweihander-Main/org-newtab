import * as styles from './style.module.css';
import Checkbox from 'components/Checkbox';

const LayoutPanel: React.FC = () => {
	return (
		<div className={styles.form}>
			<p className={styles.heading}>Connection status indicator:</p>
			<Checkbox label="Show connection status" />
		</div>
	);
};

export default LayoutPanel;

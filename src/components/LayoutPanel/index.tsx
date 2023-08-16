import * as styles from './style.module.css';

const LayoutPanel: React.FC = () => {
	// NEXT: think about UX here, change name, add functionality
	return (
		<label className={styles.label} htmlFor="show-connection-status">
			<input
				className={styles.checkbox}
				type="checkbox"
				id="show-connection-status"
			/>
			Show connection status
		</label>
	);
};

export default LayoutPanel;

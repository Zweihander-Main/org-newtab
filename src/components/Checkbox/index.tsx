import * as styles from './style.module.css';

type CheckboxProps = {
	label: string;
};

const Checkbox: React.FC<CheckboxProps> = ({ label }) => {
	return (
		<label className={styles.label} htmlFor="show-connection-status">
			<input
				className={styles.checkbox}
				type="checkbox"
				id="show-connection-status"
			/>
			{label}
		</label>
	);
};

export default Checkbox;

import * as styles from './style.module.css';

type OrgItemProps = {
	itemText: string | null;
	foregroundColor?: string;
};

const OrgItem: React.FC<OrgItemProps> = ({ itemText, foregroundColor }) => {
	return (
		<>
			{itemText && (
				<div
					className={styles.item}
					style={{ backgroundColor: foregroundColor }}
				>
					{itemText}
				</div>
			)}
		</>
	);
};

export default OrgItem;

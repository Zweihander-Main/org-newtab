import * as styles from './style.module.css';
import logo from 'data-base64:~assets/icon-1024x1024.png';
import { useAppSelector } from '../../app/hooks';
import { selectedIsInSync } from 'modules/ws/wsSlice';
import { selectedItemText, selectedTagColor } from 'modules/emacs/emacsSlice';
import classNames from 'classnames';

const OrgItem: React.FC = () => {
	const itemText = useAppSelector(selectedItemText);
	const tagColor = useAppSelector(selectedTagColor);
	const isInSync = useAppSelector(selectedIsInSync);

	return (
		<>
			{itemText ? (
				<h1
					className={classNames(styles.item, {
						[styles.stale]: !isInSync,
					})}
					style={{ backgroundColor: tagColor || undefined }}
					data-testid="item-text"
				>
					{itemText}
				</h1>
			) : (
				<img src={logo} className={styles.logo} alt="logo" />
			)}
		</>
	);
};

export default OrgItem;

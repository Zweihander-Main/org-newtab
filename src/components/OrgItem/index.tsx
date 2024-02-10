import * as styles from './style.module.css';
import logo from 'data-base64:~assets/icon-1024x1024bw.png';
import { useAppSelector } from '../../app/hooks';
import { selectedIsInSync } from 'modules/ws/wsSlice';
import { selectedItemText, selectedTagColors } from 'modules/emacs/emacsSlice';
import classNames from 'classnames';

const OrgItem: React.FC = () => {
	const itemText = useAppSelector(selectedItemText);
	const tagColors = useAppSelector(selectedTagColors);
	const isInSync = useAppSelector(selectedIsInSync);

	let background: string | undefined = undefined;
	if (tagColors.length === 1) {
		background = tagColors[0];
	} else if (tagColors.length > 1) {
		background = `linear-gradient(-45deg, ${tagColors.join(', ')})`;
	}

	return (
		<>
			{itemText ? (
				<h1
					className={classNames(styles.item, {
						[styles.stale]: !isInSync,
					})}
					style={{ background }}
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

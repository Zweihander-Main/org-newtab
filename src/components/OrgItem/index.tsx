import * as styles from './style.module.css';
import { WSReadyState } from '../../lib/types';
import logo from 'data-base64:~assets/icon-1024x1024.png';
import { useAppSelector } from '../../app/hooks';
import {
	selectedIsWaitingForResponse,
	selectedReadyState,
} from 'modules/ws/wsSlice';
import { selectedItemText, selectedTagColor } from 'modules/emacs/emacsSlice';

//TODO: flip out image with actual transparency
//TODO: pull in other data from org item
//TODO: better responsive design

const OrgItem: React.FC = () => {
	const itemText = useAppSelector(selectedItemText);
	const tagColor = useAppSelector(selectedTagColor);
	const readyState = useAppSelector(selectedReadyState);
	const isWaitingForResponse = useAppSelector(selectedIsWaitingForResponse);

	const classString = `${styles.item}${
		readyState !== WSReadyState.OPEN || isWaitingForResponse
			? ' ' + styles.stale
			: ''
	}`;

	return (
		<>
			{itemText ? (
				<h1
					className={classString}
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

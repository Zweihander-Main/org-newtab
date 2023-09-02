import * as styles from './style.module.css';
import { WSReadyState } from '../../lib/types';
import logo from 'data-base64:~assets/icon-1024x1024.png';
import { useAppSelector } from '../../app/hooks';
import {
	selectedIsWaitingForResponse,
	selectedReadyState,
} from 'modules/ws/wsSlice';
import {
	selectedItemClockStartTimeMS,
	selectedItemEffortMinutes,
	selectedItemPreviouslyClockedMinutes,
	selectedItemText,
	selectedTagColor,
} from 'modules/emacs/emacsSlice';
import { useCallback, useEffect, useState } from 'react';

//TODO: flip out image with actual transparency
//TODO: pull in other data from org item
//TODO: better responsive design

const OrgItem: React.FC = () => {
	const itemText = useAppSelector(selectedItemText);
	const tagColor = useAppSelector(selectedTagColor);
	const readyState = useAppSelector(selectedReadyState);
	const isWaitingForResponse = useAppSelector(selectedIsWaitingForResponse);
	const itemClockStartTime = useAppSelector(selectedItemClockStartTimeMS);
	const itemPreviouslyClockedMinutes = useAppSelector(
		selectedItemPreviouslyClockedMinutes
	);
	const itemEffortMinutes = useAppSelector(selectedItemEffortMinutes);

	const isClockedIn = itemClockStartTime !== null;
	const [minutesClockedIn, setMinutesClockedIn] = useState(
		itemPreviouslyClockedMinutes
	);
	const minutesToTimeString = useCallback((minutes: number): string => {
		const hours = Math.floor(minutes / 60);
		const remainingMinutes = minutes % 60;

		const formattedHours = hours.toString();
		const formattedMinutes = remainingMinutes.toString().padStart(2, '0');

		return `${formattedHours}:${formattedMinutes}`;
	}, []);

	const calculateMinutesClockedIn = useCallback(() => {
		const now = new Date().getTime();
		const start = new Date(itemClockStartTime as number).getTime();
		const diff = Math.floor((now - start) / 1000 / 60);
		const total = diff + itemPreviouslyClockedMinutes;
		setMinutesClockedIn(total);
	}, [itemClockStartTime, itemPreviouslyClockedMinutes]);

	useEffect(() => {
		let interval: NodeJS.Timeout;
		if (isClockedIn) {
			calculateMinutesClockedIn();
			interval = setInterval(calculateMinutesClockedIn, 5000);
		}
		return () => clearInterval(interval);
	}, [isClockedIn, calculateMinutesClockedIn]);
	// TODO: mark overtime

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
					{isClockedIn && (
						<span className={styles.clock}>
							{minutesToTimeString(minutesClockedIn)}
							{itemEffortMinutes && (
								<>
									{' / '}
									{minutesToTimeString(itemEffortMinutes)}
								</>
							)}
						</span>
					)}
				</h1>
			) : (
				<img src={logo} className={styles.logo} alt="logo" />
			)}
		</>
	);
};

export default OrgItem;

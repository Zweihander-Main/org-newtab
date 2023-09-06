import * as styles from './style.module.css';
import { WSReadyState } from '../../lib/types';
import logo from 'data-base64:~assets/icon-1024x1024.png';
import { useAppSelector } from '../../app/hooks';
import {
	selectedIsWaitingForResponse,
	selectedReadyState,
} from 'modules/ws/wsSlice';
import {
	selectedItemClockStartTime,
	selectedItemEffortMinutes,
	selectedItemPreviouslyClockedMinutes,
	selectedItemText,
	selectedTagColor,
} from 'modules/emacs/emacsSlice';
import { useCallback, useEffect, useState } from 'react';
import classNames from 'classnames';

//TODO: flip out image with actual transparency
//TODO: pull in other data from org item
//TODO: better responsive design
//TODO: fix number shadow issue
//TODO: figma implement overtime, position, ect.

const ClockedTime: React.FC = () => {
	const itemPreviouslyClockedMinutes = useAppSelector(
		selectedItemPreviouslyClockedMinutes
	);
	const itemEffortMinutes = useAppSelector(selectedItemEffortMinutes);
	const itemClockStartTime = useAppSelector(selectedItemClockStartTime);

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
		calculateMinutesClockedIn();
		const interval = setInterval(calculateMinutesClockedIn, 5000);
		return () => clearInterval(interval);
	}, [calculateMinutesClockedIn]);

	let overtime = false;
	if (
		itemEffortMinutes &&
		itemClockStartTime &&
		itemClockStartTime > itemEffortMinutes
	) {
		overtime = true;
	}

	return (
		<div className={styles.clock}>
			<span className={classNames({ [styles.overtime]: overtime })}>
				{minutesToTimeString(minutesClockedIn)}
			</span>
			{itemEffortMinutes && (
				<>
					{' / '}
					{minutesToTimeString(itemEffortMinutes)}
				</>
			)}
		</div>
	);
};

const OrgItem: React.FC = () => {
	const itemText = useAppSelector(selectedItemText);
	const tagColor = useAppSelector(selectedTagColor);
	const readyState = useAppSelector(selectedReadyState);
	const isWaitingForResponse = useAppSelector(selectedIsWaitingForResponse);
	const itemClockStartTime = useAppSelector(selectedItemClockStartTime);

	const isInSync = readyState === WSReadyState.OPEN && !isWaitingForResponse;

	const isClockedIn = itemClockStartTime !== null;

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
					{isClockedIn && isInSync && <ClockedTime />}
				</h1>
			) : (
				<img src={logo} className={styles.logo} alt="logo" />
			)}
		</>
	);
};

export default OrgItem;

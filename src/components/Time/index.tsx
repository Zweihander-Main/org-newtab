import * as styles from './style.module.css';
import { TIME_WARNING_THRESHOLD } from '../../lib/constants';
import { useAppSelector } from 'app/hooks';
import {
	selectedIsClockedIn,
	selectedItemClockStartTime,
	selectedItemEffortMinutes,
	selectedItemPreviouslyClockedMinutes,
} from 'modules/emacs/emacsSlice';
import { selectedIsInSync } from 'modules/ws/wsSlice';
import { useCallback, useEffect, useState } from 'react';

const ClockedTime: React.FC = () => {
	const isInSync = useAppSelector(selectedIsInSync);
	const isClockedIn = useAppSelector(selectedIsClockedIn);

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

	const determineOvertimeStrokeColor = useCallback(() => {
		if (
			itemEffortMinutes &&
			itemClockStartTime &&
			minutesClockedIn > itemEffortMinutes * TIME_WARNING_THRESHOLD
		) {
			if (minutesClockedIn > itemEffortMinutes) {
				return 'rgb(255, 0, 0)';
			} else {
				const numberOfWarningMinutes =
					itemEffortMinutes -
					itemEffortMinutes * TIME_WARNING_THRESHOLD;
				const minutesLeft = itemEffortMinutes - minutesClockedIn;
				const percentageLeft =
					1 -
					(numberOfWarningMinutes - minutesLeft) /
						numberOfWarningMinutes;
				const color = Math.floor(255 * percentageLeft);
				return `rgb(255, ${color}, ${color})`;
			}
		}
		return undefined;
	}, [itemEffortMinutes, itemClockStartTime, minutesClockedIn]);

	if (!isInSync || !isClockedIn) {
		return null;
	}

	return (
		<div
			className={styles.clock}
			style={{ WebkitTextStrokeColor: determineOvertimeStrokeColor() }}
			data-testid="clocked-time"
		>
			{minutesToTimeString(minutesClockedIn)}
			{itemEffortMinutes && (
				<>
					{' / '}
					{minutesToTimeString(itemEffortMinutes)}
				</>
			)}
		</div>
	);
};

export default ClockedTime;

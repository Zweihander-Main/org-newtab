import * as styles from './style.module.css';
import { useNProgress } from '@tanem/react-nprogress';
import { useAppSelector } from '../../app/hooks';
import { selectedIsWaitingForResponse } from 'modules/ws/wsSlice';
import { useEffect, useState } from 'react';

const Progress: React.FC<{
	animationDuration: number;
	isAnimating: boolean;
}> = ({ animationDuration, isAnimating }) => {
	const { progress, isFinished } = useNProgress({
		isAnimating,
		animationDuration,
	});

	return (
		<div
			data-testid="loading-bar"
			className={styles.outer}
			style={{
				visibility: isFinished ? 'hidden' : 'visible',
				marginLeft: `${(-1 + progress) * 100}%`,
				transition: `margin-left ${animationDuration}ms linear`,
			}}
		>
			<div className={styles.inner} />
		</div>
	);
};

const LoadingBar: React.FC = () => {
	const isWaitingForResponse = useAppSelector(selectedIsWaitingForResponse);
	const [loadingKey, setLoadingKey] = useState<number | null>(null);

	// Key needed to force re-render between very fast requests
	useEffect(() => {
		setLoadingKey((prev) => {
			if (!isWaitingForResponse) {
				return null;
			} else if (!prev) {
				return Math.floor(Math.random() * Number.MAX_SAFE_INTEGER);
			} else {
				return prev;
			}
		});
	}, [isWaitingForResponse]);

	return (
		<Progress
			key={loadingKey}
			animationDuration={200}
			isAnimating={isWaitingForResponse}
		/>
	);
};

export default LoadingBar;

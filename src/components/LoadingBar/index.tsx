import * as styles from './style.module.css';
import { useNProgress } from '@tanem/react-nprogress';
import { useAppSelector } from '../../app/hooks';
import { selectedIsWaitingForResponse } from 'modules/ws/wsSlice';

const LoadingBar: React.FC<{
	animationDuration: number;
}> = ({ animationDuration }) => {
	const isWaitingForResponse = useAppSelector(selectedIsWaitingForResponse);

	const { progress, isFinished } = useNProgress({
		isAnimating: isWaitingForResponse,
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

export default LoadingBar;

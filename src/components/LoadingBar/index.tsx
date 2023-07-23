import * as styles from './style.module.css';
import { useNProgress } from '@tanem/react-nprogress';
import { useAppSelector } from '../../hooks';

const LoadingBar: React.FC<{
	animationDuration: number;
}> = ({ animationDuration }) => {
	const isWaitingForResponse = useAppSelector(
		(state) => state.isWaitingForResponse
	);

	const { progress, isFinished } = useNProgress({
		isAnimating: isWaitingForResponse,
	});

	return (
		<div
			className={styles.outer}
			style={{
				opacity: isFinished ? 0 : 1,
				marginLeft: `${(-1 + progress) * 100}%`,
				transition: `margin-left ${animationDuration}ms linear`,
			}}
		>
			<div className={styles.inner} />
		</div>
	);
};

export default LoadingBar;

import * as styles from './style.module.css';
import { useNProgress } from '@tanem/react-nprogress';
import WSContext from 'contexts/ws';
import { useContext } from 'react';

const LoadingBar: React.FC<{
	animationDuration: number;
}> = ({ animationDuration }) => {
	const { isWaitingForResponse } = useContext(WSContext);

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

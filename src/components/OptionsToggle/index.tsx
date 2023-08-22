import * as styles from './style.module.css';
import classNames from 'classnames';

type OptionsButtonProps = {
	optionsVisible: boolean;
	toggleMenu: () => void;
};

const OptionsToggle: React.FC<OptionsButtonProps> = ({
	optionsVisible,
	toggleMenu,
}) => {
	return (
		<>
			<button
				aria-label={chrome.i18n.getMessage('optionsMenu')}
				className={classNames(styles.open, {
					[styles['is-visible']]: optionsVisible,
				})}
				onClick={toggleMenu}
				data-testid="options-open-button"
			>
				<div className={styles['open-bar1']}></div>
				<div className={styles['open-bar2']}></div>
				<div className={styles['open-bar3']}></div>
			</button>
			<button
				aria-label={chrome.i18n.getMessage('closeOptionsMenu')}
				className={classNames(styles.close, {
					[styles['is-visible']]: optionsVisible,
				})}
				onClick={toggleMenu}
				data-testid="options-close-button"
			>
				<div className={styles['close-bar1']}></div>
				<div className={styles['close-bar2']}></div>
			</button>
		</>
	);
};

export default OptionsToggle;

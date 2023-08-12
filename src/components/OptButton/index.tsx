import * as styles from './style.module.css';

type OptionsButtonProps = {
	optionsVisible: boolean;
	toggleMenu: () => void;
};

const OptButton: React.FC<OptionsButtonProps> = ({
	optionsVisible,
	toggleMenu,
}) => {
	const openButtonClass = [
		styles.open,
		optionsVisible ? styles['is-visible'] : '',
	].join(' ');

	const closeButtonClass = [
		styles.close,
		optionsVisible ? styles['is-visible'] : '',
	].join(' ');

	return (
		<>
			<button
				aria-label={chrome.i18n.getMessage('optionsMenu')}
				className={openButtonClass}
				onClick={toggleMenu}
			>
				<div className={styles['open-bar1']}></div>
				<div className={styles['open-bar2']}></div>
				<div className={styles['open-bar3']}></div>
			</button>
			<button
				aria-label={chrome.i18n.getMessage('closeOptionsMenu')}
				className={closeButtonClass}
				onClick={toggleMenu}
			>
				<div className={styles['close-bar1']}></div>
				<div className={styles['close-bar2']}></div>
			</button>
		</>
	);
};

export default OptButton;

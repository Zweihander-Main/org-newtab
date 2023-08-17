import * as styles from './style.module.css';
import { memo, useCallback, useState } from 'react';
import { useAppSelector } from '../../app/hooks';
import { CSSTransition, TransitionGroup } from 'react-transition-group';
import { OptionCategories, selectedOptionCategory } from 'modules/ui/uiSlice';
import BehaviorPanel from 'components/BehaviorPanel';
import LayoutPanel from 'components/LayoutPanel';
import ThemingPanel from 'components/ThemingPanel';
import DebugPanel from 'components/DebugPanel';
import OptionsBar from 'components/OptBar';
import OptionsButton from 'components/OptButton';
import classNames from 'classnames';

type OptionsPanelProps = {
	selectedCategory: OptionCategories;
};

const OptionsPanel: React.FC<OptionsPanelProps> = ({ selectedCategory }) => {
	const PanelToRender = useCallback(() => {
		switch (selectedCategory) {
			case 'Behavior':
				return <BehaviorPanel />;
			case 'Layout':
				return <LayoutPanel />;
			case 'Theming':
				return <ThemingPanel />;
			case 'Debug':
				return <DebugPanel />;
		}
	}, [selectedCategory]);
	return (
		<div className={styles.panel}>
			<PanelToRender />
		</div>
	);
};

const slideTransitionTimeout = 500;
const slideTransitionClassNames = {
	enter: styles['slide-enter'],
	enterActive: styles['slide-enter-active'],
	exit: styles['slide-exit'],
	exitActive: styles['slide-exit-active'],
};

const MemoizedCSSTransition = memo(CSSTransition, (prevProps, nextProps) => {
	return (
		prevProps.key === nextProps.key &&
		prevProps.in === nextProps.in &&
		prevProps.enter === nextProps.enter &&
		prevProps.exit === nextProps.exit
	);
});

const OptionsContent: React.FC = () => {
	const selectedCategory = useAppSelector(selectedOptionCategory);
	return (
		<div className={styles['content-container']}>
			<div className={styles['content']}>
				<TransitionGroup component={null}>
					<MemoizedCSSTransition
						key={selectedCategory}
						timeout={slideTransitionTimeout}
						classNames={slideTransitionClassNames}
						unmountOnExit
					>
						<OptionsPanel selectedCategory={selectedCategory} />
					</MemoizedCSSTransition>
				</TransitionGroup>
			</div>
		</div>
	);
};

const Options: React.FC = () => {
	const [menuVisible, setMenuVisible] = useState(false);
	const toggleMenu = useCallback(() => {
		setMenuVisible(!menuVisible);
	}, [menuVisible]);

	return (
		<>
			<OptionsButton
				optionsVisible={menuVisible}
				toggleMenu={toggleMenu}
			/>
			<div
				className={classNames(styles.menu, {
					[styles['is-visible']]: menuVisible,
				})}
			>
				<OptionsBar menuVisible={menuVisible} />
				<OptionsContent />
			</div>
		</>
	);
};

export default Options;

import * as styles from './style.module.css';
import { useAppDispatch, useAppSelector } from 'app/hooks';
import {
	OptionCategories,
	selectedOptionCategory,
	setOptCatTo,
} from 'modules/ui/uiSlice';
import { memo, useCallback } from 'react';
import {
	LuBrainCircuit,
	LuLayoutDashboard,
	LuPaintbrush,
	LuCode,
} from 'react-icons/lu';
import classNames from 'classnames';
import Icon from 'lib/Icon';

type OptButtonProps = {
	category: OptionCategories;
	isSelected: boolean;
	handleClick: (
		event: React.MouseEvent<HTMLButtonElement, MouseEvent>
	) => void;
	icon: React.ReactNode;
};

const OptButton: React.FC<OptButtonProps> = ({
	category,
	isSelected,
	handleClick,
	icon,
}) => {
	return (
		<button
			className={classNames(styles.button, {
				[styles['is-selected']]: isSelected,
			})}
			aria-label={chrome.i18n.getMessage(category.toLowerCase())}
			data-category={category}
			onClick={handleClick}
			data-testid={`${category.toLowerCase()}-button`}
		>
			<Icon icon={icon} />
			<span className={styles['button-label']}>
				{chrome.i18n.getMessage(category.toLowerCase())}
			</span>
		</button>
	);
};

const MemoizedOptButton = memo(OptButton, (prevProps, nextProps) => {
	return prevProps.isSelected === nextProps.isSelected;
});

type TabBarProps = {
	menuVisible: boolean;
};

const TabBar: React.FC<TabBarProps> = ({ menuVisible }) => {
	const dispatch = useAppDispatch();
	const selectedCategory = useAppSelector(selectedOptionCategory);
	const handleCategoryClick = useCallback(
		(event: React.MouseEvent<HTMLButtonElement, MouseEvent>) => {
			const { currentTarget } = event;
			const category = currentTarget.dataset.category as OptionCategories;
			if (category) {
				dispatch(setOptCatTo(category));
			}
		},
		[dispatch]
	);

	return (
		<nav
			className={classNames(styles.bar, styles.indicator, {
				[styles['is-visible']]: menuVisible,
				[styles['button-1-selected']]: selectedCategory === 'Behavior',
				[styles['button-2-selected']]: selectedCategory === 'Layout',
				[styles['button-3-selected']]: selectedCategory === 'Theming',
				[styles['button-4-selected']]: selectedCategory === 'Debug',
			})}
		>
			<MemoizedOptButton
				category="Behavior"
				isSelected={selectedCategory === 'Behavior'}
				handleClick={handleCategoryClick}
				icon={<LuBrainCircuit />}
			/>
			<MemoizedOptButton
				category="Layout"
				isSelected={selectedCategory === 'Layout'}
				handleClick={handleCategoryClick}
				icon={<LuLayoutDashboard />}
			/>
			<MemoizedOptButton
				category="Theming"
				isSelected={selectedCategory === 'Theming'}
				handleClick={handleCategoryClick}
				icon={<LuPaintbrush />}
			/>
			<MemoizedOptButton
				category="Debug"
				isSelected={selectedCategory === 'Debug'}
				handleClick={handleCategoryClick}
				icon={<LuCode />}
			/>
		</nav>
	);
};

export default TabBar;

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

const Icon: React.FC<{ icon: React.ReactNode }> = ({ icon }) => {
	return <>{icon}</>;
};

const MemoizedIcon = memo(Icon, () => true);

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
	const buttonClass = [
		styles.button,
		isSelected ? styles['is-selected'] : '',
	].join(' ');

	return (
		<button
			className={buttonClass}
			aria-label={chrome.i18n.getMessage(category.toLowerCase())}
			data-category={category}
			onClick={handleClick}
		>
			<MemoizedIcon icon={icon} />
			<span className={styles['button-label']}>
				{chrome.i18n.getMessage(category.toLowerCase())}
			</span>
		</button>
	);
};

const MemoizedOptButton = memo(OptButton, (prevProps, nextProps) => {
	return prevProps.isSelected === nextProps.isSelected;
});

type OptBarProps = {
	menuVisible: boolean;
};

const OptBar: React.FC<OptBarProps> = ({ menuVisible }) => {
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

	const barClass = [
		styles['bar'],
		styles['indicator'],
		menuVisible ? styles['is-visible'] : '',
	].join(' ');

	return (
		<nav className={barClass}>
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

export default OptBar;

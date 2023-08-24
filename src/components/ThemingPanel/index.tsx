import { useAppDispatch, useAppSelector } from 'app/hooks';
import * as styles from './style.module.css';
import { useCallback } from 'react';
import { HexColorPicker, HexColorInput } from 'react-colorful';
import {
	resetUntaggedItemBGColor,
	selectedUntaggedItemBGColor,
	setUntaggedItemBGColor,
} from 'modules/ui/uiSlice';
import Button from 'components/Button';

const ThemingPanel: React.FC = () => {
	const untaggedItemBG = useAppSelector(selectedUntaggedItemBGColor);
	const dispatch = useAppDispatch();

	const handleUntaggedUIItemBGColorChange = useCallback(
		(color: string) => {
			dispatch(setUntaggedItemBGColor(color));
		},
		[dispatch]
	);

	const handleUntaggedUIItemReset = useCallback(() => {
		dispatch(resetUntaggedItemBGColor());
	}, [dispatch]);

	return (
		<div className={styles.panel}>
			<label className={styles.label} htmlFor="untaggedItemBG">
				{chrome.i18n.getMessage('themingUntaggedItemBG')}
			</label>
			<div className={styles.picker}>
				<HexColorPicker
					id="untaggedItemBG"
					color={untaggedItemBG}
					onChange={handleUntaggedUIItemBGColorChange}
				/>
			</div>
			<HexColorInput
				color={untaggedItemBG}
				onChange={handleUntaggedUIItemBGColorChange}
			/>
			<Button
				styleType="reset"
				onClick={handleUntaggedUIItemReset}
				aria-label={chrome.i18n.getMessage(
					'themingResetUntaggedItemBG'
				)}
			>
				{chrome.i18n.getMessage('reset')}
			</Button>
		</div>
	);
};

// NEXT: Style as in figma (figure out how to style picker, put it box)

export default ThemingPanel;

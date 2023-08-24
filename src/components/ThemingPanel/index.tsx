import { useAppDispatch, useAppSelector } from 'app/hooks';
import * as styles from './style.module.css';
import { useCallback } from 'react';
import { HexColorPicker, HexColorInput } from 'react-colorful';
import {
	selectedUntaggedItemBGColor,
	setUntaggedItemBGColor,
} from 'modules/ui/uiSlice';

const ThemingPanel: React.FC = () => {
	const untaggedItemBG = useAppSelector(selectedUntaggedItemBGColor);
	const dispatch = useAppDispatch();

	const handleColorChange = useCallback(
		(color: string) => {
			dispatch(setUntaggedItemBGColor(color));
		},
		[dispatch]
	);
	return (
		<div className={styles.panel}>
			<label className={styles.label} htmlFor="untaggedItemBG">
				Untagged Item Background:
			</label>
			<div className={styles.picker}>
				<HexColorPicker
					id="untaggedItemBG"
					color={untaggedItemBG}
					onChange={handleColorChange}
				/>
			</div>
			<HexColorInput
				color={untaggedItemBG}
				onChange={handleColorChange}
			/>
		</div>
	);
};

export default ThemingPanel;

import { useAppDispatch, useAppSelector } from 'app/hooks';
import * as styles from './style.module.css';
import { useCallback, useMemo } from 'react';
import { HexColorPicker, HexColorInput } from 'react-colorful';
import {
	resetUntaggedItemBGColor,
	selectedUntaggedItemBGColor,
	setUntaggedItemBGColor,
} from 'modules/ui/uiSlice';
import Button from 'components/Button';

function pickTextColorBasedOnBgColor(bgColor: string) {
	const color = bgColor.substring(1, 7);
	const comps = (color.match(/.{1,2}/g) as Array<string>).map((comp) => {
		const color = parseInt(comp, 16) / 255;
		if (color <= 0.03928) {
			return color / 12.92;
		}
		return Math.pow((color + 0.055) / 1.055, 2.4);
	});
	const L = 0.2126 * comps[0] + 0.7152 * comps[1] + 0.0722 * comps[2];
	return L > 0.179 ? '#000000' : '#FFFFFF';
}

type ColorPickerProps = {
	color: string;
	onChange: (color: string) => void;
	onReset: () => void;
	ariaLabel: string;
};

const ColorPicker: React.FC<ColorPickerProps> = ({
	color,
	onChange,
	onReset,
	ariaLabel,
}) => {
	return (
		<div className={styles.picker}>
			<HexColorPicker color={color} onChange={onChange} />
			<Button
				small={true}
				className={styles.reset}
				styleType="reset"
				onClick={onReset}
				aria-label={ariaLabel}
			>
				{chrome.i18n.getMessage('reset')}
			</Button>
		</div>
	);
};

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

	const untaggedItemBGInputStyle = useMemo(() => {
		return { color: pickTextColorBasedOnBgColor(untaggedItemBG) };
	}, [untaggedItemBG]);

	return (
		<div className={styles.panel}>
			<label className={styles.label} htmlFor="input-untagged-item-bg">
				{chrome.i18n.getMessage('themingUntaggedItemBG')}
				{':'}
			</label>
			<div className={styles['input-container']}>
				<HexColorInput
					className={styles.input}
					id="input-untagged-item-bg"
					prefixed={true}
					color={untaggedItemBG}
					onChange={handleUntaggedUIItemBGColorChange}
					style={untaggedItemBGInputStyle}
				/>
				<ColorPicker
					color={untaggedItemBG}
					onChange={handleUntaggedUIItemBGColorChange}
					onReset={handleUntaggedUIItemReset}
					ariaLabel={chrome.i18n.getMessage('themingUntaggedItemBG')}
				/>
			</div>
		</div>
	);
};

export default ThemingPanel;

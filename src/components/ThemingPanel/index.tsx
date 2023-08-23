import * as styles from './style.module.css';
import { useState } from 'react';
import { HexColorPicker, HexColorInput } from 'react-colorful';

const ThemingPanel: React.FC = () => {
	const [untaggedItemBG, setUntaggedItemBG] = useState('#aabbcc');
	return (
		<div className={styles.panel}>
			<label className={styles.label} htmlFor="untaggeditembg">
				Untagged Item Background:
			</label>
			<div className={styles.picker}>
				<HexColorPicker
					id="untaggeditembg"
					color={untaggedItemBG}
					onChange={setUntaggedItemBG}
				/>
			</div>
			<HexColorInput
				color={untaggedItemBG}
				onChange={setUntaggedItemBG}
			/>
		</div>
	);
};

export default ThemingPanel;

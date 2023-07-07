import { useCallback, useEffect, useState } from 'react';
import * as styles from './style.module.css';
import useValue from 'hooks/useValue';
import type { AllTagsRecv } from 'types';

const OrgItem: React.FC = () => {
	const [tagsData] = useValue('tagsData');
	const [orgItem] = useValue('orgItem');
	const [foregroundColor, setForegroundColor] = useState<string | undefined>(
		undefined
	);
	const itemText = orgItem?.ITEM;

	const sanitizeTagsAndMatchData = useCallback(
		(allTagsData?: AllTagsRecv) => {
			let allTags: Array<string> | string | undefined;
			if (Array.isArray(allTagsData)) {
				allTags = [];
				allTagsData
					.filter(
						(tag): tag is string =>
							typeof tag === 'string' && tag !== ''
					)
					.forEach((tag) => {
						const splitTags = tag
							.split(':')
							.filter((tag) => tag !== '');
						(allTags as Array<string>).push(...splitTags);
					});
			} else {
				allTags = allTagsData?.split(':').filter((tag) => tag !== '');
			}
			const foundTag = allTags
				?.map((tag) => tag.replace(/^:(.*):$/i, '$1'))
				?.find((tag) => Object.keys(tagsData).includes(tag));
			return foundTag || undefined;
		},
		[tagsData]
	);

	useEffect(() => {
		const allTags = orgItem?.ALLTAGS || '';
		const foundTag = sanitizeTagsAndMatchData(allTags);
		setForegroundColor(foundTag);
	}, [sanitizeTagsAndMatchData, orgItem, tagsData]);

	return (
		<>
			{itemText && (
				<div
					className={styles.item}
					style={{ backgroundColor: foregroundColor }}
				>
					{itemText}
				</div>
			)}
		</>
	);
};

export default OrgItem;

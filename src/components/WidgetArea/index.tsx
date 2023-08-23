import * as styles from './style.module.css';
import {
	Area,
	selectedConnectionStatusArea,
	selectedOrgItemArea,
} from 'modules/layout/layoutSlice';
import classNames from 'classnames';
import { useAppSelector } from 'app/hooks';
import ConnectionStatusIndicator from 'components/ConnectionStatusIndicator';
import OrgItem from 'components/OrgItem';

type WidgetAreaProps = {
	loc: Area;
};

const WidgetArea: React.FC<WidgetAreaProps> = ({ loc }) => {
	const connectionStatusArea = useAppSelector(selectedConnectionStatusArea);
	const orgItemArea = useAppSelector(selectedOrgItemArea);

	return (
		<div
			className={classNames({
				[styles.bottom]: loc === Area.Bottom,
				[styles.mid]: loc === Area.Mid,
				[styles.top]: loc === Area.Top,
			})}
		>
			{loc === orgItemArea && <OrgItem />}
			{loc === connectionStatusArea && <ConnectionStatusIndicator />}
		</div>
	);
};

export default WidgetArea;

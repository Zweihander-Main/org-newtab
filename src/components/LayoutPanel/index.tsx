import * as styles from './style.module.css';
import React, { forwardRef, useCallback, useState } from 'react';
import {
	useDraggable,
	DndContext,
	DraggableSyntheticListeners,
	DragEndEvent,
	useDroppable,
	useDndContext,
	DragOverlay,
} from '@dnd-kit/core';
import { createPortal } from 'react-dom';
import classNames from 'classnames';
import {
	Area,
	resetLayout,
	selectedConnectionStatusArea,
	selectedOrgItemArea,
	setConnectionStatusAreaTo,
	setOrgItemAreaTo,
} from 'modules/layout/layoutSlice';
import { useAppDispatch, useAppSelector } from 'app/hooks';

type WidgetName = 'connection-status' | 'org-item';

enum WidgetText {
	ConnectionStatus = 'Connection Status',
	OrgItem = 'Org Item',
}

const WidgetTextMap: Record<WidgetName, WidgetText> = {
	'connection-status': WidgetText.ConnectionStatus,
	'org-item': WidgetText.OrgItem,
};
interface WidgetProps {
	name: WidgetName;
	dragOverlay?: boolean;
	dragging?: boolean;
	listeners?: DraggableSyntheticListeners;
	style?: React.CSSProperties;
}

const Widget = forwardRef<HTMLButtonElement, WidgetProps>(function Draggable(
	{ name, listeners, style, dragOverlay, dragging, ...props },
	ref
) {
	return (
		<div
			className={classNames(styles.widget, {
				[styles['widget-overlay']]: dragOverlay,
				[styles.dragging]: dragging,
			})}
			style={{
				...style,
			}}
		>
			<button {...props} aria-label="Draggable" {...listeners} ref={ref}>
				<p>{WidgetTextMap[name]}</p>
			</button>
		</div>
	);
});

// TODO: better labels

interface DraggableWidgetProps {
	name: WidgetName;
}

const DraggableWidget: React.FC<DraggableWidgetProps> = ({ name }) => {
	const { isDragging, setNodeRef, listeners } = useDraggable({
		id: name,
	});

	return (
		<Widget
			name={name}
			dragging={isDragging}
			ref={setNodeRef}
			listeners={listeners}
			style={{
				opacity: isDragging ? 0 : undefined,
			}}
		/>
	);
};
interface DropArea {
	dragging: boolean;
	area: Area;
}

const DropArea: React.FC<DropArea> = ({ area, dragging }) => {
	const connectionStatusArea = useAppSelector(selectedConnectionStatusArea);
	const orgItemStatusArea = useAppSelector(selectedOrgItemArea);
	const { isOver, setNodeRef } = useDroppable({
		id: area,
	});

	const areaHasChildren = useCallback(
		() => connectionStatusArea === area || orgItemStatusArea === area,
		[area, connectionStatusArea, orgItemStatusArea]
	);

	return (
		<div
			className={classNames(styles['area-drop-zone'], {
				[styles.dragging]: dragging,
				[styles.over]: isOver,
				[styles.dropped]: areaHasChildren(),
			})}
			ref={setNodeRef}
			aria-label="Droppable region"
		>
			{connectionStatusArea === area && (
				<DraggableWidget name="connection-status" />
			)}
			{orgItemStatusArea === area && <DraggableWidget name="org-item" />}
		</div>
	);
};

const WidgetOverlay: React.FC = () => {
	const { active } = useDndContext();

	return createPortal(
		<DragOverlay>
			{active ? (
				<Widget dragging dragOverlay name={active?.id as WidgetName} />
			) : null}
		</DragOverlay>,
		document.body
	);
};

const LayoutPanel: React.FC = () => {
	const [isDragging, setIsDragging] = useState(false);
	const dispatch = useAppDispatch();

	const handleDragStart = useCallback(() => {
		setIsDragging(true);
	}, []);

	const handleDragEnd = useCallback(
		({ over, active: { id } }: DragEndEvent) => {
			setIsDragging(false);
			if (over) {
				switch (id as WidgetName) {
					case 'connection-status':
						dispatch(setConnectionStatusAreaTo(over.id as Area));
						break;
					case 'org-item':
						dispatch(setOrgItemAreaTo(over.id as Area));
						break;
				}
			}
		},
		[dispatch]
	);

	const handleDragCancel = useCallback(() => {
		setIsDragging(false);
	}, []);

	const handleReset = useCallback(() => {
		dispatch(resetLayout());
	}, [dispatch]);

	return (
		<DndContext
			onDragStart={handleDragStart}
			onDragEnd={handleDragEnd}
			onDragCancel={handleDragCancel}
		>
			<div className={styles.map}>
				<div className={styles.area}>
					<DropArea
						key={Area.Top}
						area={Area.Top}
						dragging={isDragging}
					/>
				</div>
				<div className={styles.area}>
					<DropArea
						key={Area.Mid}
						area={Area.Mid}
						dragging={isDragging}
					/>
				</div>
				<div className={styles.area}>
					<DropArea
						key={Area.Bottom}
						area={Area.Bottom}
						dragging={isDragging}
					/>
				</div>
				<WidgetOverlay />
			</div>
			<button onClick={handleReset} aria-label="Reset">
				Reset
			</button>
		</DndContext>
	);
};

export default LayoutPanel;

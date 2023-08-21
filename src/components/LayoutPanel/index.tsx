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
	LayoutState,
	resetLayout,
	selectedWidgetsInArea,
	setWidgetAreaTo,
} from 'modules/layout/layoutSlice';
import { useAppDispatch, useAppSelector } from 'app/hooks';

type WidgetName = keyof LayoutState;

const WidgetTextMap: Record<WidgetName, string> = {
	connectionStatus: 'Connection Status',
	orgItem: 'Org Item',
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

// NEXT: better labels, use messaging API

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
	const widgetsInArea = useAppSelector(selectedWidgetsInArea(area));
	const { isOver, setNodeRef } = useDroppable({
		id: area,
	});

	const areaHasChildren = useCallback(
		() => widgetsInArea.length > 0,
		[widgetsInArea]
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
			{widgetsInArea.map((widget) => (
				<DraggableWidget key={widget} name={widget} />
			))}
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
				dispatch(
					setWidgetAreaTo({
						widget: id as WidgetName,
						area: over.id as Area,
					})
				);
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

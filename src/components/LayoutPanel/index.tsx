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
interface WidgetProps {
	children: React.ReactNode;
	dragOverlay?: boolean;
	dragging?: boolean;
	listeners?: DraggableSyntheticListeners;
	style?: React.CSSProperties;
}

const Widget = forwardRef<HTMLButtonElement, WidgetProps>(function Draggable(
	{ children, listeners, style, dragOverlay, dragging, ...props },
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
				{children}
			</button>
		</div>
	);
});

// TODO: better labels

interface DraggableWidgetProps {
	children: React.ReactNode;
	id: WidgetName;
}

const DraggableWidget: React.FC<DraggableWidgetProps> = ({ children, id }) => {
	const { isDragging, setNodeRef, listeners } = useDraggable({
		id,
	});

	return (
		<Widget
			dragging={isDragging}
			ref={setNodeRef}
			listeners={listeners}
			style={{
				opacity: isDragging ? 0 : undefined,
			}}
		>
			{children}
		</Widget>
	);
};

const ConnectionStatusText: React.FC = () => <p>Connection Status</p>;

const OrgItemText: React.FC = () => <p>Org Item</p>;

const ConnectionStatusDraggableWidget: React.FC = () => (
	<DraggableWidget id="connection-status">
		<ConnectionStatusText />
	</DraggableWidget>
);

const OrgItemDraggableWidget: React.FC = () => (
	<DraggableWidget id="org-item">
		<OrgItemText />
	</DraggableWidget>
);
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
				<ConnectionStatusDraggableWidget />
			)}
			{orgItemStatusArea === area && <OrgItemDraggableWidget />}
		</div>
	);
};

const WidgetOverlay: React.FC = () => {
	const { active } = useDndContext();
	const WidgetToRender = useCallback(() => {
		switch (active?.id) {
			case 'connection-status':
				return <ConnectionStatusDraggableWidget />;
			case 'org-item':
				return <OrgItemDraggableWidget />;
			default:
				return null;
		}
	}, [active]);

	return createPortal(
		<DragOverlay>
			{active ? (
				<Widget dragging dragOverlay>
					<WidgetToRender />
				</Widget>
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
				switch (id) {
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

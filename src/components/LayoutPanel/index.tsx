import * as styles from './style.module.css';
import { forwardRef, useCallback, useState } from 'react';
import {
	useDraggable,
	DndContext,
	UniqueIdentifier,
	DraggableSyntheticListeners,
	DragEndEvent,
	DragStartEvent,
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
interface DropArea {
	children: React.ReactNode;
	dragging: boolean;
	id: UniqueIdentifier;
}

const DropArea: React.FC<DropArea> = ({ children, id, dragging }) => {
	const { isOver, setNodeRef } = useDroppable({
		id,
	});

	const hasChildren = useCallback(() => {
		if (!children) {
			return false;
		}
		if (!Array.isArray(children)) {
			return true;
		}
		return (
			children.filter((child) => child !== null && child !== false)
				.length > 0
		);
	}, [children]);

	return (
		<div
			className={classNames(styles['area-drop-zone'], {
				[styles.dragging]: dragging,
				[styles.over]: isOver,
				[styles.dropped]: hasChildren(),
			})}
			ref={setNodeRef}
			aria-label="Droppable region"
		>
			{children}
		</div>
	);
};
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

interface WidgetOverlayProps {
	children: React.ReactNode;
}

const WidgetOverlay: React.FC<WidgetOverlayProps> = ({ children }) => {
	const { active } = useDndContext();

	return createPortal(
		<DragOverlay>
			{active ? (
				<Widget dragging dragOverlay>
					{children}
				</Widget>
			) : null}
		</DragOverlay>,
		document.body
	);
};

const LayoutPanel: React.FC = () => {
	const [isDragging, setIsDragging] = useState(false);
	const connectionStatusArea = useAppSelector(selectedConnectionStatusArea);
	const orgItemStatusArea = useAppSelector(selectedOrgItemArea);
	const dispatch = useAppDispatch();
	const [activeId, setActiveId] = useState<WidgetName | null>(null);

	const ConnStatusInner = useCallback(() => <p>Connection Status</p>, []);

	const OrgItemInner = useCallback(() => <p>Org Item</p>, []);

	const ConnStatusWidget = useCallback(
		() => (
			<DraggableWidget id="connection-status">
				<ConnStatusInner />
			</DraggableWidget>
		),
		[ConnStatusInner]
	);

	const OrgItemWidget = useCallback(
		() => (
			<DraggableWidget id="org-item">
				<OrgItemInner />
			</DraggableWidget>
		),
		[OrgItemInner]
	);

	const WidgetOverlayInner = useCallback(() => {
		switch (activeId) {
			case 'connection-status':
				return <ConnStatusWidget />;
			case 'org-item':
				return <OrgItemWidget />;
			default:
				return null;
		}
	}, [activeId, ConnStatusWidget, OrgItemWidget]);

	const handleDragStart = ({ active: { id } }: DragStartEvent) => {
		setIsDragging(true);
		setActiveId(id as WidgetName);
	};

	// TODO: clean this up considerably
	const handleDragEnd = ({ over, active: { id } }: DragEndEvent) => {
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
		} else {
			// TODO: visible
		}
		setActiveId(null);
	};

	const handleDragCancel = () => {
		setIsDragging(false);
		setActiveId(null);
	};

	const handleReset = () => {
		dispatch(resetLayout());
	};

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
						id={Area.Top}
						dragging={isDragging}
					>
						{connectionStatusArea === Area.Top && (
							<ConnStatusWidget />
						)}
						{orgItemStatusArea === Area.Top && <OrgItemWidget />}
					</DropArea>
					<p className={styles['area-label']}>Top</p>
				</div>
				<div className={styles.area}>
					<DropArea
						key={Area.Mid}
						id={Area.Mid}
						dragging={isDragging}
					>
						{connectionStatusArea === Area.Mid && (
							<ConnStatusWidget />
						)}
						{orgItemStatusArea === Area.Mid && <OrgItemWidget />}
					</DropArea>
					<p className={styles['area-label']}>Mid</p>
				</div>
				<div className={styles.area}>
					<DropArea
						key={Area.Bottom}
						id={Area.Bottom}
						dragging={isDragging}
					>
						{connectionStatusArea === Area.Bottom && (
							<ConnStatusWidget />
						)}
						{orgItemStatusArea === Area.Bottom && <OrgItemWidget />}
					</DropArea>
					<p className={styles['area-label']}>Bottom</p>
				</div>
				<WidgetOverlay>
					<WidgetOverlayInner />
				</WidgetOverlay>
			</div>
			<button onClick={handleReset} aria-label="Reset">
				Reset
			</button>
		</DndContext>
	);
};

export default LayoutPanel;

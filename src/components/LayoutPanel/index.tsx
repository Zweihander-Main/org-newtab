import { forwardRef, useState } from 'react';
import * as styles from './style.module.css';
import {
	useDraggable,
	DndContext,
	UniqueIdentifier,
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
	setConnectionStatusAreaTo,
} from 'modules/layout/layoutSlice';
import { useAppDispatch, useAppSelector } from 'app/hooks';

interface DropArea {
	children: React.ReactNode;
	dragging: boolean;
	id: UniqueIdentifier;
}

const DropArea: React.FC<DropArea> = ({ children, id, dragging }) => {
	const { isOver, setNodeRef } = useDroppable({
		id,
	});

	return (
		<div
			className={classNames(styles['area-drop-zone'], {
				[styles.dragging]: dragging,
				[styles.over]: isOver,
				[styles.dropped]: children,
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

interface DraggableWidgetProps {
	children: React.ReactNode;
}

const DraggableWidget: React.FC<DraggableWidgetProps> = ({ children }) => {
	const { isDragging, setNodeRef, listeners } = useDraggable({
		id: 'draggable-item',
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

type WidgetFactoryType = (children: React.ReactNode) => {
	Overlay: React.ReactNode;
	Widget: React.ReactNode;
};

const WidgetFactory: WidgetFactoryType = (children) => {
	const Overlay = <WidgetOverlay>{children}</WidgetOverlay>;
	const Widget = <DraggableWidget>{children}</DraggableWidget>;

	return { Overlay, Widget };
};

const LayoutPanel: React.FC = () => {
	const [isDragging, setIsDragging] = useState(false);
	const connectionStatusArea = useAppSelector(selectedConnectionStatusArea);
	const dispatch = useAppDispatch();

	const { Overlay: ConnStatusOverlay, Widget: ConnStatusWidget } =
		WidgetFactory(<p>Connection Status</p>);

	const handleDragStart = () => {
		setIsDragging(true);
	};

	const handleDragEnd = ({ over }: DragEndEvent) => {
		setIsDragging(false);
		if (over) {
			dispatch(setConnectionStatusAreaTo(over.id as Area));
		} else {
			// TODO: visible
		}
	};

	const handleDragCancel = () => {
		setIsDragging(false);
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
						{connectionStatusArea === Area.Top
							? ConnStatusWidget
							: null}
					</DropArea>
					<p className={styles['area-label']}>Top</p>
				</div>
				<div className={styles.area}>
					<DropArea
						key={Area.Mid}
						id={Area.Mid}
						dragging={isDragging}
					>
						{connectionStatusArea === Area.Mid
							? ConnStatusWidget
							: null}
					</DropArea>
					<p className={styles['area-label']}>Mid</p>
				</div>
				<div className={styles.area}>
					<DropArea
						key={Area.Bottom}
						id={Area.Bottom}
						dragging={isDragging}
					>
						{connectionStatusArea === Area.Bottom
							? ConnStatusWidget
							: null}
					</DropArea>
					<p className={styles['area-label']}>Bottom</p>
				</div>
				{ConnStatusOverlay}
			</div>
			<button onClick={handleReset} aria-label="Reset">
				Reset
			</button>
		</DndContext>
	);
};

// TODO: switch to message api

export default LayoutPanel;

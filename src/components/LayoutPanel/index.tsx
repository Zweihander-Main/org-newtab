import { forwardRef, useEffect, useState } from 'react';
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
	dragOverlay?: boolean;
	dragging?: boolean;
	listeners?: DraggableSyntheticListeners;
	style?: React.CSSProperties;
}

const Widget = forwardRef<HTMLButtonElement, WidgetProps>(function Draggable(
	{ listeners, style, dragOverlay, dragging, ...props },
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
				Drag Me
			</button>
		</div>
	);
});

const DraggableWidget: React.FC = () => {
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
		/>
	);
};

const WidgetOverlay: React.FC = () => {
	const { active } = useDndContext();

	return createPortal(
		<DragOverlay>
			{active ? <Widget dragging dragOverlay /> : null}
		</DragOverlay>,
		document.body
	);
};

const LayoutPanel: React.FC = () => {
	const [isDragging, setIsDragging] = useState(false);
	const [parent, setParent] = useState<UniqueIdentifier | null>(null);

	const widget = <DraggableWidget />;

	const handleDragStart = () => {
		setIsDragging(true);
	};

	const handleDragEnd = ({ over }: DragEndEvent) => {
		setIsDragging(false);
		setParent(over ? over.id : null);
	};

	const handleDragCancel = () => {
		setIsDragging(false);
	};

	useEffect(() => {
		setParent('bottom');
	}, []);

	return (
		<DndContext
			onDragStart={handleDragStart}
			onDragEnd={handleDragEnd}
			onDragCancel={handleDragCancel}
		>
			<div className={styles.map}>
				<div className={styles.area}>
					<DropArea key={'top'} id={'top'} dragging={isDragging}>
						{parent === 'top' ? widget : null}
					</DropArea>
					<p className={styles['area-label']}>Top</p>
				</div>
				<div className={styles.area}>
					<DropArea key={'mid'} id={'mid'} dragging={isDragging}>
						{parent === 'mid' ? widget : null}
					</DropArea>
					<p className={styles['area-label']}>Mid</p>
				</div>
				<div className={styles.area}>
					<DropArea
						key={'bottom'}
						id={'bottom'}
						dragging={isDragging}
					>
						{parent === 'bottom' ? widget : null}
					</DropArea>
					<p className={styles['area-label']}>Bottom</p>
				</div>
				<WidgetOverlay />
			</div>
		</DndContext>
	);
};

export default LayoutPanel;

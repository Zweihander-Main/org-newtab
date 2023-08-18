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
			className={classNames(styles['drop-area'], {
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
interface DraggableProps {
	dragOverlay?: boolean;
	dragging?: boolean;
	listeners?: DraggableSyntheticListeners;
	style?: React.CSSProperties;
}

const Draggable = forwardRef<HTMLButtonElement, DraggableProps>(
	function Draggable(
		{ listeners, style, dragOverlay, dragging, ...props },
		ref
	) {
		return (
			<div
				className={classNames(styles.draggable, {
					[styles['drag-overlay']]: dragOverlay,
					[styles.dragging]: dragging,
				})}
				style={{
					...style,
				}}
			>
				<button
					{...props}
					aria-label="Draggable"
					{...listeners}
					ref={ref}
				>
					Drag Me
				</button>
			</div>
		);
	}
);

const DraggableItem: React.FC = () => {
	const { isDragging, setNodeRef, listeners } = useDraggable({
		id: 'draggable-item',
	});

	return (
		<Draggable
			dragging={isDragging}
			ref={setNodeRef}
			listeners={listeners}
			style={{
				opacity: isDragging ? 0 : undefined,
			}}
		/>
	);
};

const DraggableOverlay: React.FC = () => {
	const { active } = useDndContext();

	return createPortal(
		<DragOverlay>
			{active ? <Draggable dragging dragOverlay /> : null}
		</DragOverlay>,
		document.body
	);
};

const LayoutPanel: React.FC = () => {
	const [isDragging, setIsDragging] = useState(false);
	const [parent, setParent] = useState<UniqueIdentifier | null>(null);

	const item = <DraggableItem />;

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
						{parent === 'top' ? item : null}
					</DropArea>
					<p>Top</p>
				</div>
				<div className={styles.area}>
					<DropArea key={'mid'} id={'mid'} dragging={isDragging}>
						{parent === 'mid' ? item : null}
					</DropArea>
					<p>Mid</p>
				</div>
				<div className={styles.area}>
					<DropArea
						key={'bottom'}
						id={'bottom'}
						dragging={isDragging}
					>
						{parent === 'bottom' ? item : null}
					</DropArea>
					<p>Bottom</p>
				</div>
				<DraggableOverlay />
			</div>
		</DndContext>
	);
};

export default LayoutPanel;

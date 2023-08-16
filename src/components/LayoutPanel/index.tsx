import { useState } from 'react';
import * as styles from './style.module.css';
import {
	useDroppable,
	useDraggable,
	DndContext,
	DragOverlay,
} from '@dnd-kit/core';

const Item: React.FC = () => {
	return (
		<div className={styles.component}>
			Connection Status Indicator
			<span className={styles.remove}>X</span>
		</div>
	);
};

type DraggableItemProps = {
	children: React.ReactNode;
	id: string;
	element?: React.ElementType;
};

const DraggableItem: React.FC<DraggableItemProps> = ({
	children,
	id,
	element,
}) => {
	const Element = element || 'div';
	const { attributes, listeners, setNodeRef } = useDraggable({
		id,
	});

	return (
		<Element ref={setNodeRef} {...listeners} {...attributes}>
			{children}
		</Element>
	);
};

const LayoutPanel: React.FC = () => {
	const { setNodeRef: setTopAreaDroppableRef } = useDroppable({
		id: 'top-area',
	});
	const { setNodeRef: setMidAreaDroppableRef } = useDroppable({
		id: 'mid-area',
	});
	const { setNodeRef: setBottomAreaDroppableRef } = useDroppable({
		id: 'bottom-area',
	});

	const [isDragging, setIsDragging] = useState(false);

	function handleDragStart() {
		setIsDragging(true);
	}

	function handleDragEnd() {
		setIsDragging(false);
	}

	return (
		<DndContext onDragStart={handleDragStart} onDragEnd={handleDragEnd}>
			<div className={styles.map}>
				<div ref={setTopAreaDroppableRef} className={styles.area}>
					Top
				</div>
				<div ref={setMidAreaDroppableRef} className={styles.area}>
					Mid
				</div>
				<div ref={setBottomAreaDroppableRef} className={styles.area}>
					<DraggableItem id="connection-status">
						<Item />
					</DraggableItem>
				</div>
				<DragOverlay>{isDragging ? <Item /> : null}</DragOverlay>
			</div>
		</DndContext>
	);
};

// import Checkbox from 'components/Checkbox';
// const LayoutPanel: React.FC = () => {
// 	return (
// 		<div className={styles.form}>
// 			<p className={styles.heading}>Connection status indicator:</p>
// 			<Checkbox label="Show connection status" />
// 		</div>
// 	);
// };

export default LayoutPanel;

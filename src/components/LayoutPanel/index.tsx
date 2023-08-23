import * as styles from './style.module.css';
import React, { forwardRef, useCallback, useState } from 'react';
import {
	useDraggable,
	DndContext,
	DraggableSyntheticListeners,
	DragEndEvent,
	useDroppable,
	DragOverlay,
	DragStartEvent,
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
import Button from 'components/Button';
import { RxDragHandleDots1 } from 'react-icons/rx';
import Icon from 'lib/Icon';

type WidgetName = keyof LayoutState;

const WidgetTextMap: Record<WidgetName, string> = {
	connectionStatus: chrome.i18n.getMessage('layoutWidgetConnectionStatus'),
	orgItem: chrome.i18n.getMessage('layoutWidgetOrgItem'),
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
			<button
				{...props}
				aria-label={`${chrome.i18n.getMessage(
					'layoutWidgetDraggableArea'
				)}: ${WidgetTextMap[name]}`}
				{...listeners}
				ref={ref}
			>
				<p className={styles['button-text']}>
					<Icon
						icon={
							<RxDragHandleDots1
								className={styles['drag-icon']}
							/>
						}
					/>
					{WidgetTextMap[name]}
				</p>
			</button>
		</div>
	);
});

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
	const widgetsInArea = useAppSelector((state) =>
		selectedWidgetsInArea(state, area)
	);
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
			aria-label={`${chrome.i18n.getMessage(
				'layoutWidgetDroppableArea'
			)}: ${area}`}
		>
			{widgetsInArea.map((widget) => (
				<DraggableWidget key={widget} name={widget} />
			))}
		</div>
	);
};

type WidgetOverlayProps = {
	isDragging: boolean;
	name: WidgetName;
};

const WidgetOverlay: React.FC<WidgetOverlayProps> = ({ isDragging, name }) => {
	return createPortal(
		<DragOverlay>
			{isDragging ? <Widget dragging dragOverlay name={name} /> : null}
		</DragOverlay>,
		document.body
	);
};

const LayoutPanel: React.FC = () => {
	const [isDragging, setIsDragging] = useState(false);
	const [draggingId, setDraggingId] = useState<WidgetName | null>(null);
	const dispatch = useAppDispatch();

	const handleDragStart = useCallback(
		({ active: { id } }: DragStartEvent) => {
			setDraggingId(id as WidgetName);
			setIsDragging(true);
		},
		[]
	);

	const handleDragEnd = useCallback(
		({ over, active: { id } }: DragEndEvent) => {
			setIsDragging(false);
			setDraggingId(null);
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
		setDraggingId(null);
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
			<div className={styles.maps}>
				<div className={styles['active-map']}>
					<p className={styles['map-label']}>
						{chrome.i18n.getMessage('layoutActive')}
						{':'}
					</p>
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
				</div>
				<div className={styles['inactive-map']}>
					<p className={styles['map-label']}>
						{chrome.i18n.getMessage('layoutInactive')}
						{':'}
					</p>
					<div className={styles.area}>
						<DropArea
							key={Area.None}
							area={Area.None}
							dragging={isDragging}
						/>
					</div>
				</div>
				<div className={styles.controls}>
					<Button
						styleType="reset"
						onClick={handleReset}
						aria-label={chrome.i18n.getMessage('layoutResetLabel')}
					>
						{chrome.i18n.getMessage('layoutReset')}
					</Button>
				</div>
				<WidgetOverlay
					isDragging={isDragging}
					name={draggingId || 'orgItem'}
				/>
			</div>
		</DndContext>
	);
};

export default LayoutPanel;

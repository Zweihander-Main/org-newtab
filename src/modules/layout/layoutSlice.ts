import { PayloadAction, createSlice } from '@reduxjs/toolkit';
import { RootState } from 'app/store';
import { Entries } from 'lib/types';

export enum Area {
	Top = 'top',
	Mid = 'mid',
	Bottom = 'bottom',
	None = 'none',
}

export type LayoutPos = {
	order: number;
	area: Area;
};

export interface LayoutState {
	connectionStatus: LayoutPos;
	orgItem: LayoutPos;
}

export const name = 'layout';
export const persistenceBlacklist: Array<keyof LayoutState> = [];

const initialState: LayoutState = {
	orgItem: {
		order: 0,
		area: Area.Mid,
	},
	connectionStatus: {
		order: 0,
		area: Area.Bottom,
	},
};

export const layoutSlice = createSlice({
	name,
	initialState,
	reducers: {
		setConnectionStatusAreaTo: (state, action: PayloadAction<Area>) => {
			state.connectionStatus.area = action.payload;
		},
		setOrgItemAreaTo: (state, action: PayloadAction<Area>) => {
			state.orgItem.area = action.payload;
		},
		setWidgetAreaTo(
			state,
			action: PayloadAction<{ widget: keyof LayoutState; area: Area }>
		) {
			state[action.payload.widget].area = action.payload.area;
		},
		resetLayout: () => initialState,
	},
});

// NEXT: move the name and logic for WidgetName here

export type LayoutSliceActions = keyof typeof layoutSlice.actions;

export const {
	setConnectionStatusAreaTo,
	setOrgItemAreaTo,
	setWidgetAreaTo,
	resetLayout,
} = layoutSlice.actions;

export const selectedConnectionStatusArea = (state: RootState) =>
	state.layout.connectionStatus.area;
export const selectedOrgItemArea = (state: RootState) =>
	state.layout.orgItem.area;
export const selectedWidgetsInArea = (area: Area) => {
	return (state: RootState) =>
		(Object.entries(state.layout) as Entries<typeof state.layout>)
			.filter(([, value]) => value.area === area)
			.map(([widget]) => widget);
};

export default layoutSlice.reducer;

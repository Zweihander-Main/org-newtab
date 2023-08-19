import { PayloadAction, createSlice } from '@reduxjs/toolkit';
import { RootState } from 'app/store';

export enum Area {
	Top = 'top',
	Mid = 'mid',
	Bottom = 'bottom',
}

export type LayoutPos = {
	visible: boolean;
	order: number;
	area: Area;
};

export interface LayoutState {
	connectionStatus: LayoutPos;
}

export const name = 'layout';
export const persistenceBlacklist: Array<keyof LayoutState> = [];

const initialState: LayoutState = {
	connectionStatus: {
		visible: true,
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
		resetLayout: () => initialState,
	},
});

export const { setConnectionStatusAreaTo, resetLayout } = layoutSlice.actions;

export const selectedConnectionStatusArea = (state: RootState) =>
	state.layout.connectionStatus.area;

export default layoutSlice.reducer;

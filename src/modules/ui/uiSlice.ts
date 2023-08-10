import { PayloadAction, createSlice } from '@reduxjs/toolkit';
import { RootState } from 'app/store';

export type OptionCategories = 'Behavior' | 'Layout' | 'Theming' | 'Debug';

export interface UIState {
	optionCategory: OptionCategories;
}

export const name = 'ui';
export const persistenceBlacklist: Array<keyof UIState> = ['optionCategory'];

const initialState: UIState = {
	optionCategory: 'Behavior',
};

export const uiSlice = createSlice({
	name,
	initialState,
	reducers: {
		setOptCatTo: (state, action: PayloadAction<OptionCategories>) => {
			state.optionCategory = action.payload;
		},
		selectBehaviorOptCat: (state) => {
			state.optionCategory = 'Behavior';
		},
		selectLayoutOptCat: (state) => {
			state.optionCategory = 'Layout';
		},
		selectThemingOptCat: (state) => {
			state.optionCategory = 'Theming';
		},
		selectDebugOptCat: (state) => {
			state.optionCategory = 'Debug';
		},
	},
});

export const {
	setOptCatTo,
	selectBehaviorOptCat,
	selectLayoutOptCat,
	selectThemingOptCat,
	selectDebugOptCat,
} = uiSlice.actions;

export const selectedOptionCategory = (state: RootState) =>
	state.ui.optionCategory;

export default uiSlice.reducer;

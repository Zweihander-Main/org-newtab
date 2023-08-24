import { Action, PayloadAction, createSlice } from '@reduxjs/toolkit';
import { listenerMiddleware } from 'app/middleware';
import { RootState } from 'app/store';
import { REHYDRATE } from '@plasmohq/redux-persist';

export type OptionCategories = 'Behavior' | 'Layout' | 'Theming' | 'Debug';

export interface UIState {
	optionCategory: OptionCategories;
	untaggedItemBGColor: string;
}

export const name = 'ui';
export const persistenceBlacklist: Array<keyof UIState> = ['optionCategory'];

const initialState: UIState = {
	optionCategory: 'Behavior',
	untaggedItemBGColor: '#484848',
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
		setUntaggedItemBGColor: (state, action: PayloadAction<string>) => {
			state.untaggedItemBGColor = action.payload;
		},
		resetUntaggedItemBGColor: (state) => {
			state.untaggedItemBGColor = initialState.untaggedItemBGColor;
		},
	},
});

export const {
	setOptCatTo,
	selectBehaviorOptCat,
	selectLayoutOptCat,
	selectThemingOptCat,
	selectDebugOptCat,
	setUntaggedItemBGColor,
	resetUntaggedItemBGColor,
} = uiSlice.actions;

export const selectedOptionCategory = (state: RootState) =>
	state.ui.optionCategory;
export const selectedUntaggedItemBGColor = (state: RootState) =>
	state.ui.untaggedItemBGColor;

export default uiSlice.reducer;

interface RehydrateUIState extends Action<typeof REHYDRATE> {
	payload: UIState;
}

/**
 * Set the item background color for untagged items (or items which have tags
 * that don't have corresponding colors). Will also be called on rehydrate.
 */
listenerMiddleware.startListening({
	predicate: (action) =>
		(action.type === REHYDRATE && action.key === 'ui') ||
		action.type === setUntaggedItemBGColor.type,
	effect: (action) => {
		const { payload } = action as RehydrateUIState;
		const { untaggedItemBGColor } = payload;
		document.documentElement.style.setProperty(
			'--color-untagged-item-bg',
			untaggedItemBGColor
		);
	},
});

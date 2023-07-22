import type { EmacsItemMsg } from 'lib/types';
import { PayloadAction, createSlice } from '@reduxjs/toolkit';

type Tags = { [key: string]: string };
type OrgItem = EmacsItemMsg['data'];

export interface AppState {
	matchQuery: string | undefined;
	tagsData: Tags | null;
	orgItem: OrgItem | null;
}

const INITIAL_VALUE: AppState = {
	matchQuery: 'TODO="TODO"',
	tagsData: {},
	orgItem: null,
};

const appSlice = createSlice({
	name: 'app',
	initialState: INITIAL_VALUE,
	reducers: {
		changeMatchQuery: (state, action: PayloadAction<string>) => {
			state.matchQuery = action.payload;
		},
		changeTagsData: (state, action: PayloadAction<Tags>) => {
			state.tagsData = action.payload;
		},
		changeOrgItem: (state, action: PayloadAction<OrgItem>) => {
			state.orgItem = action.payload;
		},
	},
});

export const { changeMatchQuery, changeTagsData, changeOrgItem } =
	appSlice.actions;

export default appSlice.reducer;

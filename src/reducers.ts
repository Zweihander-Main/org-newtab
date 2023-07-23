import type { EmacsItemMsg } from 'lib/types';
import { PayloadAction, createSlice } from '@reduxjs/toolkit';

type MatchQuery = string | undefined;
type Tags = { [key: string]: string | null };
type OrgItem = EmacsItemMsg['data'] | null;

export interface AppState {
	matchQuery: MatchQuery;
	tagsData: Tags;
	orgItem: OrgItem;
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
		setMatchQueryTo: (state, action: PayloadAction<string>) => {
			state.matchQuery = action.payload;
		},
		setTagsDataTo: (state, action: PayloadAction<Tags>) => {
			state.tagsData = action.payload;
		},
		setOrgItemTo: (state, action: PayloadAction<OrgItem>) => {
			state.orgItem = action.payload;
		},
	},
});

export const { setMatchQueryTo, setTagsDataTo, setOrgItemTo } =
	appSlice.actions;

export default appSlice.reducer;

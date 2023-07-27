import { PayloadAction, createSlice } from '@reduxjs/toolkit';
import { RootState } from 'app/store';
import { type EmacsItemMsg } from 'lib/types';

type MatchQuery = string | undefined;
type Tags = { [key: string]: string | null };
type OrgItem = EmacsItemMsg['data'] | null;

export interface EmacsState {
	matchQuery: MatchQuery;
	tagsData: Tags;
	orgItem: OrgItem;
}

const initialState: EmacsState = {
	matchQuery: 'TODO="TODO"',
	tagsData: {},
	orgItem: null,
};

const emacsSlice = createSlice({
	name: 'emacs',
	initialState,
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

export const selectedMatchQuery = (state: RootState) => state.emacs.matchQuery;
export const selectedTagsData = (state: RootState) => state.emacs.tagsData;
export const selectedOrgItem = (state: RootState) => state.emacs.orgItem;

export const { setMatchQueryTo, setTagsDataTo, setOrgItemTo } =
	emacsSlice.actions;

export default emacsSlice.reducer;

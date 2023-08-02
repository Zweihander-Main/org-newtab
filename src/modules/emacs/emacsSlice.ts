import { PayloadAction, createSlice } from '@reduxjs/toolkit';
import { listenerMiddleware } from 'app/middleware';
import { RootState } from 'app/store';
import { EmacsSendMsg, type EmacsItemMsg } from 'lib/types';

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
		getItem: () => {},
		// eslint-disable-next-line @typescript-eslint/no-unused-vars
		sendMsgToEmacs: (_state, _action: PayloadAction<EmacsSendMsg>) => {},
	},
});

export const selectedMatchQuery = (state: RootState) => state.emacs.matchQuery;
export const selectedTagsData = (state: RootState) => state.emacs.tagsData;
export const selectedOrgItem = (state: RootState) => state.emacs.orgItem;

export const {
	setMatchQueryTo,
	setTagsDataTo,
	setOrgItemTo,
	getItem,
	sendMsgToEmacs,
} = emacsSlice.actions;

listenerMiddleware.startListening({
	actionCreator: setMatchQueryTo,
	effect: (action, listenerApi) => {
		const { dispatch } = listenerApi;
		const matchQuery = action.payload;
		const jsonMessage = {
			command: 'updateMatchQuery',
			data: matchQuery,
		} as EmacsSendMsg;
		dispatch(sendMsgToEmacs(jsonMessage));
	},
});

listenerMiddleware.startListening({
	actionCreator: getItem,
	effect: (_action, listenerApi) => {
		const { dispatch } = listenerApi;
		const getState = listenerApi.getState.bind(this);
		const { matchQuery } = getState().emacs;
		const jsonMessage = {
			command: 'getItem',
			data: matchQuery,
		} as EmacsSendMsg;
		dispatch(sendMsgToEmacs(jsonMessage));
	},
});

export default emacsSlice.reducer;

/* eslint-disable @typescript-eslint/no-unused-vars */
import { PayloadAction, createSlice, isAnyOf } from '@reduxjs/toolkit';
import { listenerMiddleware } from 'app/middleware';
import { RootState } from 'app/store';
import { EmacsSendMsg, type EmacsItemMsg, EmacsRecvMsg } from 'lib/types';

type MatchQuery = string | undefined;
type Tags = { [key: string]: string | null };
type OrgItem = EmacsItemMsg['data'] | null;

export interface EmacsState {
	matchQuery: MatchQuery;
	tagsData: Tags;
	orgItem: OrgItem;
}

export const name = 'emacs';
export const persistenceBlacklist: Array<keyof EmacsState> = [];

const initialState: EmacsState = {
	matchQuery: 'TODO="TODO"',
	tagsData: {},
	orgItem: null,
};

const emacsSlice = createSlice({
	name,
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
		_sendMsgToEmacs: (_state, _action: PayloadAction<EmacsSendMsg>) => {},
		_recvMsgFromEmacs: (state, action: PayloadAction<EmacsRecvMsg>) => {
			const { payload } = action;
			if (payload === null) return;
			switch (payload.type) {
				case 'ITEM':
					state.orgItem = payload?.data || null;
					break;
				case 'TAGS':
					state.tagsData = payload?.data || {};
					break;
				default:
					console.error('[NewTab] Unknown message: ', payload);
					break;
			}
		},
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
	_sendMsgToEmacs,
	_recvMsgFromEmacs,
} = emacsSlice.actions;

export default emacsSlice.reducer;

listenerMiddleware.startListening({
	matcher: isAnyOf(getItem, setMatchQueryTo),
	effect: (_action, listenerApi) => {
		const { dispatch } = listenerApi;
		const getState = listenerApi.getState.bind(this);
		const { matchQuery } = getState().emacs;
		const jsonMessage = {
			command: 'getItem',
			data: matchQuery,
		} as EmacsSendMsg;
		dispatch(_sendMsgToEmacs(jsonMessage));
	},
});

// TODO: update query and get item do nothing different

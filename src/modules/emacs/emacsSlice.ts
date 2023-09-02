/* eslint-disable @typescript-eslint/no-unused-vars */
import {
	PayloadAction,
	createSelector,
	createSlice,
	isAnyOf,
} from '@reduxjs/toolkit';
import { listenerMiddleware } from 'app/middleware';
import { RootState } from 'app/store';
import { EmacsSendMsg, EmacsRecvMsg, AllTagsRecv } from 'lib/types';

type MatchQuery = string | undefined;
type Tags = { [key: string]: string | null };
type ItemText = string | null;
type ItemTags = Array<string>;

export interface EmacsState {
	matchQuery: MatchQuery;
	tagsData: Tags;
	itemText: ItemText;
	itemTags: ItemTags;
}

export const name = 'emacs';
export const persistenceBlacklist: Array<keyof EmacsState> = [];

const initialState: EmacsState = {
	matchQuery: 'TODO="TODO"',
	tagsData: {},
	itemText: null,
	itemTags: [],
};

const extractTagsFromItemAllTags = (allTagsData?: AllTagsRecv): ItemTags => {
	let allTags: Array<string> | string | undefined;
	if (Array.isArray(allTagsData)) {
		allTags = [];
		allTagsData
			.filter(
				(tag): tag is string => typeof tag === 'string' && tag !== ''
			)
			.forEach((tag) => {
				const splitTags = tag.split(':').filter((tag) => tag !== '');
				(allTags as Array<string>).push(...splitTags);
			});
	} else {
		allTags = allTagsData?.split(':').filter((tag) => tag !== '');
	}
	return allTags || [];
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
		getItem: () => {},
		_sendMsgToEmacs: (_state, _action: PayloadAction<EmacsSendMsg>) => {},
		_recvMsgFromEmacs: (state, action: PayloadAction<EmacsRecvMsg>) => {
			const { payload } = action;
			if (payload === null) return;
			switch (payload.type) {
				case 'ITEM':
					state.itemText = payload?.data?.ITEM || null;
					state.itemTags = extractTagsFromItemAllTags(
						payload?.data?.ALLTAGS
					);
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
export const selectedItemText = (state: RootState) => state.emacs.itemText;
export const selectedItemTags = (state: RootState) => state.emacs.itemTags;
export const selectedTagColor = createSelector(
	[selectedItemTags, selectedTagsData],
	(itemTags, tagsData) => {
		const foundTag = itemTags
			?.map((tag) => tag.replace(/^:(.*):$/i, '$1'))
			?.find((tag) => Object.keys(tagsData).includes(tag));
		return foundTag ? tagsData[foundTag] : null;
	}
);

export const {
	setMatchQueryTo,
	setTagsDataTo,
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

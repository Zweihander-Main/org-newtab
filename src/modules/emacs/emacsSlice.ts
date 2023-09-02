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
type TagFaces = { [key: string]: string | null };
type ItemText = string | null;
type ItemTags = Array<string>;
type ItemClockStartTimeMS = number | null;
type ItemPreviouslyClockedMinutes = number;

export interface EmacsState {
	matchQuery: MatchQuery;
	tagFaces: TagFaces;
	itemText: ItemText;
	itemTags: ItemTags;
	itemClockStartTimeMS: ItemClockStartTimeMS;
	itemPreviouslyClockedMinutes: ItemPreviouslyClockedMinutes;
}

export const name = 'emacs';
export const persistenceBlacklist: Array<keyof EmacsState> = [];

const initialState: EmacsState = {
	matchQuery: 'TODO="TODO"',
	tagFaces: {},
	itemText: null,
	itemTags: [],
	itemClockStartTimeMS: null,
	itemPreviouslyClockedMinutes: 0,
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
		setTagsDataTo: (state, action: PayloadAction<TagFaces>) => {
			state.tagFaces = action.payload;
		},
		getItem: () => {},
		_sendMsgToEmacs: (_state, _action: PayloadAction<EmacsSendMsg>) => {},
		_recvMsgFromEmacs: (state, action: PayloadAction<EmacsRecvMsg>) => {
			const { payload } = action;
			if (payload === null) return;
			const itemClockStartTimeSec =
				payload?.data?.CURRENT_CLOCK_START_TIMESTAMP;
			switch (payload.type) {
				case 'ITEM':
					state.itemText = payload?.data?.ITEM || null;
					state.itemTags = extractTagsFromItemAllTags(
						payload?.data?.ALLTAGS
					);
					state.itemClockStartTimeMS = itemClockStartTimeSec
						? new Date(
								parseInt(itemClockStartTimeSec, 10) * 1000
						  ).getTime()
						: null;
					state.itemPreviouslyClockedMinutes =
						payload?.data?.PREVIOUSLY_CLOCKED_MINUTES || 0;
					break;
				case 'TAGS':
					state.tagFaces = payload?.data || {};
					break;
				default:
					console.error('[NewTab] Unknown message: ', payload);
					break;
			}
		},
	},
});

// TODO: previously clocked minutes not accurate if item clock start time not sent

export const selectedMatchQuery = (state: RootState) => state.emacs.matchQuery;
export const selectedTagsData = (state: RootState) => state.emacs.tagFaces;
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
export const selectedItemClockStartTimeMS = (state: RootState) =>
	state.emacs.itemClockStartTimeMS;
export const selectedItemPreviouslyClockedMinutes = (state: RootState) =>
	state.emacs.itemPreviouslyClockedMinutes;

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

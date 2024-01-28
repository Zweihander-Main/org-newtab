/* eslint-disable @typescript-eslint/no-unused-vars */
import { PayloadAction, createSelector, createSlice } from '@reduxjs/toolkit';
import { listenerMiddleware } from 'app/middleware';
import { resetData } from 'app/actions';
import { RootState } from 'app/store';
import { EmacsSendMsg, EmacsRecvMsg, AllTagsRecv } from 'lib/types';

type MatchQuery = string | undefined;
type TagFaces = { [key: string]: string | null };
type ItemText = string | null;
type ItemTags = Array<string>;
type ItemClockStartTime = number | null;
type ItemPreviouslyClockedMinutes = number;
type ItemEffortMinutes = number | null;

export interface EmacsState {
	matchQuery: MatchQuery;
	tagFaces: TagFaces;
	itemText: ItemText;
	itemTags: ItemTags;
	itemClockStartTime: ItemClockStartTime;
	itemPreviouslyClockedMinutes: ItemPreviouslyClockedMinutes;
	itemEffortMinutes: ItemEffortMinutes;
}

export const name = 'emacs';
export const persistenceBlacklist: Array<keyof EmacsState> = [];

const initialState: EmacsState = {
	matchQuery: 'TODO="TODO"',
	tagFaces: {},
	itemText: null,
	itemTags: [],
	itemClockStartTime: null,
	itemPreviouslyClockedMinutes: 0,
	itemEffortMinutes: null,
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
	extraReducers: (builder) => builder.addCase(resetData, () => initialState),
	reducers: {
		setMatchQueryTo: (state, action: PayloadAction<string>) => {
			state.matchQuery = action.payload;
		},
		setTagsDataTo: (state, action: PayloadAction<TagFaces>) => {
			return { ...state, tagFaces: action.payload };
		},
		getItem: () => {},
		_sendMsgToEmacs: (_state, _action: PayloadAction<EmacsSendMsg>) => {},
		_recvMsgFromEmacs: (state, action: PayloadAction<EmacsRecvMsg>) => {
			const { payload } = action;
			if (payload === null) return;
			switch (payload.type) {
				case 'ITEM':
					return {
						...state,
						itemText: payload?.data?.ITEM || null,
						itemTags: extractTagsFromItemAllTags(
							payload?.data?.ALLTAGS
						),
						itemClockStartTime:
							payload?.data?.CURRENT_CLOCK_START_TIMESTAMP ||
							null,
						itemPreviouslyClockedMinutes:
							payload?.data?.PREVIOUSLY_CLOCKED_MINUTES || 0,
						itemEffortMinutes:
							payload?.data?.EFFORT_MINUTES || null,
					};
					break;
				case 'TAGS':
					return {
						...state,
						tagFaces: payload?.data || {},
					};
					break;
				default:
					console.error('[NewTab] Unknown message: ', payload);
					return;
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
export const selectedItemClockStartTime = (state: RootState) =>
	state.emacs.itemClockStartTime;
export const selectedItemPreviouslyClockedMinutes = (state: RootState) =>
	state.emacs.itemPreviouslyClockedMinutes;
export const selectedItemEffortMinutes = (state: RootState) =>
	state.emacs.itemEffortMinutes;
export const selectedIsClockedIn = (state: RootState) =>
	state.emacs.itemClockStartTime;

export const {
	setMatchQueryTo,
	setTagsDataTo,
	getItem,
	_sendMsgToEmacs,
	_recvMsgFromEmacs,
} = emacsSlice.actions;

export default emacsSlice.reducer;

/**
 * Get the current item from Emacs on request
 */
listenerMiddleware.startListening({
	actionCreator: getItem,
	effect: (_action, listenerApi) => {
		const { dispatch } = listenerApi;
		const getState = listenerApi.getState.bind(this);
		const {
			emacs: { matchQuery },
		} = getState();
		const jsonMessage = {
			command: 'getItem',
			data: matchQuery,
		} as EmacsSendMsg;
		dispatch(_sendMsgToEmacs(jsonMessage));
	},
});

/**
 * Ask for current item if the match query changes
 */
listenerMiddleware.startListening({
	actionCreator: setMatchQueryTo,
	effect: (action, listenerApi) => {
		const { dispatch } = listenerApi;
		const {
			emacs: { matchQuery: prevMatchQuery },
		} = listenerApi.getOriginalState();
		const currMatchQuery = action.payload;
		if (prevMatchQuery !== currMatchQuery) {
			dispatch(getItem());
		}
	},
});

import type { EmacsItemMsg, EmacsSendMsg } from 'lib/types';
import { PayloadAction, createSlice } from '@reduxjs/toolkit';
import { ReadyState } from 'react-use-websocket';

type MatchQuery = string | undefined;
type Tags = { [key: string]: string | null };
type OrgItem = EmacsItemMsg['data'] | null;

export interface AppState {
	matchQuery: MatchQuery;
	tagsData: Tags;
	orgItem: OrgItem;
	amMasterWS: boolean;
	readyState: ReadyState;
	isWaitingForResponse: boolean;
}

const INITIAL_VALUE: AppState = {
	matchQuery: 'TODO="TODO"',
	tagsData: {},
	orgItem: null,
	amMasterWS: false,
	readyState: ReadyState.UNINSTANTIATED,
	isWaitingForResponse: false,
};

export const appSlice = createSlice({
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
		becomeMasterWS: (state) => {
			state.amMasterWS = true;
		},
		becomeClientWS: (state) => {
			state.amMasterWS = false;
		},
		setReadyStateTo: (state, action: PayloadAction<ReadyState>) => {
			state.readyState = action.payload;
		},
		startWaitingForResponse: (state) => {
			state.isWaitingForResponse = true;
		},
		stopWaitingForResponse: (state) => {
			state.isWaitingForResponse = false;
		},
		// eslint-disable-next-line @typescript-eslint/no-unused-vars
		sendMsgToEmacs: (_state, _action: PayloadAction<EmacsSendMsg>) => {
			// TODO start waiting
		},
	},
});

// NEXT: unsafe eval -- build your own

export const {
	setMatchQueryTo,
	setTagsDataTo,
	setOrgItemTo,
	becomeMasterWS,
	becomeClientWS,
	setReadyStateTo,
	startWaitingForResponse,
	stopWaitingForResponse,
	sendMsgToEmacs,
} = appSlice.actions;

export default appSlice.reducer;

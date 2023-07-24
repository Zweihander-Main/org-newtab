import { WSReadyState, type EmacsItemMsg, type EmacsSendMsg } from 'lib/types';
import { PayloadAction, createSlice } from '@reduxjs/toolkit';

type MatchQuery = string | undefined;
type Tags = { [key: string]: string | null };
type OrgItem = EmacsItemMsg['data'] | null;

export interface AppState {
	matchQuery: MatchQuery;
	tagsData: Tags;
	orgItem: OrgItem;
	amMasterWS: boolean;
	readyState: WSReadyState;
	responsesWaitingFor: Array<number>;
}

const INITIAL_VALUE: AppState = {
	matchQuery: 'TODO="TODO"',
	tagsData: {},
	orgItem: null,
	amMasterWS: false,
	readyState: WSReadyState.UNINSTANTIATED,
	responsesWaitingFor: [],
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
		setReadyStateTo: (state, action: PayloadAction<WSReadyState>) => {
			state.readyState = action.payload;
		},
		removeFromResponsesWaitingFor: (
			state,
			action: PayloadAction<number>
		) => {
			state.responsesWaitingFor = state.responsesWaitingFor.filter(
				(id) => id !== action.payload
			);
		},
		addToResponsesWaitingFor: (state, action: PayloadAction<number>) => {
			state.responsesWaitingFor.push(action.payload);
		},
		setResponsesWaitingForTo: (
			state,
			action: PayloadAction<Array<number>>
		) => {
			state.responsesWaitingFor = action.payload;
		},
		// eslint-disable-next-line @typescript-eslint/no-unused-vars
		sendMsgToEmacs: (_state, _action: PayloadAction<EmacsSendMsg>) => {},
	},
});

export const {
	setMatchQueryTo,
	setTagsDataTo,
	setOrgItemTo,
	becomeMasterWS,
	becomeClientWS,
	setReadyStateTo,
	removeFromResponsesWaitingFor,
	addToResponsesWaitingFor,
	setResponsesWaitingForTo,
	sendMsgToEmacs,
} = appSlice.actions;

export default appSlice.reducer;

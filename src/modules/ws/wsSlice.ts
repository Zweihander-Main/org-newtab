import { WSReadyState, type EmacsSendMsg } from 'lib/types';
import { PayloadAction, createSlice } from '@reduxjs/toolkit';
import { RootState } from 'app/store';

export interface WSState {
	amMasterWS: boolean;
	readyState: WSReadyState;
	responsesWaitingFor: Array<number>;
}

const initialState: WSState = {
	amMasterWS: false,
	readyState: WSReadyState.UNINSTANTIATED,
	responsesWaitingFor: [],
};

export const wsSlice = createSlice({
	name: 'ws',
	initialState,
	reducers: {
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
		getItem: () => {},
		establishRole: () => {},
	},
});

// NEXT: move  updatematchquery here but remember that middleware also sets
// updatematchquery
// Create listeners to updatewsstate
// Run through listeners carefully
// Write test to make sure client doesn't send data

export const {
	becomeMasterWS,
	becomeClientWS,
	setReadyStateTo,
	removeFromResponsesWaitingFor,
	addToResponsesWaitingFor,
	setResponsesWaitingForTo,
	sendMsgToEmacs,
	getItem,
	establishRole,
} = wsSlice.actions;

export const selectedReadyState = (state: RootState) => state.ws.readyState;
export const selectedAmMasterWs = (state: RootState) => state.ws.amMasterWS;
export const selectedIsWaitingForResponse = (state: RootState) =>
	state.ws.responsesWaitingFor.length > 0;
export const selectedResponsesWaitingFor = (state: RootState) =>
	state.ws.responsesWaitingFor;

export default wsSlice.reducer;

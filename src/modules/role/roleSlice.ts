import { createSlice } from '@reduxjs/toolkit';
import { listenerMiddleware } from 'app/middleware';
import { RootState } from 'app/store';
import Port from 'lib/Port';
import {
	getMasterWSTabId,
	sendMsgToBGSWPort,
	sendMsgToTab,
	sendUpdateInWSState,
} from 'lib/messages';
import { MsgToBGSWType, MsgToTabType, WSReadyState } from 'lib/types';
import { getItem } from 'modules/emacs/emacsSlice';
import {
	_addToResponsesWaitingFor,
	_closeWS,
	_openWS,
	_removeFromResponsesWaitingFor,
	_resetWS,
	_setReadyStateTo,
	_setResponsesWaitingForTo,
	setWSPortTo,
} from 'modules/ws/wsSlice';

/**
 * Role is defined as master or client and refers to the websocket connection.
 * The Master websocket is the one that actually talks to Emacs while the client
 * is a dummy connection that talks through the master. This is done to avoid
 * multiple websocket connections to Emacs.
 */

export interface RoleState {
	amMasterRole: boolean;
	stateResolved: boolean;
}

const initialState: RoleState = {
	amMasterRole: false,
	stateResolved: false,
};

export const name = 'role';
export const persistenceBlacklist: Array<keyof RoleState> = Object.keys(
	initialState
) as Array<keyof RoleState>;

export const roleSlice = createSlice({
	name,
	initialState,
	reducers: {
		establishRole: () => {},
		setStateAsResolved: (state) => {
			state.stateResolved = true;
		},
		_becomeMasterRole: (state) => {
			state.amMasterRole = true;
		},
		_becomeClientRole: (state) => {
			state.amMasterRole = false;
		},
	},
});
export const {
	establishRole,
	setStateAsResolved,
	_becomeMasterRole,
	_becomeClientRole,
} = roleSlice.actions;

export const selectedAmMasterRole = (state: RootState) =>
	state.role.amMasterRole;
export const selectedStateResolved = (state: RootState) =>
	state.role.stateResolved;

export default roleSlice.reducer;

/**
 * Open websocket when role becomes master and state is resolved.
 * The role is necessary to avoid multiple websocket connections to Emacs. The
 * state is necessary to allow custom websocket ports.
 */
listenerMiddleware.startListening({
	predicate: (action, currentState, originalState) =>
		(action.type === _becomeMasterRole.type &&
			!originalState.role.amMasterRole &&
			currentState.role.stateResolved) ||
		(action.type === setStateAsResolved && currentState.role.amMasterRole),
	effect: (_action, listenerApi) => {
		const { dispatch } = listenerApi;
		dispatch(_openWS());
	},
});

/**
 * Close websocket when role becomes client.
 */
listenerMiddleware.startListening({
	actionCreator: _becomeClientRole,
	effect: (_action, listenerApi) => {
		const { dispatch } = listenerApi;
		dispatch(_closeWS());
	},
});

/**
 * Everytime the port changes, restart the websocket as master or let the
 * master know it needs to. The master will not restart without the message
 * from client as the side effect is not triggered without an action (which the
 * storage sync doesn't trigger).
 */
listenerMiddleware.startListening({
	actionCreator: setWSPortTo,
	effect: (action, listenerApi) => {
		const { dispatch } = listenerApi;
		const getState = listenerApi.getState.bind(this);
		const {
			role: { amMasterRole: amMasterWS },
		} = getState();
		if (amMasterWS) {
			dispatch(_resetWS());
		} else {
			void getMasterWSTabId().then((masterWSTabNum) => {
				if (masterWSTabNum) {
					const port = action.payload;
					sendMsgToTab(MsgToTabType.SET_WS_PORT, masterWSTabNum, {
						port,
					});
				}
			});
		}
	},
});

/**
 * As master, send updates in readyState to other tabs
 * (not persisted in storage)
 */
listenerMiddleware.startListening({
	predicate: (action, currentState) =>
		currentState.role.amMasterRole && action.type === _setReadyStateTo.type,
	effect: (_action, listenerApi) => {
		const getState = listenerApi.getState.bind(this);
		const { readyState } = getState().ws;
		sendUpdateInWSState({ readyState });
	},
});

/**
 * As master, send updates in responsesWaitingFor to other tabs
 * (not persisted in storage)
 */
listenerMiddleware.startListening({
	predicate: (action, currentState) =>
		currentState.role.amMasterRole &&
		(action.type === _removeFromResponsesWaitingFor.type ||
			action.type === _addToResponsesWaitingFor.type ||
			action.type === _setResponsesWaitingForTo.type),
	effect: (_action, listenerApi) => {
		const getState = listenerApi.getState.bind(this);
		const { responsesWaitingFor } = getState().ws;
		sendUpdateInWSState({ responsesWaitingFor });
	},
});

/**
 * Every time the websocket opens, ask Emacs for the current item
 * (assuming master role)
 */
listenerMiddleware.startListening({
	predicate: (action, currentState) =>
		action.type === _setReadyStateTo.type &&
		currentState.role.amMasterRole &&
		currentState.ws.readyState === WSReadyState.OPEN,
	effect: (_action, listenerApi) => {
		const { dispatch } = listenerApi;
		const getState = listenerApi.getState.bind(this);
		const {
			emacs: { matchQuery },
		} = getState();
		if (matchQuery) {
			dispatch(getItem());
		}
	},
});

/**
 * Establish role using BGSW port. Further messages are sent into the
 * message slice/handler.
 */
listenerMiddleware.startListening({
	actionCreator: establishRole,
	effect: () => {
		sendMsgToBGSWPort(MsgToBGSWType.QUERY_WS_ROLE, Port.port);
	},
});

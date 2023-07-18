import useWebSocket from 'react-use-websocket';
import type {
	JsonValue,
	SendJsonMessage,
} from 'react-use-websocket/dist/lib/types';
import {
	type WSCommonProps,
	type EmacsRecvMsg,
	MsgToTabType,
} from '../lib/types';
import useValue from './useValue';
import { useCallback, useState } from 'react';
import { sendMsgToTab } from '../lib/messages';

type useSingleWebsocket = () => WSCommonProps & {
	amMasterWS: boolean;
	setAmMasterWS: (amMasterWS: boolean) => void;
};

const useSingleWebsocket: useSingleWebsocket = () => {
	const [amMasterWS, setAmMasterWS] = useState(false);
	let sendJsonMessage: SendJsonMessage = useCallback(
		(jsonMessage: JsonValue) => {
			chrome.storage.local
				.get('masterWSTabId')
				.then((masterWSObject) => {
					const { masterWSTabId } = masterWSObject;
					const masterWSTabAsNumber =
						masterWSTabId && typeof masterWSTabId === 'string'
							? parseInt(masterWSTabId, 10)
							: null;
					if (masterWSTabAsNumber) {
						sendMsgToTab(
							MsgToTabType.PASS_ON_TO_EMACS,
							masterWSTabAsNumber,
							jsonMessage
						);
					}
				})
				.catch((err) => {
					console.error(err);
				});
		},
		[]
	);

	let lastRecvJsonMessage: EmacsRecvMsg = null;
	const { setValue: setReadyState } = useValue('readyState');

	const {
		sendJsonMessage: sendJsonMessageMaster,
		lastJsonMessage: lastRecvJsonMessageMaster,
		readyState: readyStateMaster,
	} = useWebSocket<EmacsRecvMsg>(amMasterWS ? 'ws://localhost:35942/' : null);

	const setAmMasterWSWrapper = useCallback(
		(newValue: boolean) => {
			if (amMasterWS !== newValue) {
				setAmMasterWS(newValue);
			}
		},
		[amMasterWS, setAmMasterWS]
	);

	if (amMasterWS) {
		sendJsonMessage = sendJsonMessageMaster;
		lastRecvJsonMessage = lastRecvJsonMessageMaster;
		setReadyState(readyStateMaster);
	}

	return {
		sendJsonMessage,
		lastRecvJsonMessage,
		amMasterWS,
		setAmMasterWS: setAmMasterWSWrapper,
	};
};

export default useSingleWebsocket;

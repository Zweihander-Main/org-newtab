import useWebSocket from 'react-use-websocket';
import {
	type WSCommonProps,
	type EmacsRecvMsg,
	MsgToTabType,
	EmacsSendMsg,
	sendJsonMessage,
	EmacsSendMsgWithResid,
} from '../lib/types';
import useValue from './useValue';
import { useCallback, useEffect, useState } from 'react';
import { sendMsgToTab } from '../lib/messages';

const MAXIMUM_TIME_TO_WAIT_FOR_RESPONSE = 10000;

const sendJsonMessageWrapper = (
	sendJsonMessage: sendJsonMessage,
	addToResponsesWaitingFor: (resid: number) => void
) => {
	const wrappedFunc: sendJsonMessage = (jsonMessage: EmacsSendMsg) => {
		const resid = Math.floor(Math.random() * 1000000000);
		sendJsonMessage({
			...jsonMessage,
			resid,
		} as EmacsSendMsgWithResid);
		addToResponsesWaitingFor(resid);
	};
	return wrappedFunc;
};

type useSingleWebsocket = () => WSCommonProps & {
	setAmMasterWS: (amMasterWS: boolean) => void;
};

const useSingleWebsocket: useSingleWebsocket = () => {
	const [amMasterWS, setAmMasterWS] = useState(false);
	let lastRecvJsonMessage: EmacsRecvMsg = null;
	const { setValue: setReadyState } = useValue('readyState');

	const [responsesWaitingFor, setResponsesWaitingFor] = useState<
		Array<number>
	>([]);

	const { setValue: setIsWaitingForResponse } = useValue(
		'isWaitingForResponse'
	);

	const removeFromResponsesWaitingFor = useCallback((resid: number) => {
		setResponsesWaitingFor((prevValue) =>
			prevValue.filter((value) => value !== resid)
		);
	}, []);

	const addToResponsesWaitingFor = useCallback(
		(resid: number) => {
			setResponsesWaitingFor((prevValue) => [...prevValue, resid]);
			setTimeout(() => {
				removeFromResponsesWaitingFor(resid);
			}, MAXIMUM_TIME_TO_WAIT_FOR_RESPONSE);
		},
		[removeFromResponsesWaitingFor]
	);

	let sendJsonMessage: sendJsonMessage = useCallback(
		(jsonMessage: EmacsSendMsg) => {
			void chrome.storage.local
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
				});
		},
		[]
	);

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

	useEffect(() => {
		if (
			amMasterWS &&
			lastRecvJsonMessage &&
			lastRecvJsonMessage.type === 'ITEM' &&
			lastRecvJsonMessage.resid
		) {
			const lastMessageId = lastRecvJsonMessage.resid;
			removeFromResponsesWaitingFor(lastMessageId);
		}
	}, [lastRecvJsonMessage, amMasterWS, removeFromResponsesWaitingFor]);

	useEffect(() => {
		if (!amMasterWS) return;
		if (responsesWaitingFor.length > 0) {
			setIsWaitingForResponse(true);
		} else {
			setIsWaitingForResponse(false);
		}
	}, [amMasterWS, responsesWaitingFor, setIsWaitingForResponse]);

	useEffect(() => {
		if (!amMasterWS) return;
		setReadyState(readyStateMaster);
	}, [amMasterWS, readyStateMaster, setReadyState]);

	if (amMasterWS) {
		sendJsonMessage = sendJsonMessageMaster;
		lastRecvJsonMessage = lastRecvJsonMessageMaster;
	}

	sendJsonMessage = sendJsonMessageWrapper(
		sendJsonMessage,
		addToResponsesWaitingFor
	);

	return {
		sendJsonMessage,
		lastRecvJsonMessage,
		amMasterWS,
		setAmMasterWS: setAmMasterWSWrapper,
	};
};

export default useSingleWebsocket;

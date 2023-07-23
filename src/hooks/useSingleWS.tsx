import useWebSocket from 'react-use-websocket';
import {
	type WSCommonProps,
	type EmacsRecvMsg,
	MsgToTabType,
	EmacsSendMsg,
	sendJsonMessage,
	EmacsSendMsgWithResid,
} from '../lib/types';
import { useCallback, useEffect, useState } from 'react';
import { sendMsgToTab, sendUpdateInWSState } from '../lib/messages';
import { useAppDispatch, useAppSelector } from '../hooks';
import {
	setReadyStateTo,
	startWaitingForResponse,
	stopWaitingForResponse,
} from '../stateReducer';

const MAXIMUM_TIME_TO_WAIT_FOR_RESPONSE = 60000;

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

type useSingleWebsocket = () => WSCommonProps;

const useSingleWebsocket: useSingleWebsocket = () => {
	const dispatch = useAppDispatch();
	const amMasterWS = useAppSelector((state) => state.amMasterWS);
	let lastRecvJsonMessage: EmacsRecvMsg = null;

	const [responsesWaitingFor, setResponsesWaitingFor] = useState<
		Array<number>
	>([]);

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
							MsgToTabType.PASS_TO_EMACS,
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
			dispatch(startWaitingForResponse());
			sendUpdateInWSState({ isWaitingForResponse: true });
		} else {
			dispatch(stopWaitingForResponse());
			sendUpdateInWSState({ isWaitingForResponse: false });
		}
	}, [amMasterWS, dispatch, responsesWaitingFor]);

	useEffect(() => {
		if (!amMasterWS) return;
		dispatch(setReadyStateTo(readyStateMaster));
		sendUpdateInWSState({ readyState: readyStateMaster });
	}, [amMasterWS, dispatch, readyStateMaster]);

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
	};
};

export default useSingleWebsocket;

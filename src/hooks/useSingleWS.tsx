import {
	type WSCommonProps,
	MsgToTabType,
	EmacsSendMsg,
	sendJsonMessage,
	EmacsSendMsgWithResid,
} from '../lib/types';
import { useCallback, useEffect, useState } from 'react';
import { sendMsgToTab, sendUpdateInWSState } from '../lib/messages';
import { useAppDispatch, useAppSelector } from '../hooks';
import {
	sendMsgToEmacs,
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
	const readyState = useAppSelector((state) => state.readyState);

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
			if (amMasterWS) {
				dispatch(sendMsgToEmacs(jsonMessage));
			} else {
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
			}
		},
		[amMasterWS, dispatch]
	);

	// NEXT: fix this
	// useEffect(() => {
	// 	if (
	// 		amMasterWS &&
	// 		lastRecvJsonMessage &&
	// 		lastRecvJsonMessage.type === 'ITEM' &&
	// 		lastRecvJsonMessage.resid
	// 	) {
	// 		const lastMessageId = lastRecvJsonMessage.resid;
	// 		removeFromResponsesWaitingFor(lastMessageId);
	// 	}
	// }, [lastRecvJsonMessage, amMasterWS, removeFromResponsesWaitingFor]);

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
		sendUpdateInWSState({ readyState });
	}, [amMasterWS, readyState]);

	sendJsonMessage = sendJsonMessageWrapper(
		sendJsonMessage,
		addToResponsesWaitingFor
	);

	return {
		sendJsonMessage,
	};
};

export default useSingleWebsocket;

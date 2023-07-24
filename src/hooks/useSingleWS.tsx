import {
	type WSCommonProps,
	MsgToTabType,
	EmacsSendMsg,
	sendJsonMessage,
	EmacsSendMsgWithResid,
} from '../lib/types';
import { useCallback, useEffect } from 'react';
import { sendMsgToTab, sendUpdateInWSState } from '../lib/messages';
import { useAppDispatch, useAppSelector } from '../hooks';
import { addToResponsesWaitingFor, sendMsgToEmacs } from '../stateReducer';

type useSingleWebsocket = () => WSCommonProps;

const useSingleWebsocket: useSingleWebsocket = () => {
	const dispatch = useAppDispatch();
	const amMasterWS = useAppSelector((state) => state.amMasterWS);
	const readyState = useAppSelector((state) => state.readyState);
	const responsesWaitingFor = useAppSelector(
		(state) => state.responsesWaitingFor
	);

	const sendJsonMessage: sendJsonMessage = useCallback(
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

	const sendJsonMessageWrapper = useCallback(
		(sendJsonMessage: sendJsonMessage) => {
			const wrappedFunc: sendJsonMessage = (
				jsonMessage: EmacsSendMsg
			) => {
				const resid = Math.floor(Math.random() * 1000000000);
				sendJsonMessage({
					...jsonMessage,
					resid,
				} as EmacsSendMsgWithResid);
				dispatch(addToResponsesWaitingFor(resid));
			};
			return wrappedFunc;
		},
		[dispatch]
	);

	useEffect(() => {
		if (!amMasterWS) return;
		sendUpdateInWSState({ responsesWaitingFor });
	}, [amMasterWS, responsesWaitingFor]);

	useEffect(() => {
		if (!amMasterWS) return;
		sendUpdateInWSState({ readyState });
	}, [amMasterWS, readyState]);

	return {
		sendJsonMessage: sendJsonMessageWrapper(sendJsonMessage),
	};
};

export default useSingleWebsocket;

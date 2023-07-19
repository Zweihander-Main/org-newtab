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

	const [isWaitingForResponse, setIsWaitingForResponse] = useState(false);

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
				setResponsesWaitingFor((prevValue) => [...prevValue, resid]);
			};
			return wrappedFunc;
		},
		[]
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

	if (amMasterWS) {
		sendJsonMessage = sendJsonMessageMaster;
		lastRecvJsonMessage = lastRecvJsonMessageMaster;
		setReadyState(readyStateMaster);
	}

	useEffect(() => {
		if (
			amMasterWS &&
			lastRecvJsonMessage &&
			lastRecvJsonMessage.type === 'ITEM' &&
			lastRecvJsonMessage.resid
		) {
			const lastMessageId = lastRecvJsonMessage.resid;
			setResponsesWaitingFor((prevValue) => {
				const newValue = prevValue.filter(
					(resid) => resid !== lastMessageId
				);
				return newValue;
			});
		}
	}, [lastRecvJsonMessage, amMasterWS]);

	useEffect(() => {
		if (responsesWaitingFor.length > 0) {
			setIsWaitingForResponse(true);
		} else {
			setIsWaitingForResponse(false);
		}
	}, [responsesWaitingFor]);

	return {
		sendJsonMessage: sendJsonMessageWrapper(sendJsonMessage),
		lastRecvJsonMessage,
		amMasterWS,
		setAmMasterWS: setAmMasterWSWrapper,
		isWaitingForResponse,
	};
};

export default useSingleWebsocket;

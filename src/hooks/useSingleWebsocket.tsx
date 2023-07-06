import useWebSocket from 'react-use-websocket';
import type { SendJsonMessage } from 'react-use-websocket/dist/lib/types';
import type { WSCommonProps, WSRecvMsg } from 'types';
import { ReadyState } from 'react-use-websocket';
import { useChromeStorageLocal } from 'use-chrome-storage';

type useSingleWebsocket = (amMasterWS: boolean) => WSCommonProps;

const useSingleWebsocket: useSingleWebsocket = (amMasterWS: boolean) => {
	let sendJsonMessage: SendJsonMessage = () => {
		return;
	};
	let lastRecvJsonMessage: WSRecvMsg = null;
	const [readyState, setReadyState] = useChromeStorageLocal(
		'readyState',
		ReadyState.UNINSTANTIATED
	);

	const {
		sendJsonMessage: sendJsonMessageMaster,
		lastJsonMessage: lastRecvJsonMessageMaster,
		readyState: readyStateMaster,
	} = useWebSocket<WSRecvMsg>(amMasterWS ? 'ws://localhost:35942/' : null);

	if (amMasterWS) {
		sendJsonMessage = sendJsonMessageMaster;
		lastRecvJsonMessage = lastRecvJsonMessageMaster;
		setReadyState(readyStateMaster);
	}

	return { sendJsonMessage, lastRecvJsonMessage, readyState };
};

export default useSingleWebsocket;

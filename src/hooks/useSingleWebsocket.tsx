import useWebSocket from 'react-use-websocket';
import type { SendJsonMessage } from 'react-use-websocket/dist/lib/types';
import type { WSCommonProps, WebSocketRecvMessage } from 'types';
import { ReadyState } from 'react-use-websocket';

type useSingleWebsocket = (amMasterWS: boolean) => WSCommonProps;

const useSingleWebsocket: useSingleWebsocket = (amMasterWS: boolean) => {
	let sendJsonMessage: SendJsonMessage = () => {
		return;
	};
	let lastRecvJsonMessage: WebSocketRecvMessage = null;
	let readyState: ReadyState = ReadyState.UNINSTANTIATED;

	const {
		sendJsonMessage: sendJsonMessageMaster,
		lastJsonMessage: lastRecvJsonMessageMaster,
		readyState: readyStateMaster,
	} = useWebSocket<WebSocketRecvMessage>(
		amMasterWS ? 'ws://localhost:35942/' : null
	);

	if (amMasterWS) {
		sendJsonMessage = sendJsonMessageMaster;
		lastRecvJsonMessage = lastRecvJsonMessageMaster;
		readyState = readyStateMaster;
	}
	return { sendJsonMessage, lastRecvJsonMessage, readyState };
};

export default useSingleWebsocket;

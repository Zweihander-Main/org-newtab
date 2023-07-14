import useWebSocket from 'react-use-websocket';
import type { SendJsonMessage } from 'react-use-websocket/dist/lib/types';
import type { WSCommonProps, EmacsRecvMsg } from '../util/types';
import useValue from './useValue';

type useSingleWebsocket = (amMasterWS: boolean) => WSCommonProps;

const useSingleWebsocket: useSingleWebsocket = (amMasterWS: boolean) => {
	let sendJsonMessage: SendJsonMessage = () => {
		return;
	};
	let lastRecvJsonMessage: EmacsRecvMsg = null;
	const { value: readyState, setValue: setReadyState } =
		useValue('readyState');

	const {
		sendJsonMessage: sendJsonMessageMaster,
		lastJsonMessage: lastRecvJsonMessageMaster,
		readyState: readyStateMaster,
	} = useWebSocket<EmacsRecvMsg>(amMasterWS ? 'ws://localhost:35942/' : null);

	if (amMasterWS) {
		sendJsonMessage = sendJsonMessageMaster;
		lastRecvJsonMessage = lastRecvJsonMessageMaster;
		setReadyState(readyStateMaster);
	}

	return { sendJsonMessage, lastRecvJsonMessage, readyState };
};

export default useSingleWebsocket;

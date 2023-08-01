import { createContext, useEffect, useRef } from 'react';
import { sendUpdateInWSState } from '../lib/messages';
import { useAppDispatch, useAppSelector } from '../app/hooks';
import {
	establishRole,
	selectedAmMasterWs,
	selectedReadyState,
	selectedResponsesWaitingFor,
} from '../modules/ws/wsSlice';

export type WSContextProps = Record<string, never>;

const WSContext = createContext<WSContextProps>({});

export default WSContext;

export const WSProvider: React.FC<{ children?: React.ReactNode }> = ({
	children,
}) => {
	const dispatch = useAppDispatch();
	const amMasterWS = useAppSelector(selectedAmMasterWs);
	const readyState = useAppSelector(selectedReadyState);
	const responsesWaitingFor = useAppSelector(selectedResponsesWaitingFor);

	const isInitialRender = useRef(true);

	useEffect(() => {
		if (isInitialRender.current) {
			isInitialRender.current = false;
			dispatch(establishRole());
		}
	}, [dispatch]);

	useEffect(() => {
		if (!amMasterWS) return;
		sendUpdateInWSState({ responsesWaitingFor });
	}, [amMasterWS, responsesWaitingFor]);

	useEffect(() => {
		if (!amMasterWS) return;
		sendUpdateInWSState({ readyState });
	}, [amMasterWS, readyState]);

	return <WSContext.Provider value={{}}>{children}</WSContext.Provider>;
};

export const { Consumer: WSConsumer } = WSContext;

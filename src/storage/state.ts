import { ReadyState } from 'react-use-websocket';
import { Storage } from '@plasmohq/storage';

export interface State {
	matchQuery: string | undefined;
	tagsData: { [key: string]: string };
	itemText: string | null;
	readyState: ReadyState;
}

export const defaultState: State = {
	matchQuery: undefined,
	tagsData: {},
	itemText: null,
	readyState: ReadyState.UNINSTANTIATED,
};

export const db = new Storage({ area: 'local' });

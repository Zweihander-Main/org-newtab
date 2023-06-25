/* eslint-disable no-console */
import type { PlasmoMessaging } from '@plasmohq/messaging';

export type RequestBody = string;

export type RequestResponse = string;

const handler: PlasmoMessaging.PortHandler<RequestBody, RequestResponse> = (
	req,
	res
) => {
	console.log('Data recv on bg end: ', req);
	res.send('Hello from the background script!');
};

export default handler;

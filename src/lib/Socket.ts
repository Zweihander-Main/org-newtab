import ReconnectingWebSocket from 'reconnecting-websocket';
import { RECONNECTION_ATTEMPT_GROWTH_FACTOR } from './constants';

const WebSocketOptions = {
	reconnectionDelayGrowFactor: RECONNECTION_ATTEMPT_GROWTH_FACTOR,
	debug: true,
};
class Socket {
	private static _instance: Socket;
	private _socket: ReconnectingWebSocket | null;

	private constructor() {
		if (Socket._instance) {
			throw new Error('[NewTab] Use Socket.Instance() instead of new.');
		}
		Socket._instance = this;
		this._socket = null;
	}

	public static getInstance() {
		return this._instance || (this._instance = new this());
	}

	public connect(url: string) {
		if (!this._socket) {
			this._socket = new ReconnectingWebSocket(url, [], WebSocketOptions);
		}
	}

	public disconnect() {
		if (this._socket) {
			this._socket.close();
			this._socket = null;
		}
	}

	public sendJSON(message: Record<string, unknown>) {
		if (this._socket) {
			this._socket.send(JSON.stringify(message));
		}
	}

	public on<T extends keyof WebSocketEventMap>(
		eventName: T,
		listener: Parameters<
			typeof ReconnectingWebSocket.prototype.addEventListener<T>
		>[1]
	) {
		if (this._socket) {
			this._socket.addEventListener<T>(eventName, listener);
		}
	}
}

export default Socket.getInstance();

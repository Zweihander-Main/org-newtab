import ReconnectingWebSocket from 'reconnecting-websocket';
import { RECONNECTION_ATTEMPT_GROWTH_FACTOR } from './constants';

const WebSocketOptions = {
	reconnectionDelayGrowFactor: RECONNECTION_ATTEMPT_GROWTH_FACTOR,
	debug: false,
};
class Socket {
	static #instance: Socket;
	#socket: ReconnectingWebSocket | null;

	private constructor() {
		if (Socket.#instance) {
			throw new Error('[NewTab] Use Socket.Instance() instead of new.');
		}
		Socket.#instance = this;
		this.#socket = null;
	}

	public static getInstance() {
		return this.#instance || (this.#instance = new this());
	}

	public connect(url: string) {
		if (!this.#socket) {
			this.#socket = new ReconnectingWebSocket(url, [], WebSocketOptions);
		}
	}

	public disconnect() {
		if (this.#socket) {
			this.#socket.close();
			this.#socket = null;
		}
	}

	public sendJSON(message: Record<string, unknown>) {
		if (this.#socket) {
			this.#socket.send(JSON.stringify(message));
		}
	}

	public on<T extends keyof WebSocketEventMap>(
		eventName: T,
		listener: Parameters<
			typeof ReconnectingWebSocket.prototype.addEventListener<T>
		>[1]
	) {
		if (this.#socket) {
			this.#socket.addEventListener<T>(eventName, listener);
		}
	}

	public get exists() {
		return this.#socket !== null;
	}
}

export default Socket.getInstance();

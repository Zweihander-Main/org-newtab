class Socket {
	private static _instance: Socket;
	private _socket: WebSocket | null;

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
			this._socket = new WebSocket(url);
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
		callback: (this: WebSocket, ev: MessageEvent<T>) => void
	) {
		if (this._socket) {
			this._socket.addEventListener<T>(
				eventName,
				callback as EventListener
			);
		}
	}
}

export default Socket.getInstance();

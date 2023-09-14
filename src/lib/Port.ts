import { log } from './logging';
import { LogLoc } from './types';

class Port {
	private static _instance: Port;
	private _port: chrome.runtime.Port | null;
	private _name: string;

	private constructor(name: string) {
		if (Port._instance) {
			throw new Error('[NewTab] Use Port.Instance() instead of new.');
		}
		this._name = name;
		this._port = null;
		this.createPort();
		Port._instance = this;
	}

	public static getInstance(name: string) {
		return this._instance || (this._instance = new this(name));
	}

	private createPort() {
		const handlePortDisconnect = () => {
			log(LogLoc.NEWTAB, 'Port disconnected, reconnecting...');
			this.createPort();
		};

		this._port = chrome.runtime.connect({ name: this._name });
		if (!this._port.onDisconnect.hasListener(handlePortDisconnect)) {
			this._port.onDisconnect.addListener(handlePortDisconnect);
		}
	}

	public get port() {
		return this._port as chrome.runtime.Port;
	}
}

export default Port.getInstance('ws');

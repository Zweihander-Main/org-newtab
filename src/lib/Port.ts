import { log } from './logging';
import { LogLoc } from './types';

class Port {
	static #instance: Port;
	#port: chrome.runtime.Port | null;
	#name: string;

	private constructor(name: string) {
		if (Port.#instance) {
			throw new Error('[NewTab] Use Port.Instance() instead of new.');
		}
		this.#name = name;
		this.#port = null;
		this.#createPort();
		Port.#instance = this;
	}

	public static getInstance(name: string) {
		return this.#instance || (this.#instance = new this(name));
	}

	#createPort() {
		const handlePortDisconnect = () => {
			log(LogLoc.NEWTAB, 'Port disconnected, reconnecting...');
			this.#createPort();
		};

		this.#port = chrome.runtime.connect({ name: this.#name });
		if (!this.#port.onDisconnect.hasListener(handlePortDisconnect)) {
			this.#port.onDisconnect.addListener(handlePortDisconnect);
		}
	}

	public get port() {
		return this.#port as chrome.runtime.Port;
	}
}

export default Port.getInstance('ws');

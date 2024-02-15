import type { BaseStorage } from '@plasmohq/storage';
import { confirmTabIdAlive } from './messaging';
import Storage from './Storage';
import { log } from 'lib/logging';
import { LogLoc } from 'lib/types';

type connectedTabIds = Set<number>;

class Connections {
	static #instance: Connections;
	#storage: BaseStorage;
	#connectedTabIds: connectedTabIds;

	private constructor(storage: BaseStorage) {
		if (Connections.#instance) {
			throw new Error(
				'[BGSW] Use MasterWSTabId.Instance() instead of new.'
			);
		}
		Connections.#instance = this;
		this.#storage = storage;
		this.#connectedTabIds = new Set([]);
	}

	public static getInstance(storage: BaseStorage) {
		return this.#instance || (this.#instance = new this(storage));
	}

	public async add(port: chrome.runtime.Port) {
		this.#connectedTabIds.add(port?.sender?.tab?.id as number);
		await this.#saveToStorage();
	}

	public async remove(port: chrome.runtime.Port) {
		this.#connectedTabIds.delete(port?.sender?.tab?.id as number);
		await this.#saveToStorage();
	}

	public has(port: chrome.runtime.Port) {
		return this.#connectedTabIds.has(port?.sender?.tab?.id as number);
	}

	async #saveToStorage() {
		await this.#storage.set(
			'connectedTabIds',
			Array.from(this.#connectedTabIds)
		);
	}

	public async loadFromStorage() {
		const loadedConnectedTabIds =
			await this.#storage.get<Array<number>>('connectedTabIds');
		if (loadedConnectedTabIds && Array.isArray(loadedConnectedTabIds)) {
			for (const tabId of loadedConnectedTabIds) {
				const isAlive = await confirmTabIdAlive(tabId);
				if (isAlive) {
					this.#connectedTabIds.add(tabId);
					log(
						LogLoc.BGSW,
						'Confirmed alive from storage re-add for tab',
						tabId
					);
				} else {
					this.#connectedTabIds.delete(tabId);
				}
			}
			await this.#saveToStorage();
		}
	}

	public get tabIds() {
		return Array.from(this.#connectedTabIds);
	}

	public get size() {
		return this.#connectedTabIds.size;
	}
}

export default Connections.getInstance(Storage);

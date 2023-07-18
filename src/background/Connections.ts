import type { BaseStorage } from '@plasmohq/storage';
import { confirmTabIdAlive } from './messaging';
import Storage from './Storage';
import { LogLoc, log } from 'lib/logging';

type connectedTabIds = Set<number>;

class Connections {
	private static _instance: Connections;
	private _storage: BaseStorage;
	private _connectedTabIds: connectedTabIds;

	private constructor(storage: BaseStorage) {
		if (Connections._instance) {
			throw new Error(
				'[BGSW] Use MasterWSTabId.Instance() instead of new.'
			);
		}
		Connections._instance = this;
		this._storage = storage;
		this._connectedTabIds = new Set([]);
	}

	public static getInstance(storage: BaseStorage) {
		return this._instance || (this._instance = new this(storage));
	}

	public async add(port: chrome.runtime.Port) {
		this._connectedTabIds.add(port?.sender?.tab?.id as number);
		await this.saveToStorage();
	}

	public async remove(port: chrome.runtime.Port) {
		this._connectedTabIds.delete(port?.sender?.tab?.id as number);
		await this.saveToStorage();
	}

	public has(port: chrome.runtime.Port) {
		return this._connectedTabIds.has(port?.sender?.tab?.id as number);
	}

	private async saveToStorage() {
		await this._storage.set(
			'connectedTabIds',
			Array.from(this._connectedTabIds)
		);
	}

	public async loadFromStorage() {
		const loadedConnectedTabIds = await this._storage.get<Array<number>>(
			'connectedTabIds'
		);
		if (loadedConnectedTabIds && Array.isArray(loadedConnectedTabIds)) {
			for (const tabId of loadedConnectedTabIds) {
				const isAlive = await confirmTabIdAlive(tabId);
				if (isAlive) {
					this._connectedTabIds.add(tabId);
					log(
						LogLoc.BGSW,
						'Confirmed alive from storage re-add for tab',
						tabId
					);
				} else {
					this._connectedTabIds.delete(tabId);
				}
			}
			await this.saveToStorage();
		}
	}

	public get tabIds() {
		return Array.from(this._connectedTabIds);
	}

	public get size() {
		return this._connectedTabIds.size;
	}
}

export default Connections.getInstance(Storage);

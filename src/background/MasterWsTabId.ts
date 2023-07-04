import type { BaseStorage } from '@plasmohq/storage';

type masterWSTabId = number | null;

export default class MasterWSTabId {
	private static _instance: MasterWSTabId;
	private _value: masterWSTabId;
	private _storage: BaseStorage;

	private constructor(storage: BaseStorage) {
		if (MasterWSTabId._instance) {
			throw new Error(
				'[BSGW] Use MasterWSTabId.Instance() instead of new.'
			);
		}
		MasterWSTabId._instance = this;
		this._value = null;
		this._storage = storage;
	}

	public static getInstance(storage: BaseStorage) {
		return this._instance || (this._instance = new this(storage));
	}

	public get() {
		return this._value;
	}

	public async set(val: masterWSTabId) {
		this._value = val;
		await this._storage.set('masterWSTabId', val);
	}
}

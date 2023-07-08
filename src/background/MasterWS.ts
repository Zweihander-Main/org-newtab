/* eslint-disable no-console */
import type { BaseStorage } from '@plasmohq/storage';
import { sendMsgToTab, confirmTabIdAlive } from './messaging';
import { MsgToTabType } from '../util/types';
import Storage from './Storage';

type masterWS = number | null;

class MasterWS {
	private static _instance: MasterWS;
	private _value: masterWS;
	private _storage: BaseStorage;

	private constructor(storage: BaseStorage) {
		if (MasterWS._instance) {
			throw new Error(
				'[BSGW] Use MasterWSTabId.getInstance() instead of new.'
			);
		}
		MasterWS._instance = this;
		this._value = null;
		this._storage = storage;
	}

	public static getInstance(storage: BaseStorage) {
		return this._instance || (this._instance = new this(storage));
	}

	public get val() {
		return this._value;
	}

	public async set(val: masterWS) {
		this._value = val;
		await this._storage.set('masterWSTabId', val);
	}

	public async loadFromStorage() {
		const loadedMasterWSTabId = await this._storage.get<number>(
			'masterWSTabId'
		);
		if (loadedMasterWSTabId) {
			const isAlive = await confirmTabIdAlive(loadedMasterWSTabId);
			if (
				isAlive &&
				typeof loadedMasterWSTabId === 'number' &&
				!isNaN(loadedMasterWSTabId)
			) {
				await this.set(loadedMasterWSTabId);
				await sendMsgToTab(
					MsgToTabType.YOU_ARE_MASTER_WS,
					loadedMasterWSTabId
				);
				console.log(
					'[BSGW] Confirmed alive from storage re-add for master tab %d',
					loadedMasterWSTabId
				);
			} else {
				this._value = null;
			}
		}
	}
}

export default MasterWS.getInstance(Storage);

/* eslint-disable no-console */
import type { BaseStorage } from '@plasmohq/storage';
import { sendMsgToTab, confirmTabIdAlive } from './messaging';
import { MsgBGSWToNewTabType } from '../types';

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
					MsgBGSWToNewTabType.YOU_ARE_MASTER_WS,
					loadedMasterWSTabId
				);
				console.log(
					'[BSGW] Confirmed alive from storage re-add for master tab %d',
					loadedMasterWSTabId
				);
			}
		}
	}
}

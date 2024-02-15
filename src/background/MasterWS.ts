import type { BaseStorage } from '@plasmohq/storage';
import { sendMsgToTab, confirmTabIdAlive } from './messaging';
import { MsgToTabType, LogLoc } from '../lib/types';
import Storage from './Storage';
import { log } from 'lib/logging';

type masterWS = number | null;

class MasterWS {
	static #instance: MasterWS;
	#value: masterWS;
	#storage: BaseStorage;

	private constructor(storage: BaseStorage) {
		if (MasterWS.#instance) {
			throw new Error(
				'[BSGW] Use MasterWSTabId.getInstance() instead of new.'
			);
		}
		MasterWS.#instance = this;
		this.#value = null;
		this.#storage = storage;
	}

	public static getInstance(storage: BaseStorage) {
		return this.#instance || (this.#instance = new this(storage));
	}

	public get val() {
		return this.#value;
	}

	public async set(val: masterWS) {
		this.#value = val;
		await this.#storage.set('masterWSTabId', val);
	}

	public async loadFromStorage() {
		const loadedMasterWSTabId =
			await this.#storage.get<number>('masterWSTabId');
		if (loadedMasterWSTabId) {
			const isAlive = await confirmTabIdAlive(loadedMasterWSTabId);
			if (
				isAlive &&
				typeof loadedMasterWSTabId === 'number' &&
				!isNaN(loadedMasterWSTabId)
			) {
				await this.set(loadedMasterWSTabId);
				await sendMsgToTab(
					MsgToTabType.SET_ROLE_MASTER,
					loadedMasterWSTabId
				);
				log(
					LogLoc.BGSW,
					'Confirmed alive from storage re-add for master tab',
					loadedMasterWSTabId
				);
			} else {
				this.#value = null;
			}
		}
	}
}

export default MasterWS.getInstance(Storage);

import type { Persistor } from '@plasmohq/redux-persist/lib/types';

/**
 * Persistor class (persisted store) to be used as a singleton. Needed to
 * allow middleware to reference persistor while also allowing the store to
 * be created before the persistor.
 */
class PersistorClass {
	static #instance: PersistorClass;
	#persistedStore: Persistor | undefined = undefined;
	#flushing: boolean = false;

	private constructor() {
		if (PersistorClass.#instance) {
			throw new Error('Use PersistorClass.Instance() instead of new.');
		}
		PersistorClass.#instance = this;
	}

	public static getInstance() {
		return this.#instance || (this.#instance = new this());
	}

	public getStore() {
		if (this.#persistedStore) {
			return this.#persistedStore;
		} else {
			throw new Error('Store not persisted yet.');
		}
	}

	public setStore(p: Persistor) {
		this.#persistedStore = p;
	}

	public async flush() {
		this.#flushing = true;
		await this.#persistedStore?.flush();
		// Wait until storage is written, .flush doesn't promise storage write
		chrome.storage.local.get(() => {
			this.#flushing = false;
			// Persisting will rehydrate, no need to manually call/queue it
			this.#persistedStore?.persist();
		});
	}

	public async resync() {
		// Will be rehydrated when flushing is done
		if (!this.#flushing) {
			await this.#persistedStore?.resync();
		}
	}

	public get isFlushing() {
		return this.#flushing;
	}
}

export default PersistorClass.getInstance();

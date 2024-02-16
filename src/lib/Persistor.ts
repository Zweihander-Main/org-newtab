import type { Persistor } from '@plasmohq/redux-persist/lib/types';

/**
 * Persistor class (persisted store) to be used as a singleton. Needed to
 * allow middleware to reference persistor while also allowing the store to
 * be created before the persistor.
 */
class PersistorClass {
	static #instance: PersistorClass;
	#persistedStore: Persistor | undefined = undefined;

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
		return this.#persistedStore;
	}

	public setStore(p: Persistor) {
		this.#persistedStore = p;
	}

	public async flush() {
		await this.#persistedStore?.flush();
	}

	public async resync() {
		await this.#persistedStore?.resync();
	}
}

export default PersistorClass.getInstance();

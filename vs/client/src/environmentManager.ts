export class EnvironmentManager {
	/**
	 * Checks if the extension is running Merlin.
	 * 
	 * @returns True if running in Merlin and false otherwise.
	 */
	static isInMerlin(): boolean {
			const { MACHINE_EXEC_PORT } = process.env;
			return MACHINE_EXEC_PORT !== undefined;
	}
}
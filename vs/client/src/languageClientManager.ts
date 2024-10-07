import { ILEObject, ImpactedObject } from "@ibm/sourceorbit/dist/src/targets";
import * as path from 'path';
import { ExtensionContext, Uri, WorkspaceFolder } from 'vscode';
import { LanguageClient, LanguageClientOptions, ServerOptions, TransportKind } from 'vscode-languageclient/node';

export class LanguageClientManager {
	private static client: LanguageClient;

	public static async getLanguageClient(context?: ExtensionContext): Promise<LanguageClient> {
		if (!LanguageClientManager.client) {
			const serverModule = context!.asAbsolutePath(
				path.join('out', 'server.js')
			);
			const debugOptions = { execArgv: ['--nolazy', '--inspect=8720'] };

			// If the extension is launched in debug mode then the debug server options are used, otherwise the run options are used
			const serverOptions: ServerOptions = {
				run: { module: serverModule, transport: TransportKind.ipc },
				debug: {
					module: serverModule,
					transport: TransportKind.ipc,
					options: debugOptions
				}
			};

			// Options to control the language client
			const clientOptions: LanguageClientOptions = {
				documentSelector: [
					{ scheme: 'file', language: 'rpgle' },
					{ scheme: 'file', language: 'bnd' },
					{ scheme: 'file', language: 'cl' },
					{ scheme: 'file', language: 'dds.pf' },
					{ scheme: 'file', language: 'dds.lf' },
					{ scheme: 'file', language: 'dds.dspf' },
					{ scheme: 'file', language: 'dds.prtf' },
					{ scheme: 'file', language: 'sql' },
				],
			};

			// Create the language client and start the client.
			LanguageClientManager.client = new LanguageClient(
				'source-orbit-server',
				'Source Orbit Server',
				serverOptions,
				clientOptions
			);

			// Start the client which will also start the server
			try {
				await LanguageClientManager.client.start();				
			} catch (error) {
				console.log(error);
			}
		}

		return LanguageClientManager.client;
	}

	public static stop(): Promise<void> | undefined {
		if (LanguageClientManager.client) {
			return LanguageClientManager.client.stop();
		}
	}

	public static async isReady(workspaceFolder: WorkspaceFolder): Promise<boolean> {
		if (LanguageClientManager.client) {
			return await LanguageClientManager.client.sendRequest<boolean>(`isReady`, [workspaceFolder.uri.toString()]);
		} else {
			return false;
		}
	}

	public static async getResolvedObjects(workspaceFolder: WorkspaceFolder): Promise<ILEObject[]> {
		if (LanguageClientManager.client) {
			return await LanguageClientManager.client.sendRequest<ILEObject[]>(`getResolvedObjects`, [workspaceFolder.uri.toString()]);
		} else {
			return [];
		}
	}

	public static async getDeps(workspaceFolder: WorkspaceFolder, ileObject: ILEObject): Promise<ILEObject[]> {
		if (LanguageClientManager.client) {
			return await LanguageClientManager.client.sendRequest<ILEObject[]>(`getDeps`, [workspaceFolder.uri.toString(), ileObject]);
		} else {
			return [];
		}
	}

	public static async getImpacts(workspaceFolder: WorkspaceFolder, fileUris: Uri[]): Promise<ImpactedObject[]> {
		if (LanguageClientManager.client) {
			return await LanguageClientManager.client.sendRequest<ImpactedObject[]>(`getImpacts`, [workspaceFolder.uri.toString(), fileUris.map(uri => uri.toString())]);
		} else {
			return [];
		}
	}

	public static reloadProject(workspaceFolder: WorkspaceFolder) {
		if (LanguageClientManager.client) {
			return LanguageClientManager.client.sendRequest(`reloadProject`, [workspaceFolder.uri.toString()]);
		} else {
			return Promise.reject();
		}
	}

	public static fixProject(workspaceFolder: WorkspaceFolder, suggestion: string) {
		if (LanguageClientManager.client) {
			return LanguageClientManager.client.sendRequest(`fixProject`, [workspaceFolder.uri.toString(), suggestion]);
		} else {
			return;
		}
	}

	public static generateBuildFile(workspaceFolder: WorkspaceFolder, type: string) {
		if (LanguageClientManager.client) {
			return LanguageClientManager.client.sendRequest(`generateBuildFile`, [workspaceFolder.uri.toString(), type]);
		} else {
			return;
		}
	}
}
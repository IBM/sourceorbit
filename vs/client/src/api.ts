import { commands, ExtensionContext, Uri, WorkspaceFolder } from 'vscode';
import { LanguageClientManager } from './languageClientManager';
import { ILEObject } from '@ibm/sourceorbit/dist/src/targets';

/**
 * These commands are to be used by other extensions.
 */
export namespace SourceOrbitApi {
	export function registerCommands(context: ExtensionContext) {
		context.subscriptions.push(
			commands.registerCommand(`vscode-sourceorbit.isReady`, async (workspaceFolder: WorkspaceFolder) => {
				if (workspaceFolder) {
					return await LanguageClientManager.isReady(workspaceFolder);
				}
				return false;
			}),

			commands.registerCommand(`vscode-sourceorbit.reloadProject`, async (workspaceFolder: WorkspaceFolder) => {
				if (workspaceFolder) {
					return await LanguageClientManager.reloadProject(workspaceFolder);
				}
			}),

			commands.registerCommand(`vscode-sourceorbit.resolvePathToObject`, async (workspaceFolder: WorkspaceFolder, localPath: string) => {
				if (workspaceFolder && localPath) {
					return await LanguageClientManager.resolvePathToObject(workspaceFolder, localPath);
				}
				return undefined;
			}),

			commands.registerCommand(`vscode-sourceorbit.getResolvedObjects`, async (workspaceFolder: WorkspaceFolder) => {
				if (workspaceFolder) {
					return await LanguageClientManager.getResolvedObjects(workspaceFolder);
				}
				return [];
			}),

			commands.registerCommand(`vscode-sourceorbit.getDeps`, async (workspaceFolder: WorkspaceFolder, ileObject: ILEObject) => {
				if (workspaceFolder && ileObject) {
					return await LanguageClientManager.getDeps(workspaceFolder, ileObject);
				}
				return [];
			}),

			commands.registerCommand(`vscode-sourceorbit.getImpactsToUris`, async (workspaceFolder: WorkspaceFolder, fileUris: Uri[]) => {
				if (workspaceFolder && fileUris) {
					return await LanguageClientManager.getImpactsToUris(workspaceFolder, fileUris);
				}
				return [];
			}),
	
			commands.registerCommand(`vscode-sourceorbit.getImpactsToObjects`, async (workspaceFolder: WorkspaceFolder, ileObjects: ILEObject[]) => {
				if (workspaceFolder && ileObjects) {
					return await LanguageClientManager.getImpactsToObjects(workspaceFolder, ileObjects);
				}
				return [];
			}),

			commands.registerCommand(`vscode-sourceorbit.getExports`, async (workspaceFolder: WorkspaceFolder) => {
				if (workspaceFolder) {
					return await LanguageClientManager.getExports(workspaceFolder);
				}
				return {};
			}),

			commands.registerCommand(`vscode-sourceorbit.autoFix`, (workspaceFolder: WorkspaceFolder, type: "includes" | "renames") => {
				if (workspaceFolder && type) {
					return LanguageClientManager.fixProject(workspaceFolder, type);
				}
			}),

			commands.registerCommand(`vscode-sourceorbit.generateBuildFile`, async (workspaceFolder: WorkspaceFolder, type: string) => {
				if (workspaceFolder && type) {
					await LanguageClientManager.generateBuildFile(workspaceFolder, type);
				}
			})
		);
	}
}
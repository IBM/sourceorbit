import { commands, ExtensionContext, WorkspaceFolder } from 'vscode';
import { LanguageClientManager } from './languageClientManager';

/**
 * These commands are to be used by other extensions.
 */
export namespace SourceOrbitApi {
	export function registerCommands(context: ExtensionContext) {
		context.subscriptions.push(
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
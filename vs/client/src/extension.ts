import { IProject } from '@ibm/vscode-ibmi-projectexplorer-types/iproject';
import { commands, ExtensionContext, window, workspace, WorkspaceFolder } from 'vscode';
import { Api } from './api';
import { EnvironmentManager } from './environmentManager';
import { GitEventHandler } from './gitEventHandler';
import { LanguageClientManager } from './languageClientManager';
import { getProjectManager, loadIBMiProjectExplorer } from './ProjectExplorer';
import { initialiseTaskProvider } from './tasks';
import { ImpactView } from './views/impactView/impactView';
import { ILEObjectTreeItem } from './views/projectExplorer/ileObjectTreeItem';
import { SourceOrbitTreeItem } from './views/projectExplorer/sourceOrbitTreeItem';

export async function activate(context: ExtensionContext) {
	console.log('Congratulations, your extension "vscode-sourceorbit" is now active!');

	// Start the client
	await LanguageClientManager.getLanguageClient(context);

	initialiseTaskProvider(context);

	Api.registerCommands(context);

	if (!EnvironmentManager.isInMerlin()) {
		// Ensure that the PE items only load if that extension is installed
		const peLoaded = await loadIBMiProjectExplorer();

		if (peLoaded) {
			const projectManager = getProjectManager();
			const objectViews: { [workspaceUri: string]: SourceOrbitTreeItem } = {};

			if (projectManager) {
				commands.executeCommand(`setContext`, `vscode-sourceorbit:projectExplorerLoaded`, true);

				projectManager.pushExtensibleChildren(async (iProject: IProject) => {
					const fsPath = iProject.workspaceFolder.uri.fsPath;

					objectViews[fsPath] = new SourceOrbitTreeItem(iProject.workspaceFolder);
					return [objectViews[fsPath]];
				});

				context.subscriptions.push(
					// Project Explorer specific command
					commands.registerCommand(`vscode-sourceorbit.objects.loadProject`, async (node: SourceOrbitTreeItem) => {
						if (node) {
							await LanguageClientManager.reloadProject(node.workspaceFolder);
							node.refresh();
						}
					}),

					// Project Explorer specific command
					commands.registerCommand(`vscode-sourceorbit.objects.autoFix`, ((node: SourceOrbitTreeItem) => {
						if (node && node.workspaceFolder) {
							window.showInformationMessage(`Select auto fix method for ${node.workspaceFolder.name}`, `Cancel`, `File names`, `RPG includes`).then(chosen => {
								if (chosen) {
									let type: "includes" | "renames" | undefined;

									switch (chosen) {
										case `File names`: type = `renames`; break;
										case `RPG includes`: type = `includes`; break;
									}

									if (type) {
										LanguageClientManager.fixProject(node.workspaceFolder, type);
									}
								}
							});
						}
					})),
				);
			}
		}

		// Register Git and active editor based impact views
		const gitImpactView: ImpactView = new ImpactView();
		const activeImpactView: ImpactView = new ImpactView();
		context.subscriptions.push(
			window.registerTreeDataProvider(`activeImpactView`, activeImpactView),
			window.registerTreeDataProvider(`gitImpactView`, gitImpactView),
			commands.registerCommand(`vscode-sourceorbit.objects.goToFile`, ((node: ILEObjectTreeItem) => {
				if (node && node.resourceUri) {
					workspace.openTextDocument(node.resourceUri).then(doc => {
						window.showTextDocument(doc);
					});
				}
			})),
			window.onDidChangeActiveTextEditor(e => {
				if (activeImpactView && e && e.document) {
					activeImpactView.showImpactFor([e.document.uri]);
				} else {
					activeImpactView.showImpactFor([]);
				}
			}),
			workspace.onDidChangeWorkspaceFolders((e) => {
				GitEventHandler.setup(context, gitImpactView, e.added as WorkspaceFolder[]);
			})
		);

		// Git setup for current workspace folders
		if (workspace.workspaceFolders) {
			GitEventHandler.setup(context, gitImpactView, workspace.workspaceFolders as WorkspaceFolder[]);
		}

		// Impact for current active editor
		const activeTextEditor = window.activeTextEditor;
		if(activeTextEditor) {
			activeImpactView.showImpactFor([activeTextEditor.document.uri]);
		}
	}
}

export function deactivate(): Thenable<void> | undefined {
	return LanguageClientManager.stop();
}
import { IProject } from '@ibm/vscode-ibmi-projectexplorer-types/iproject';
import { commands, ExtensionContext, window, workspace, WorkspaceFolder } from 'vscode';
import { SourceOrbitApi } from './api';
import { EnvironmentManager } from './environmentManager';
import { GitEventHandler } from './gitEventHandler';
import { LanguageClientManager } from './languageClientManager';
import { getProjectManager, loadIBMiProjectExplorer } from './ProjectExplorer';
import { SourceOrbitTask } from './tasks';
import { ImpactView } from './views/impactView';
import { ILEObjectTreeItem } from './views/projectExplorer/ileObjectTreeItem';
import { SourceOrbitTreeItem } from './views/projectExplorer/sourceOrbitTreeItem';

export async function activate(context: ExtensionContext) {
	console.log('Congratulations, your extension "vscode-sourceorbit" is now active!');

	// Start the client
	await LanguageClientManager.getLanguageClient(context);

	SourceOrbitTask.initializeTaskProvider(context);

	SourceOrbitApi.registerCommands(context);

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

				// Project Explorer specific commands
				context.subscriptions.push(
					commands.registerCommand(`vscode-sourceorbit.objects.loadProject`, async (node: SourceOrbitTreeItem) => {
						if (node) {
							await LanguageClientManager.reloadProject(node.workspaceFolder);
							node.refresh();
						}
					}),
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
					commands.registerCommand(`vscode-sourceorbit.objects.generateBobBuildFile`, async (node: SourceOrbitTreeItem) => {
						if (node && node.workspaceFolder) {
							await LanguageClientManager.generateBuildFile(node.workspaceFolder, 'bob');
						}
					}),
					commands.registerCommand(`vscode-sourceorbit.objects.generateMakeBuildFile`, async (node: SourceOrbitTreeItem) => {
						if (node && node.workspaceFolder) {
							await LanguageClientManager.generateBuildFile(node.workspaceFolder, 'make');
						}
					}),
					commands.registerCommand(`vscode-sourceorbit.objects.generateImdBuildFile`, async (node: SourceOrbitTreeItem) => {
						if (node && node.workspaceFolder) {
							await LanguageClientManager.generateBuildFile(node.workspaceFolder, 'imd');
						}
					}),
					commands.registerCommand(`vscode-sourceorbit.objects.generateJsonBuildFile`, async (node: SourceOrbitTreeItem) => {
						if (node && node.workspaceFolder) {
							await LanguageClientManager.generateBuildFile(node.workspaceFolder, 'json');
						}
					})
				);
			}
		}

		// Register Git and active editor based impact views
		const gitImpactView: ImpactView = new ImpactView();
		const activeImpactView: ImpactView = new ImpactView();
		context.subscriptions.push(
			window.createTreeView(`gitImpactView`, { treeDataProvider: gitImpactView, showCollapseAll: true }),
			window.createTreeView(`activeImpactView`, { treeDataProvider: activeImpactView, showCollapseAll: true }),
			commands.registerCommand(`vscode-sourceorbit.objects.refreshGitImpactView`, (async () => {
				if (gitImpactView.impactOf && gitImpactView.impactOf.length > 0) {
					const workspaceFolder = workspace.getWorkspaceFolder(gitImpactView.impactOf[0]);
					if (workspaceFolder) {
						await LanguageClientManager.reloadProject(workspaceFolder);
					}
				}
				gitImpactView.refresh();
			})),
			commands.registerCommand(`vscode-sourceorbit.objects.refreshActiveImpactView`, (async () => {
				if (activeImpactView.impactOf && activeImpactView.impactOf.length > 0) {
					const workspaceFolder = workspace.getWorkspaceFolder(activeImpactView.impactOf[0]);
					if (workspaceFolder) {
						await LanguageClientManager.reloadProject(workspaceFolder);
					}
				}
				activeImpactView.refresh();
			})),
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
		if (activeTextEditor) {
			activeImpactView.showImpactFor([activeTextEditor.document.uri]);
		}
	}
}

export function deactivate(): Thenable<void> | undefined {
	return LanguageClientManager.stop();
}
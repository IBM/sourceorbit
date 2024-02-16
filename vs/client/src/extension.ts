
import * as path from 'path';
import { workspace, ExtensionContext, window, commands, Uri, WorkspaceFolder } from 'vscode';

import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	TransportKind
} from 'vscode-languageclient/node';
import { fixProject, generateBuildFile, reloadProject, setClient } from './requests';
import { getProjectExplorer, getProjectManager, loadIBMiProjectExplorer } from './ProjectExplorer';
import { ILEObjectTreeItem, ObjectsView } from './views/objectView';
import { IProject } from '@ibm/vscode-ibmi-projectexplorer-types/iproject';
import { ImpactView } from './views/impactView';
import { getDeployGitFiles as getChanged, getDeployGitFiles as getChangedFiles, getGitAPI, lastBranch } from './git';
import { initialiseTaskProvider } from './tasks';
import { EnvironmentManager } from './environmentManager';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
	// The server is implemented in node
	const serverModule = context.asAbsolutePath(
		path.join('out', 'server.js')
	);

	const debugOptions = { execArgv: ['--nolazy', '--inspect=8720'] };

	// If the extension is launched in debug mode then the debug server options are used
	// Otherwise the run options are used
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
	client = new LanguageClient(
		'sourceorder-server',
		'Source Orbit Server',
		serverOptions,
		clientOptions
	);

	// Start the client. This will also launch the server
	client.start();

	setClient(client);

	initialiseTaskProvider(context);

	registerApiCommands(context);

	if (!EnvironmentManager.isInMerlin()) {
		// Hide the views if we are in Merlin. Merlin has its own stuff.
		registerViews(context);
	}
}

export function enableViews() {
	commands.executeCommand(`setContext`, `vscode-sourceorbit:projectsLoaded`, true);
}

function registerApiCommands(context: ExtensionContext) {
	context.subscriptions.push(
		commands.registerCommand(`vscode-sourceorbit.autoFix`, (workspaceFolder?: WorkspaceFolder, type?: "includes" | "renames") => {
			if (workspaceFolder && type) {
				return fixProject(workspaceFolder, type);
			}
		}),

		commands.registerCommand(`vscode-sourceorbit.generateBuildFile`, async (workspaceFolder?: WorkspaceFolder, type?: string) => {
			if (workspaceFolder && type) {
				await generateBuildFile(workspaceFolder, type);
				enableViews();
			}
		}),
	);
}

async function registerViews(context: ExtensionContext) {
	// Ensure that the PE items only load if that extension is installed
	const peLoaded = await loadIBMiProjectExplorer();

	if (peLoaded) {
		const projectManager = getProjectManager();
		const objectViews: { [workspaceUri: string]: ObjectsView } = {};

		if (projectManager) {
			commands.executeCommand(`setContext`, `vscode-sourceorbit:projectExplorerLoaded`, true);

			projectManager.pushExtensibleChildren(async (iProject: IProject) => {
				const fsPath = iProject.workspaceFolder.uri.fsPath;

				objectViews[fsPath] = new ObjectsView(iProject.workspaceFolder);
				return [objectViews[fsPath]];
			});

			context.subscriptions.push(
				// Project Explorer specific command
				commands.registerCommand(`vscode-sourceorbit.objects.loadProject`, async (node: ObjectsView) => {
					if (node) {
						await reloadProject(node.workspaceFolder);
						enableViews();
						node.refresh();
					}
				}),

				// Project Explorer specific command
				commands.registerCommand(`vscode-sourceorbit.objects.autoFix`, ((node: ObjectsView) => {
					if (node && node.workspaceFolder) {
						window.showInformationMessage(`Select auto fix method for ${node.workspaceFolder.name}`, `Cancel`, `File names`, `RPG includes`).then(chosen => {
							if (chosen) {
								let type: "includes" | "renames" | undefined;

								switch (chosen) {
									case `File names`: type = `renames`; break;
									case `RPG includes`: type = `includes`; break;
								}

								if (type) {
									fixProject(node.workspaceFolder, type);
								}
							}
						});
					}
				})),
			);
		}
	}

	// Register all the remaining views
	const gitImpactView: ImpactView = new ImpactView();
	const activeImpactView: ImpactView = new ImpactView();

	context.subscriptions.push(
		commands.registerCommand(`vscode-sourceorbit.objects.goToFile`, ((node: ILEObjectTreeItem) => {
			if (node && node.resourceUri) {
				workspace.openTextDocument(node.resourceUri).then(doc => {
					window.showTextDocument(doc);
				});
			}
		})),

		window.registerTreeDataProvider(`activeImpactView`, activeImpactView),
		window.onDidChangeActiveTextEditor(e => {
			if (activeImpactView && e && e.document) {
				// For impact view. Doesn't do anything if the mode isn't set
				activeImpactView.showImpactFor([e.document.uri]);
			}
		}),

		window.registerTreeDataProvider(`gitImpactView`, gitImpactView),
		workspace.onDidChangeWorkspaceFolders((e) => {
			setupGitEventHandler(e.added as WorkspaceFolder[]);
		})
	);

	function setupGitEventHandler(workspaceFolders: WorkspaceFolder[]) {
		const gitApi = getGitAPI();

		if (gitApi) {
			for (const workspaceFolder of workspaceFolders) {
				const repo = gitApi.getRepository(workspaceFolder.uri);
				if (repo) {
					const workspaceUri = workspaceFolder.uri.toString();
					const head = repo.state.HEAD;
					if (head && head.name) {
						lastBranch[workspaceUri] = head.name;

						context.subscriptions.push(repo.state.onDidChange((_e) => {
							const currentBranch = head.name;
							if (currentBranch) {
								if (lastBranch[workspaceUri] && currentBranch !== lastBranch[workspaceUri]) {
									gitImpactView.showImpactFor([]);
									reloadProject(workspaceFolder);

								} else {
									getChangedFiles(workspaceFolder).then(files => {
										gitImpactView.showImpactFor(files);
									});

								}

								lastBranch[workspaceUri] = currentBranch;
							}
						}));
					}
				}
			}
		}
	}

	if (workspace.workspaceFolders) setupGitEventHandler(workspace.workspaceFolders as WorkspaceFolder[]);
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}

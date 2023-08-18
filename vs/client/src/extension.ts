
import * as path from 'path';
import { workspace, ExtensionContext, window, commands, Uri, WorkspaceFolder } from 'vscode';

import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	TransportKind
} from 'vscode-languageclient/node';
import { fixProject, reloadProject, resolveProject, setClient } from './requests';
import { getProjectExplorer, getProjectManager, loadIBMiProjectExplorer } from './ProjectExplorer';
import { ILEObjectTreeItem, ObjectsView } from './views/objectView';
import { IProject } from '@ibm/vscode-ibmi-projectexplorer-types/iproject';
import { ProjectExplorerTreeItem } from '@ibm/vscode-ibmi-projectexplorer-types/views/projectExplorer/projectExplorerTreeItem';
import { TargetSuggestions } from '@halcyontech/source-orbit/dist/src/targets';
import { ImpactView } from './views/impactView';
import { getDeployGitFiles as getChanged, getDeployGitFiles as getChangedFiles, getGitAPI, lastBranch } from './git';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
	// The server is implemented in node
	const serverModule = context.asAbsolutePath(
		path.join('server', 'out', 'server.js')
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

	loadIBMiProjectExplorer();

	const projectManager = getProjectManager();
	const projectExplorer = getProjectExplorer();

	const activeImpactView: ImpactView = new ImpactView();
	const gitImpactView: ImpactView = new ImpactView();
	const objectViews: { [workspaceUri: string]: ObjectsView } = {};

	if (projectManager) {
		projectManager.pushExtensibleChildren(async (iProject: IProject) => {
			const fsPath = iProject.workspaceFolder.uri.fsPath;

			objectViews[fsPath] = new ObjectsView(iProject.workspaceFolder);
			return [objectViews[fsPath]];
		});
	}

	function setupGitEventHandler(workspaceFolders: WorkspaceFolder[]) {
		const gitApi = getGitAPI();

		for (const workspaceFolder of workspaceFolders) {
			const repo = gitApi.getRepository(workspaceFolder.uri);
			if (repo) {
				const workspaceUri = workspaceFolder.uri.toString();
				if (repo.state.HEAD)
					lastBranch[workspaceUri] = repo.state.HEAD.name;

				context.subscriptions.push(repo.state.onDidChange((e) => {
					const currentBranch = repo.state.HEAD.name;
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

	context.subscriptions.push(
		client.onRequest(`reloadUi`, (params: [string[]]) => {
			const folderPaths = params[0];

			for (const folderPath of folderPaths) {
				if (objectViews[folderPath]) {
					projectExplorer.refresh(objectViews[folderPath]);
				}
			}
		}),
		commands.registerCommand(`vscode-sourceorbit.objects.goToFile`, ((node: ILEObjectTreeItem) => {
			if (node && node.resourceUri) {
				workspace.openTextDocument(node.resourceUri).then(doc => {
					window.showTextDocument(doc);
				});
			}
		})),
		commands.registerCommand(`vscode-sourceorbit.objects.resolve`, ((node: ObjectsView) => {
			if (node && node.workspaceFolder) {
				resolveProject(node.workspaceFolder);
			}
		})),
		commands.registerCommand(`vscode-sourceorbit.objects.autoFix`, ((node: ObjectsView) => {
			if (node && node.workspaceFolder) {
				window.showInformationMessage(`Select auto fix method for ${node.workspaceFolder.name}`, `Cancel`, `File names`, `RPG includes`).then(chosen => {
					if (chosen) {
						let type: "includes" | "renames";

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

	if (workspace.workspaceFolders) setupGitEventHandler(workspace.workspaceFolders as WorkspaceFolder[]);
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}

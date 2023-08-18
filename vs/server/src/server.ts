import {
	createConnection,
	Diagnostic,
	DiagnosticSeverity,
	ProposedFeatures,
	InitializeParams,
	TextDocumentSyncKind,
	InitializeResult,
	WorkspaceFolder,
	Range,
	Position
} from 'vscode-languageserver/node';
import { SupportedGlob, TargetsManager } from './TargetsManager';
import { setupRequestHandler } from './requests';
import { reloadLog, clearLogs, reResolve, reloadUi } from './ui';
import { initAndRefresh } from './setup';

// Create a connection for the server, using Node's IPC as a transport.
// Also include all preview / proposed LSP features.
export const connection = createConnection(ProposedFeatures.all);

let hasWorkspaceFolderCapability = false;

connection.onInitialize((params: InitializeParams) => {
	const capabilities = params.capabilities;

	// Does the client support the `workspace/configuration` request?
	// If not, we fall back using global settings.
	hasWorkspaceFolderCapability = !!(
		capabilities.workspace && !!capabilities.workspace.workspaceFolders
	);

	const result: InitializeResult = {
		capabilities: {
			textDocumentSync: TextDocumentSyncKind.Incremental,
		}
	};
	if (hasWorkspaceFolderCapability) {
		result.capabilities.workspace = {
			fileOperations: {
				didCreate: { filters: [{ pattern: { glob: SupportedGlob, options: { ignoreCase: true } } }] },
				didRename: { filters: [{ pattern: { glob: SupportedGlob, options: { ignoreCase: true } } }] },
				didDelete: { filters: [{ pattern: { glob: SupportedGlob, options: { ignoreCase: true } } }] }
			},
			workspaceFolders: {
				supported: true,
				changeNotifications: true
			}
		};
	}
	return result;
});

connection.onInitialized(() => {
	// TODO: workspace management
	if (hasWorkspaceFolderCapability) {
		connection.workspace.getWorkspaceFolders().then(workspaceFolders => {
			connection.console.log(`Connected and got workspace folders`);
			if (workspaceFolders) {
				for (const workspaceFolder of workspaceFolders) {
					connection.console.log(JSON.stringify(workspaceFolder, null, 2));
					initAndRefresh(workspaceFolder.uri);
				}
			}
		});

		connection.workspace.onDidChangeWorkspaceFolders(_event => {
			connection.console.log('Workspace folder change event received.');
			connection.console.log(JSON.stringify(_event, null, 2));

			for (const removed of _event.removed) {
				TargetsManager.destory(removed.uri);
			}

			for (const added of _event.added) {
				initAndRefresh(added.uri);
			}
		});

		connection.onDidSaveTextDocument((params) => {
			const uri = params.textDocument.uri;
			connection.console.log(JSON.stringify(uri, null, 2));

			TargetsManager.refreshSingle(uri)?.then(() => {
				reloadLog(uri);
				reloadUi([uri]);
			});
		});

		connection.workspace.onDidDeleteFiles((params) => {
			const files = params.files;
			connection.console.log(JSON.stringify(files, null, 2));

			for (const file in files) {
				clearLogs(file);
			}

			Promise.all(files.map(deleted => TargetsManager.removeSingle(deleted.uri))).then(r => {
				const uris = files.map(f => f.uri);
				reResolve(uris);
				reloadUi(uris);
			});

		});

		connection.workspace.onDidCreateFiles((params) => {
			const files = params.files;
			connection.console.log(JSON.stringify(files, null, 2));

			for (const created of files) {
				TargetsManager.refreshSingle(created.uri);
			}
		});

		connection.workspace.onDidRenameFiles(async (params) => {
			const files = params.files;
			connection.console.log(JSON.stringify(files, null, 2));

			const workspaceUris: string[] = [];

			for (const rename of files) {
				clearLogs(rename.oldUri);
				await TargetsManager.removeSingle(rename.oldUri);
				TargetsManager.refreshSingle(rename.newUri);


			}

			const uris = files.map(f => f.newUri);
			reResolve(uris);
			reloadUi(uris);
		});
	}
});

// Listen on the connection
connection.listen();

setupRequestHandler(connection);
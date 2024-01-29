import { Connection } from 'vscode-languageserver';
import { initAndRefresh } from './setup';
import { TargetsManager } from './TargetsManager';

export function setupFsListener(connection: Connection) {

	// Commented out since we don't read entire projects on load
	// connection.workspace.getWorkspaceFolders().then(workspaceFolders => {
	// 	connection.console.log(`Connected and got workspace folders`);
	// 	if (workspaceFolders) {
	// 		for (const workspaceFolder of workspaceFolders) {
	// 			connection.console.log(JSON.stringify(workspaceFolder, null, 2));
	// 			initAndRefresh(workspaceFolder.uri);
	// 		}
	// 	}
	// });

	connection.workspace.onDidChangeWorkspaceFolders(_event => {
		connection.console.log('Workspace folder change event received.');
		connection.console.log(JSON.stringify(_event, null, 2));

		for (const removed of _event.removed) {
			TargetsManager.destory(removed.uri);
		}

		// Commented out since we don't read entire projects on load
		// for (const added of _event.added) {
		// 	initAndRefresh(added.uri);
		// }
	});

	connection.onDidSaveTextDocument((params) => {
		const uri = params.textDocument.uri;
		connection.console.log(JSON.stringify(uri, null, 2));
		
		TargetsManager.refreshSingle(uri);
	});

	connection.workspace.onDidDeleteFiles((params) => {
		const files = params.files;
		connection.console.log(JSON.stringify(files, null, 2));

		files.map(deleted => TargetsManager.removeSingle(deleted.uri));

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
			await TargetsManager.removeSingle(rename.oldUri);
			TargetsManager.refreshSingle(rename.newUri);
		}
	});
}
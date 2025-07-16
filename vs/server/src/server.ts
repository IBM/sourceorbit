import { Targets } from '@ibm/sourceorbit';
import {
	createConnection,
	InitializeParams,
	InitializeResult,
	ProposedFeatures,
	TextDocumentSyncKind
} from 'vscode-languageserver/node';
import { setupFsListener } from './fileSystemListener';
import { setupRequestHandler } from './requests';

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
				didCreate: { filters: [{ pattern: { glob: Targets.LanguageProvider.getGlob(), options: { ignoreCase: true } } }] },
				didRename: { filters: [{ pattern: { glob: Targets.LanguageProvider.getGlob(), options: { ignoreCase: true } } }] },
				didDelete: { filters: [{ pattern: { glob: Targets.LanguageProvider.getGlob(), options: { ignoreCase: true } } }] }
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
	if (hasWorkspaceFolderCapability) {
		setupFsListener(connection);
	}
});

// Listen on the connection
connection.listen();

setupRequestHandler(connection);
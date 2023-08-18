import { Targets } from '@halcyontech/source-orbit';
import { TargetsManager } from './TargetsManager';
import { FileLog } from '@halcyontech/source-orbit/dist/src/logger';
import path from 'path';
import { Diagnostic, Range, DiagnosticSeverity } from 'vscode-languageserver';
import { URI } from 'vscode-uri';
import { connection } from './server';

export function reResolve(fileUris: string[]) {
	const fullPaths = fileUris.map(fu => URI.parse(fu).fsPath);
	const workspaceFolders = fullPaths.map(fullPath => TargetsManager.getWorkspaceFolder(fullPath)).map(x => x) as string[];
	const unique = workspaceFolders.filter((value, index, array) => array.indexOf(value) === index);

	for (const workspaceFolder of unique) {
		const target = TargetsManager.getTargetsForWorkspacePath(workspaceFolder);
		if (target) {
			connection.console.log(`Re-resolving ${workspaceFolder}`);
			target.resolveBinder();

			reloadLogs(target);
		}

		connection.sendRequest(`reloadUi`, unique);
	}
}

export function reloadUi(fileUris: string[]) {
	const fullPaths = fileUris.map(fu => URI.parse(fu).fsPath);
	const workspaceFolders = fullPaths.map(fullPath => TargetsManager.getWorkspaceFolder(fullPath)).map(x => x) as string[];
	const unique = workspaceFolders.filter((value, index, array) => array.indexOf(value) === index);

	connection.sendRequest(`reloadUi`, [unique]);
}

export function reloadLog(fileUri: string) {
	const target = TargetsManager.getTargetsForFile(fileUri);

	if (target) {
		const fullPath = target.getRelative(URI.parse(fileUri).fsPath);
		const specificLogs = target.logger.getLogsFor(fullPath);

		if (specificLogs) {
			const logs = specificLogs.filter(log => [`warning`, `info`].includes(log.type));

			sendLogs(fileUri, logs);
		} else {
			sendLogs(fileUri, []);
		}
	}
}

export function reloadLogs(target: Targets) {
	const cwd = target.getCwd();
	const allLogs = target.logger.getAllLogs();

	for (const relativePath in allLogs) {
		const logs = allLogs[relativePath].filter(log => [`warning`, `info`].includes(log.type));

		sendLogs(URI.file(path.join(cwd, relativePath)).toString(), logs);
	}
}

function sendLogs(fileUri: string, logs: FileLog[]) {
	const diags: Diagnostic[] = [];

	for (const log of logs) {
		let range: Range;

		if (log.range) {
			// No idea how to support start/end range yet
			range = Range.create(0, 0, 0, 0);

		} else

			if (log.line && log.line >= 0) {
				range = Range.create(log.line, 0, log.line, 200);
			} else {
				range = Range.create(0, 0, 0, 0);
			}

		if (range) {
			diags.push(Diagnostic.create(
				range,
				log.message,
				log.type === `warning` ? DiagnosticSeverity.Warning : DiagnosticSeverity.Information,
				undefined,
				`Source Orbit`
			));
		}
	}

	connection.sendDiagnostics({ uri: fileUri, diagnostics: diags });
}

export function clearLogs(fileUri: string) {
	connection.sendDiagnostics({ uri: fileUri, diagnostics: [] });
}
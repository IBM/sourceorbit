import { URI } from 'vscode-uri';
import { SupportedGlob, TargetsManager, getFiles } from './TargetsManager';
import { Targets } from '@ibm/sourceorbit';
import { TargetSuggestions } from '@ibm/sourceorbit/dist/src/targets';
import { Logger } from '@ibm/sourceorbit/dist/src/logger';
import path from 'path';
import fs from 'fs';
import { connection } from './server';
import { reloadLogs, reloadUi } from './ui';

export async function initAndRefresh(workspaceUri: string, destoryOld = false) {
	const progress = await connection.window.createWorkDoneProgress();
	progress.begin(`Source Orbit`, undefined, `Initialising..`);

	TargetsManager.initialise(workspaceUri, destoryOld)
		.then(() => {
			progress.report(`Initialised`);
			const target = TargetsManager.getTargetsForWorkspaceUri(workspaceUri);
			if (target) {
				connection.console.log(`Reloading logs`);
				reloadLogs(target);
				reloadUi([workspaceUri]);
			}
		})
		.catch(_e => {
			progress.report(`Failed to initialise`);
		})
		.finally(() => {
			setTimeout(() => {
				progress.done();
			}, 1500);
		});
}

export async function fixProject(workspaceUri: string, suggestions: TargetSuggestions) {
	const uri = URI.parse(workspaceUri);
	const url = uri.fsPath;

	const progress = await connection.window.createWorkDoneProgress();

	progress.begin(`Source Orbit`, undefined, `Fetching files..`);
	const files = getFiles(url, SupportedGlob);

	const targets = new Targets(url);
	targets.setSuggestions(suggestions);

	progress.report(`Loading files..`);
	targets.loadObjectsFromPaths(files);

	progress.report(`Processing files..`);
	await Promise.allSettled(files.map(f => targets.parseFile(f)));

	progress.report(`Resolving the project`);
	targets.resolveBinder();

	try {
		if (suggestions.includes) {
			progress.report(`Fixing includes`);
			replaceIncludes(targets.logger);
		}

		if (suggestions.renames) {
			progress.report(`Fixing file names`);
			renameFiles(targets.logger);
		}

	} catch (e) {
		console.log(e);

	} finally {
		progress.done();
		// Let's reinitialised the project when we're done.
		initAndRefresh(workspaceUri, true);
	}
	
}

function replaceIncludes(logger: Logger) {
	console.log(`Starting include fix process. Do not end process.`);
	const allLogs = logger.getAllLogs();

	for (const filePath in allLogs) {
		const content = fs.readFileSync(filePath, { encoding: `utf8` });
		const eol = content.includes(`\r\n`) ? `\r\n` : `\n`;
		const lines = content.split(eol);

		const logs = allLogs[filePath].filter(l => l.type === `includeFix` && l.line);
		for (const log of logs) {
			if (log.change && log.line && log.change.lineContent) {
				lines[log.line] = log.change.lineContent;
			}
		}

		console.log(`${filePath} changes: ${logs.length}`);
		fs.writeFileSync(filePath, lines.join(eol));
	}
}

function renameFiles(logger: Logger) {
	console.log(`Starting rename process. Do not end process.`);
	const allLogs = logger.getAllLogs();

	const validRenames: {[path: string]: string} = {};

	for (const filePath in allLogs) {
		const logs = allLogs[filePath].filter(l => l.type === `rename`);

		for (const log of logs) {
			if (log.change && log.change.rename) {
				const detail = log.change.rename;
				validRenames[detail.path] = detail.newName;
			}
		}
	}


	for (const ogPath in validRenames) {
		fs.renameSync(ogPath, path.join(path.dirname(ogPath), validRenames[ogPath]));
		console.log(`'${ogPath}' -> '${validRenames[ogPath]}'`);
	}
}
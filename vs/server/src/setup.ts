import { BobProject, ImpactMarkdown, MakeProject, Targets } from '@ibm/sourceorbit';
import { Logger } from '@ibm/sourceorbit/dist/src/logger';
import { TargetSuggestions } from '@ibm/sourceorbit/dist/src/targets';
import fs from 'fs';
import path from 'path';
import { URI } from 'vscode-uri';
import { ReadFileSystem } from './readFileSystem';
import { connection } from './server';
import { SupportedGlob, TargetsManager } from './TargetsManager';

export async function initAndRefresh(workspaceUri: string) {
	const progress = await connection.window.createWorkDoneProgress();
	progress.begin(`Source Orbit`, undefined, `Initializing..`);

	try {
		await TargetsManager.refreshProject(workspaceUri);
		progress.report(`Initialized`);

	} catch (e) {
		progress.report(`Failed to initialize`);
	}

	setTimeout(() => {
		progress.done();
	}, 1500);

	return;
}

export async function generateBuildFile(workspaceUri: string, type: string) {
	const progress = await connection.window.createWorkDoneProgress();

	await TargetsManager.refreshProject(workspaceUri);

	progress.begin(`Source Orbit`, undefined, `Reloading project..`);
	const targets = TargetsManager.getTargetsForWorkspaceUri(workspaceUri);

	if (targets) {
		const cwd = targets.getCwd();

		progress.report(`Creating '${type}' build file..`);

		switch (type) {
			case `bob`:
				const bobProj = new BobProject(targets);
				const outFiles = bobProj.createRules();

				for (const filePath in outFiles) {
					fs.writeFileSync(path.join(cwd, filePath), outFiles[filePath]);
				}
				break;

			case `make`:
				const makeProj = new MakeProject(cwd, targets);
				fs.writeFileSync(path.join(cwd, `makefile`), makeProj.getMakefile().join(`\n`));
				break;

			case `imd`:
				const impactMarkdown = new ImpactMarkdown(cwd, targets, []);
				fs.writeFileSync(path.join(cwd, `impact.md`), impactMarkdown.getContent().join(`\n`));
				break;

			case `json`:
				const outJson = {
					targets: targets.getTargets(),
					resolved: targets.getResolvedObjects(),
					exports: targets.getExports(),
					messages: targets.logger.getAllLogs()
				};

				fs.writeFileSync(path.join(cwd, `sourceorbit.json`), JSON.stringify(outJson, null, 2));
				break;
		}
	} else {
		progress.report(`Failed to reload project`);
	}

	progress.done();
}

export async function fixProject(workspaceUri: string, suggestions: TargetSuggestions) {
	const uri = URI.parse(workspaceUri);
	const url = uri.fsPath;

	const progress = await connection.window.createWorkDoneProgress();

	progress.begin(`Source Orbit`, undefined, `Fetching files..`);
	
	const rfs = new ReadFileSystem();
	const files = await rfs.getFiles(url, SupportedGlob);

	const targets = new Targets(url, rfs);
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
		// Let's reinitialize the project when we're done.
		await initAndRefresh(workspaceUri);
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

	const validRenames: { [path: string]: string } = {};

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
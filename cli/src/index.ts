

import { writeFileSync } from 'fs';

import { ILEObject, Targets } from './targets';
import { MakeProject } from './builders/make';
import path from 'path';
import { BuildFiles, cliSettings, error, infoOut, warningOut } from './cli';
import { BobProject } from "./builders/bob";
import { ImpactMarkdown } from "./builders/imd";
import { referencesFileName } from "./extensions";
import { getBranchLibraryName } from "./builders/environment";
import { renameFiles, replaceIncludes } from './utils';
import { ReadFileSystem } from './readFileSystem';

const isCli = process.argv.length >= 2 && (process.argv[1].endsWith(`so`) || process.argv[1].endsWith(`index.js`));

if (isCli || process.env.VSCODE_INSPECTOR_OPTIONS) {
	cliSettings.cliMode = true;
	main();
}

async function main() {
	const parms = process.argv.slice(2);
	let cwd = process.cwd();
	let scanGlob: string|undefined = undefined;

	for (let i = 0; i < parms.length; i++) {
		switch (parms[i]) {
			case `-g`:
			case `--glob`:
				scanGlob = parms[i + 1];
				i++;
				break;

			case `-d`:
			case `--cwd`:
				cwd = parms[i + 1];
				i++;
				break;

			case `-ar`:
				warningOut(`Auto rename enabled. No makefile will be generated.`)
				cliSettings.autoRename = true;
				break;

			case `-fi`:
				warningOut(`Include fix enabled enabled.`)
				cliSettings.fixIncludes = true;
				break;

			case `--verbose`:
				cliSettings.infoMessages = true;
				break;

			case `-bf`:
				cliSettings.buildFile = parms[i + 1] as BuildFiles;
				i++;
				break;

			case `-wa`:
				cliSettings.withActions = true;
				break;

			case '-nc':
			case '--no-children':
				warningOut(`--no-children is deprecated and is default when doing partial builds.`);
				break;

			case `-wp`:
			case `--with-parents`:
				cliSettings.makefileWithParents = true;
				break;

			case '-ap':
			case '--assume-programs':
				cliSettings.assumeSourcesArePrograms = true;
				break;

			case `-bl`:
				cliSettings.userBranch = parms[i+1];
				i++;
				break;
			case `-f`:
			case `--files`:
			case `-l`:
				cliSettings.fileList = true;
				break;

			case `-h`:
			case `--help`:
				console.log(``);
				console.log(`\t-d <dir>`)
				console.log(`\t--cwd <dir>\tTo see the directory of where source code lives.`);
				console.log(`\t\t\tThe default is the current working directory.`);
				console.log(``);
				console.log(`\t-f <relative paths>`)
				console.log(`\t--files <relative paths>`);
				console.log(`\t\t\tthis option will still scan the entire working`);
				console.log(`\t\t\tdirectory, but will single out these specific files`);
				console.log(`\t\t\twhen generating other files. If no build file is`);
				console.log(`\t\t\tspecified, dependencies will be printed.`);
				console.log(``);
				console.log(`\t-bf make|bob|imd|json\tCreate build files of a specific format`);
				console.log(`\t\t\t\tExample: -bf make`);
				console.log(``);
				console.log(`\t-wa\t\tWhen using '-bf make', use commands`);
				console.log(`\t\t\tthat are found in the 'actions.json' files.`);
				console.log(``);
				console.log(`\t-bl <name>\tSet the BRANCHLIB environment variable based on `);
				console.log(`\t\t\ta user provided branch name, and will write it out.`);
				console.log(`\t\t\tExample: -bl feature/123-cool-idea`);
				console.log(`\t\t\t\t -bl bug/123-bad-move`);
				console.log(``);
				console.log(`\t-ap`)
				console.log(`\t--assume-programs`);
				console.log(`\t\t\tAssume that all sources are programs. This is useful`);
				console.log(`\t\t\tif your program sources do not use the .pgm notation.`);
				console.log(``);
				console.log(`Options for project cleanup:`);
				console.log(``);
				console.log(`\t-ar\t\tRun the auto-rename process after scanning all code`);
				console.log(`\t\t\tEnsure it is run inside of source control.`);
				console.log(`\t\t\tRename program sources to have the '.pgm.' attribute in the name`);
				console.log(`\t\t\tRename RPGLE copybooks found (based on include statements) to be '.rpgleinc'`);
				console.log(``);
				console.log(`\t-fi\t\tFix includes for RPGLE sources`);
				console.log(`\t\t\tEnsure it is run inside of source control.`);
				console.log(`\t\t\tFixes all include/copy directives to use local source if available`);
				console.log(``);
				console.log(`\t--verbose\tPrint all the detail.`);
				console.log(``);
				console.log(`Options specific to '-bf make':`);
				console.log(``);
				console.log(`\t-wp`);
				console.log(`\t--with-parents\tUsed with '-bf make' and will add parents of`);
				console.log(`\t\t\tobjects being partially built to the makefile.`);
				console.log(``);
				process.exit(0);
				break;

			default:
				if (cliSettings.fileList) {
					cliSettings.lookupFiles.push(parms[i]);
				}
				break;
		}
	}

	if (cliSettings.userBranch) {
		const branchLibrary = getBranchLibraryName(cliSettings.userBranch);
		console.log(branchLibrary);
		
		process.env.BRANCHLIB = branchLibrary;
		process.exit(0);
	}

	const fs = new ReadFileSystem();
	const targets = new Targets(cwd, fs);

	targets.setSuggestions({
		includes: cliSettings.fixIncludes,
		renames: cliSettings.autoRename
	});

	targets.setAssumePrograms(cliSettings.assumeSourcesArePrograms);

	let files: string[];

	if (!scanGlob) {
		scanGlob = targets.getSearchGlob();
	}

	try {
		files = await fs.getFiles(cwd, scanGlob);
	} catch (e) {
		error(e.message || e);
		process.exit(1);
	}

	const referenceFile = path.join(cwd, referencesFileName);
	if (await fs.exists(referenceFile)) {
		infoOut(`Found reference file: ${referenceFile}`);
		await targets.handleRefsFile(referenceFile);
	}

	await targets.loadObjectsFromPaths(files);

	for (const filePath of files) {
		const result = await targets.parseFile(filePath);
		if (!result) {
			error(`Report this issue to us with an example: github.com/IBM/sourceorbit/issues`);
		}
	}

	let exitEarly = false;

	if (cliSettings.autoRename) {
		renameFiles(targets.logger);
		exitEarly = true;
	}

	if (cliSettings.fixIncludes) {
		replaceIncludes(targets.logger);
		exitEarly = true;
	}

	if (exitEarly) {
		process.exit();
	}

	targets.resolveBinder();
	
	if (cliSettings.lookupFiles && cliSettings.buildFile === `none`) {
		for (const value of cliSettings.lookupFiles) {
			await listDeps(cwd, targets, value);
		}
	}

	switch (cliSettings.buildFile) {
		case `bob`:
			const bobProj = new BobProject(targets);
			const outFiles = bobProj.createRules();

			for (const filePath in outFiles) {
				writeFileSync(path.join(cwd, filePath), outFiles[filePath]);
			}

			break;
		case `make`:
			const makeProj = new MakeProject(cwd, targets, fs);

			// Enables the use of actions.json for commands
			if (cliSettings.withActions) {
				makeProj.useActions();
			}

			await makeProj.setupSettings();
			
			makeProj.setPartialWithImpacts(cliSettings.makefileWithParents);

			let specificObjects: ILEObject[] | undefined = cliSettings.fileList ? cliSettings.lookupFiles.map(f => targets.getResolvedObject(path.join(cwd, f))).filter(o => o) : undefined;
			writeFileSync(path.join(cwd, `makefile`), makeProj.getMakefile(specificObjects).join(`\n`));
			
			break;
		case `imd`:
			const impactMarkdown = new ImpactMarkdown(cwd, targets, cliSettings.lookupFiles);
			writeFileSync(path.join(cwd, `impact.md`), impactMarkdown.getContent().join(`\n`));
			break;

		case `json`:
			const outJson = {
				targets: targets.getTargets(),
				resolved: targets.getResolvedObjects(),
				exports: targets.getExports(),
				messages: targets.logger.getAllLogs()
			};

			writeFileSync(path.join(cwd, `sourceorbit.json`), JSON.stringify(outJson, null, 2));
			break;
	}
}

/**
 * @param query Can be object (ABCD.PGM) or relative path
 */
async function listDeps(cwd: string, targets: Targets, query: string) {
	const fullPath = path.join(cwd, query);

	let [name, type] = query.split(`.`);

	if (name) name = name.toUpperCase();
	if (type) type = type.toUpperCase();

	let theObject = targets.getResolvedObjects().find(o => o.systemName === name && o.type === type);

	if (!theObject) {
		theObject = await targets.resolvePathToObject(fullPath);
	}

	const allDeps = targets.getTargets();
	let currentTree: ILEObject[] = [];

	function lookupObject(ileObject: ILEObject) {
		console.log(`${''.padEnd(currentTree.length, `\t`)}${ileObject.systemName}.${ileObject.type} (${ileObject.relativePath || `no source`})`);

		currentTree.push(ileObject);

		for (const target of allDeps) {
			const containsLookup = target.deps.some(d => d.systemName === ileObject.systemName && d.type === ileObject.type);
			const circular = currentTree.some(d => d.systemName === target.systemName && d.type === target.type);

			if (containsLookup && !circular) {
				lookupObject(target);
			}
		}

		currentTree.pop();
	}

	lookupObject(theObject);
}

export { Targets } from './targets';
export { TargetsLanguageProvider } from './targets/languages';
export { MakeProject } from './builders/make';
export { BobProject } from "./builders/bob";
export { ImpactMarkdown } from "./builders/imd"
export * as Utils from './utils';
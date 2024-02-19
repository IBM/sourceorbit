import glob from 'glob';
import path from 'path';
import fs from 'fs/promises';
import Cache from "vscode-rpgle/language/models/cache";
import { IncludeStatement } from "vscode-rpgle/language/parserTypes";
import { infoOut, warningOut } from './cli';
import { DefinitionType, File, Module, CLParser } from 'vscode-clle/language';
import { DisplayFile as dds } from "vscode-displayfile/src/dspf";
import Document from "vscode-db2i/src/language/sql/document";
import { StatementType } from 'vscode-db2i/src/language/sql/types';
import { rpgExtensions, clExtensions, ddsExtension, sqlExtensions, srvPgmExtensions, cmdExtensions } from './extensions';
import Parser from "vscode-rpgle/language/parser";
import { setupParser } from './parser';
import { Logger } from './logger';
import { asPosix, getReferenceObjectsFrom, getSystemNameFromPath, toLocalPath } from './utils';

export type ObjectType = "PGM" | "SRVPGM" | "MODULE" | "FILE" | "BNDDIR" | "DTAARA" | "CMD" | "MENU" | "DTAQ";

const ignoredObjects = [`QSYSPRT`, `QCMDEXC`, `*LDA.DTAARA`, `QDCXLATE`, `QUSRJOBI`, `QTQCVRT`, `QWCRDTAA`, `QUSROBJD`, `QUSRMBRD`, `QUSROBJD`, `QUSLOBJ`, `QUSRTVUS`, `QUSCRTUS`];

const sqlTypeExtension = {
	'TABLE': `table`,
	'VIEW': `view`,
	'PROCEDURE': `sqlprc`,
	'FUNCTION': `sqludf`,
	'TRIGGER': `sqltrg`,
	'ALIAS': `sqlalias`,
	'SEQUENCE': `sqlseq`
};

const bindingDirectoryTarget: ILEObject = { systemName: `$(APP_BNDDIR)`, type: `BNDDIR` };

const TextRegex = /\%TEXT.*(?=\n|\*)/gm

export interface ILEObject {
	systemName: string;
	longName?: string;
	type: ObjectType;
	text?: string,
	relativePath?: string;
	extension?: string;

	reference?: boolean;

	/** exported functions */
	exports?: string[];
	/** each function import in the object */
	imports?: string[];
}

export interface ILEObjectTarget extends ILEObject {
	deps: ILEObject[];
}

export interface TargetSuggestions {
	renames?: boolean;
	includes?: boolean;
}

export interface ImpactedObject {
	ileObject: ILEObject,
	children: ImpactedObject[]
}

interface RpgLookup {
	lookup: string,
	line?: number
}

interface FileOptions {
	isFree?: boolean;
	text?: string;
}

/**
 * This class is responsible for storing all the targets
 * and their dependencies. It also handles the parsing
 * of files and the creation of targets.
 * 
 * const files = getAllFilesInDir(`.`);
 * const targets = new Targets(cwd);
 * targets.handlePseudoFile(pseudoFilePath);
 * targets.loadObjectsFromPaths(files);
 * await Promise.all(files.map(f => targets.parseFile(f)));
 * targets.resolveBinder();
 */

export class Targets {
	private rpgParser: Parser;

	/* pathCache and resolvedSearches are used for file resolving. */
	private pathCache: { [path: string]: true | string[] } | undefined;
	private resolvedSearches: { [query: string]: string } = {};

	private resolvedObjects: { [localPath: string]: ILEObject } = {};
	private resolvedExports: { [name: string]: ILEObject } = {};
	private targets: { [name: string]: ILEObjectTarget } = {};
	private needsBinder = false;

	private suggestions: TargetSuggestions = {};

	public logger: Logger;

	constructor(private cwd: string) {
		this.rpgParser = setupParser(this);
		this.logger = new Logger();
	}

	public getCwd() {
		return this.cwd;
	}

	public setSuggestions(newSuggestions: TargetSuggestions) {
		this.suggestions = newSuggestions;
	}

	public getBinderTarget() {
		return bindingDirectoryTarget;
	}

	public getRelative(fullPath: string) {
		return path.relative(this.cwd, fullPath);
	}

	private storeResolved(localPath: string, ileObject: ILEObject) {
		this.resolvedObjects[localPath] = ileObject;
	}

	public resolvePathToObject(localPath: string, newText?: string) {
		if (this.resolvedObjects[localPath]) {
			if (newText) this.resolvedObjects[localPath].text = newText;
			return this.resolvedObjects[localPath];
		}

		const detail = path.parse(localPath);
		const relativePath = this.getRelative(localPath);

		const isProgram = detail.name.toUpperCase().endsWith(`.PGM`);
		const name = getSystemNameFromPath(isProgram ? detail.name.substring(0, detail.name.length - 4) : detail.name);
		const extension = detail.ext.length > 1 ? detail.ext.substring(1) : detail.ext;
		const type: ObjectType = (isProgram ? "PGM" : this.getObjectType(relativePath, extension));

		const theObject: ILEObject = {
			systemName: name,
			type: type,
			text: newText,
			relativePath,
			extension
		};

		this.storeResolved(localPath, theObject);

		return theObject;
	}

	/**
	 * This can be expensive. It should only be called:
	 * before loadObjectsFromPaths and parseFile are called.
	 * @param filePath Fully qualified path to the file. Assumed to exist.
	 */
	public async handleRefsFile(filePath: string) {
		const content = await fs.readFile(filePath, { encoding: `utf-8` });

		const pseudoObjects = getReferenceObjectsFrom(content);

		pseudoObjects.forEach(ileObject => {
			if (!this.searchForObject(ileObject)) {
				const key = `/${ileObject.systemName}.${ileObject.type}`;
				ileObject.reference = true;
				this.resolvedObjects[key] = ileObject;
			}
		});
	}

	public removeObjectByPath(localPath: string) {
		const resolvedObject = this.resolvedObjects[localPath];

		if (resolvedObject) {
			// First, delete the simple caches
			this.resolvedObjects[localPath] = undefined;

			return this.removeObject(resolvedObject);
		}

		return []
	}

	public removeObject(resolvedObject: ILEObject) {
		let impactedTargets: ILEObject[] = [];

		for (const targetId in this.targets) {
			const target = this.targets[targetId];

			if (target) {
				const depIndex = target.deps.findIndex(d => (d.systemName === resolvedObject.systemName && d.type === resolvedObject.type) || d.relativePath === resolvedObject.relativePath);

				if (depIndex >= 0) {
					impactedTargets.push(target);
					target.deps.splice(depIndex, 1);

					if (target.relativePath) {
						this.logger.fileLog(target.relativePath, {
							type: `info`,
							message: `This object depended on ${resolvedObject.systemName}.${resolvedObject.type} before it was deleted.`
						})
					}
				}
			}
		}

		// Remove it as a global target
		this.targets[`${resolvedObject.systemName}.${resolvedObject.type}`] = undefined;
		this.resolvedSearches[`${resolvedObject.systemName}.${resolvedObject.type}`] = undefined;

		// Remove possible logs
		if (resolvedObject.relativePath) {
			this.logger.flush(resolvedObject.relativePath)
		}

		return impactedTargets;
	}

	/**
	 * Resolves a search to an object. Use `systemName` parameter for short and long name.
	 */
	public searchForObject(lookFor: ILEObject) {
		return this.getResolvedObjects().find(o => (o.systemName === lookFor.systemName || o.systemName === lookFor.longName) && o.type === lookFor.type);
	}

	public searchForAnyObject(lookFor: { name: string, types: ObjectType[] }) {
		lookFor.name = lookFor.name.toUpperCase();
		return this.getResolvedObjects().find(o => (o.systemName === lookFor.name || o.longName?.toUpperCase() === lookFor.name) && lookFor.types.includes(o.type));
	}

	public resolveLocalFile(name: string, baseFile?: string): string | undefined {
		name = name.toUpperCase();

		if (this.resolvedSearches[name]) return this.resolvedSearches[name];

		if (!this.pathCache) {
			// We don't really want to spam the FS
			// So we can a list of files which can then
			// use in glob again later.
			this.pathCache = {};

			glob.sync(`**/*`, {
				cwd: this.cwd,
				absolute: true,
				nocase: true,
			}).forEach(localPath => {
				this.pathCache[localPath] = true;
			});
		}

		let globString = `**/${name}*`;

		const results = glob.sync(globString, {
			cwd: this.cwd,
			absolute: true,
			nocase: true,
			ignore: baseFile ? `**/${baseFile}` : undefined,
			cache: this.pathCache
		});

		if (results[0]) {
			// To local path is required because glob returns posix paths
			const localPath = toLocalPath(results[0])
			this.resolvedSearches[name] = localPath;
			return localPath;
		}
	}

	private getObjectType(relativePath: string, ext: string): ObjectType {
		switch (ext.toLowerCase()) {
			case `dspf`:
			case `prtf`:
			case `pf`:
			case `lf`:
			case `sql`:
			case `table`:
			case `view`:
			case `index`:
			case `alias`:
			case `sqludf`:
			case `sqludt`:
			case `sqlalias`:
			case `sqlseq`:
			case `sequence`:
			case `msgf`:
				return "FILE";

			case `dtaara`:
				return "DTAARA";

			case `cmd`:
				return "CMD";

			case `rpgle`:
			case `sqlrpgle`:
			case `clle`:
			case `cl`:
				return "MODULE";

			case `binder`:
			case `bnd`:
			case `function`:
				return `SRVPGM`;

			case `procedure`:
			case `trigger`:
			case `sqlprc`:
			case `sqltrg`:
				return `PGM`;

			default:
				this.logger.fileLog(relativePath, {
					type: `warning`,
					message: `'${ext}' not found a matching object type. Defaulting to '${ext}'`
				});
				return (ext.toUpperCase() as ObjectType);
		}
	}

	public loadObjectsFromPaths(paths: string[]) {
		paths.forEach(p => this.resolvePathToObject(p));
	}

	public async parseFile(filePath: string) {
		const pathDetail = path.parse(filePath);
		const relative = this.getRelative(filePath);

		let success = true;

		if (pathDetail.ext.length > 1) {
			if (!this.suggestions.renames) {
				// Don't clear the logs if we're suggestion renames.
				this.logger.flush(relative);
			}

			const ext = pathDetail.ext.substring(1).toLowerCase();

			try {
				const content = await fs.readFile(filePath, { encoding: `utf-8` });
				const eol = content.indexOf(`\r\n`) >= 0 ? `\r\n` : `\n`;

				// Really only applied to rpg
				const isFree = (content.length >= 6 ? content.substring(0, 6).toLowerCase() === `**free` : false);

				let textMatch;
				try {
					[textMatch] = content.match(TextRegex);
					if (textMatch) {
						if (textMatch.startsWith(`%TEXT`)) textMatch = textMatch.substring(5);
						if (textMatch.endsWith(`*`)) textMatch = textMatch.substring(0, textMatch.length - 1);
						textMatch = textMatch.trim();
					}
				} catch (e) { }

				const options: FileOptions = {
					isFree,
					text: textMatch
				};

				if (rpgExtensions.includes(ext)) {
					const rpgDocs = await this.rpgParser.getDocs(
						filePath,
						content,
						{
							ignoreCache: true,
							withIncludes: true
						}
					);

					if (rpgDocs) {
						this.createRpgTarget(filePath, rpgDocs, options);
					}

				}
				else if (clExtensions.includes(ext)) {
					const clDocs = new CLParser();
					const tokens = clDocs.parseDocument(content);

					const module = new Module();
					module.parseStatements(tokens);

					this.createClTarget(filePath, module, options);
				}
				else if (ddsExtension.includes(ext)) {
					const ddsFile = new dds();
					ddsFile.parse(content.split(eol));

					this.createDdsFileTarget(filePath, ddsFile, options);
				}
				else if (sqlExtensions.includes(ext)) {
					const sqlDoc = new Document(content);
					this.createSqlTargets(filePath, sqlDoc, options);
				}
				else if (srvPgmExtensions.includes(ext)) {
					const clDocs = new CLParser();
					const tokens = clDocs.parseDocument(content);

					const module = new Module();
					module.parseStatements(tokens);

					this.createSrvPgmTarget(filePath, module, options);
				}
				else if (cmdExtensions.includes(ext)) {
					this.createCmdTarget(filePath, options);
				}
			} catch (e) {
				this.logger.fileLog(relative, {
					message: `Failed to parse file.`,
					type: `warning`
				});

				console.log(relative);
				console.log(e);

				success = false;
			}

			infoOut(``);
		} else {
			success = false;
		}

		return success;

	}

	private createCmdTarget(localPath, options: FileOptions = {}) {
		this.resolvePathToObject(localPath, options.text);

		// Since cmd source doesn't explicity contains deps, we resolve later on
	}

	private createSrvPgmTarget(localPath: string, module: Module, options: FileOptions = {}) {
		const ileObject = this.resolvePathToObject(localPath, options.text);
		const target: ILEObjectTarget = {
			...ileObject,
			deps: [],
			exports: []
		};

		if (ileObject.extension === `binder`) {
			const pathDetail = path.parse(localPath);

			if (this.suggestions.renames) {
				this.logger.fileLog(ileObject.relativePath, {
					message: `Rename suggestion`,
					type: `rename`,
					change: {
						rename: {
							path: localPath,
							newName: pathDetail.name + `.bnd`
						}
					}
				});
			} else {
				this.logger.fileLog(ileObject.relativePath, {
					message: `Extension is '${ileObject.extension}'. Consolidate by using 'bnd'?`,
					type: `warning`,
				});
			}
		}

		const validStatements = module.statements.filter(s => {
			const possibleObject = s.getObject();
			return (possibleObject && possibleObject.name && [`STRPGMEXP`, `ENDPGMEXP`, `EXPORT`].includes(possibleObject.name));
		});

		for (const statement of validStatements) {
			const currentCommand = statement.getObject().name;
			if (currentCommand === `EXPORT`) {
				const parms = statement.getParms();
				const symbolTokens = parms[`SYMBOL`];

				if (symbolTokens.block && symbolTokens.block.length === 1 && symbolTokens.block[0].type === `string` && symbolTokens.block[0].value) {
					target.exports.push(trimQuotes(symbolTokens.block[0].value));
				} else
					if (symbolTokens.block && symbolTokens.block.length === 1 && symbolTokens.block[0].type === `word` && symbolTokens.block[0].value) {
						target.exports.push(trimQuotes(symbolTokens.block[0].value, `"`));
					} else {
						this.logger.fileLog(ileObject.relativePath, {
							message: `Invalid EXPORT found. Single quote string expected.`,
							type: `warning`,
							range: {
								start: symbolTokens.range.start,
								end: symbolTokens.range.end
							}
						})
					}

			} else
				if (currentCommand === `ENDPGMEXP`) {
					// Return, we only really care about the first export block
					break;
				}
		}

		this.addNewTarget(target);
	}

	/**
	 * Handles all DDS types: pf, lf, dspf
	 */
	private createDdsFileTarget(localPath: string, dds: dds, options: FileOptions = {}) {
		const ileObject = this.resolvePathToObject(localPath, options.text);
		const target: ILEObjectTarget = {
			...ileObject,
			deps: []
		};

		infoOut(`${ileObject.systemName}.${ileObject.type}: ${ileObject.relativePath}`);

		// We have a local cache of refs found so we don't keep doing global lookups
		// on objects we already know to depend on in this object.
		
		let alreadyFoundRefs: string[] = [];

		const handleObjectPath = (currentKeyword: string, recordFormat: any, value: string) => {
			const qualified = value.split(`/`);

			let objectName: string | undefined;
			if (qualified.length === 2 && qualified[0].toLowerCase() === `*libl`) {
				objectName = qualified[1];
			} else if (qualified.length === 1) {
				objectName = qualified[0];
			}

			if (objectName) {
				const upperName = objectName.toUpperCase();
				if (alreadyFoundRefs.includes(upperName)) return;

				const resolvedPath = this.searchForObject({ systemName: upperName, type: `FILE` });
				if (resolvedPath) {
					target.deps.push(resolvedPath);
					alreadyFoundRefs.push(upperName);
				}
				else {
					this.logger.fileLog(ileObject.relativePath, {
						message: `no object found for reference '${objectName}'`,
						type: `warning`,
						line: recordFormat.range.start
					});
				}
			} else {
				this.logger.fileLog(ileObject.relativePath, {
					message: `${currentKeyword} reference not included as possible reference to library found.`,
					type: `info`,
					line: recordFormat.range.start
				});
			}
		}

		// PFILE -> https://www.ibm.com/docs/en/i/7.5?topic=80-pfile-physical-file-keywordlogical-files-only
		// REF -> https://www.ibm.com/docs/en/i/7.5?topic=80-ref-reference-keywordphysical-files-only

		const ddsRefKeywords = [`PFILE`, `REF`, `JFILE`];

		for (const recordFormat of dds.formats) {

			// Look through this record format keywords for the keyword we're looking for
			for (const keyword of ddsRefKeywords) {
				const keywordObj = recordFormat.keywords.find(k => k.name === keyword);
				if (keywordObj) {
					const wholeValue: string = keywordObj.value;
					const parts = wholeValue.split(` `).filter(x => x.length > 0);

					// JFILE can have multiple files referenced in it, whereas 
					// REF and PFILE can only have one at the first element
					const pathsToCheck = (keyword === `JFILE` ? parts.length : 1);

					for (let i = 0; i < pathsToCheck; i++) {
						handleObjectPath(keyword, recordFormat, parts[i]);
					}
				}
			}

			// REFFLD -> https://www.ibm.com/docs/en/i/7.5?topic=80-reffld-referenced-field-keywordphysical-files-only

			// Then, let's loop through the fields in this format and see if we can find REFFLD
			for (const field of recordFormat.fields) {
				const refFld = field.keywords.find(k => k.name === `REFFLD`);

				if (refFld) {
					const [fieldRef, fileRef] = refFld.value.trim().split(` `);

					if (fileRef) {
						handleObjectPath(`REFFLD`, recordFormat, fileRef);
					}
				}
			}
		}

		if (target.deps.length > 0)
			infoOut(`Depends on: ${target.deps.map(d => `${d.systemName}.${d.type}`).join(` `)}`);

		this.addNewTarget(target);
	}

	private createClTarget(localPath: string, module: Module, options: FileOptions = {}) {
		const pathDetail = path.parse(localPath);
		const sourceName = pathDetail.base;
		const ileObject = this.resolvePathToObject(localPath);
		const target: ILEObjectTarget = {
			...ileObject,
			deps: []
		};

		infoOut(`${ileObject.systemName}.${ileObject.type}: ${ileObject.relativePath}`);

		if (ileObject.extension?.toLowerCase() === `clp`) {
			if (this.suggestions.renames) {
				this.logger.fileLog(ileObject.relativePath, {
					message: `Rename suggestion`,
					type: `rename`,
					change: {
						rename: {
							path: localPath,
							newName: pathDetail.name + `.pgm.clle`
						}
					}
				});
			} else {
				this.logger.fileLog(ileObject.relativePath, {
					message: `Extension is '${ileObject.extension}', but Source Orbit doesn't support CLP. Is it possible the extension should use '.pgm.clle'?`,
					type: `warning`,
				});
			}

		} else {
			if (ileObject.type === `MODULE`) {
				if (this.suggestions.renames) {
					this.logger.fileLog(ileObject.relativePath, {
						message: `Rename suggestion`,
						type: `rename`,
						change: {
							rename: {
								path: localPath,
								newName: pathDetail.name + `.pgm` + pathDetail.ext
							}
						}
					});
				} else {
					this.logger.fileLog(ileObject.relativePath, {
						message: `Type detected as ${ileObject.type} but Source Orbit doesn't support CL modules. Is it possible the extension should include '.pgm'?`,
						type: `warning`,
					});
				}
			}
		}

		const files = module.getDefinitionsOfType<File>(DefinitionType.File);

		// Loop through local file defs to find a possible dep
		files.forEach(def => {
			const possibleObject = def.file;
			if (possibleObject) {
				if (possibleObject.library) {
					this.logger.fileLog(ileObject.relativePath, {
						message: `Definition to ${possibleObject.library}/${possibleObject.name} ignored due to qualified path.`,
						range: {
							start: def.range.start,
							end: def.range.end
						},
						type: `info`,
					});

				} else {
					if (ignoredObjects.includes(possibleObject.name.toUpperCase())) return;

					const resolvedPath = this.searchForObject({ systemName: possibleObject.name.toUpperCase(), type: `FILE` });
					if (resolvedPath) target.deps.push(resolvedPath);
					else {
						this.logger.fileLog(ileObject.relativePath, {
							message: `no object found for reference '${possibleObject.name}'`,
							range: {
								start: def.range.start,
								end: def.range.end
							},
							type: `warning`,
						});
					}
				}
			}
		});

		module.statements.filter(s => {
			const possibleObject = s.getObject();
			return (possibleObject && possibleObject.name && possibleObject.name === `CALL`);
		}).forEach(s => {

			const parms = s.getParms();
			const pgmParm = parms[`PGM`];

			if (pgmParm && pgmParm.block) {
				const block = pgmParm.block;
				if (block.length === 1) {
					const name = block[0].value!;

					if (ignoredObjects.includes(name.toUpperCase())) return;

					const resolvedPath = this.searchForObject({ systemName: name.toUpperCase(), type: `PGM` });
					if (resolvedPath) target.deps.push(resolvedPath);
					else {
						this.logger.fileLog(ileObject.relativePath, {
							message: `no object found for reference '${name}'`,
							range: {
								start: pgmParm.range.start,
								end: pgmParm.range.end
							},
							type: `warning`,
						});
					}
				} else {
					this.logger.fileLog(ileObject.relativePath, {
						message: `PGM call not included as possible reference to library.`,
						range: {
							start: pgmParm.range.start,
							end: pgmParm.range.end
						},
						type: `info`,
					});
				}
			}
		});

		// We also look to see if there is a `.cmd` object with the same name
		const possibleCommandObject = this.searchForObject({ systemName: ileObject.systemName, type: `CMD` });
		if (possibleCommandObject) this.createOrAppend(possibleCommandObject, target);

		if (target.deps.length > 0)
			infoOut(`Depends on: ${target.deps.map(d => `${d.systemName}.${d.type}`).join(` `)}`);

		this.addNewTarget(target);
	}

	private createSqlTargets(localPath: string, document: Document, options: FileOptions = {}) {
		const pathDetail = path.parse(localPath);
		const relativePath = this.getRelative(localPath);

		const groups = document.getStatementGroups();

		// TODO: Note, this returns high level definitions.
		// If the index/view/etc specifies a table dep,
		// they will not appear as a dependency

		const createCount = groups.filter(g => g.statements[0].type === StatementType.Create).length;

		if (createCount > 1) {
			this.logger.fileLog(relativePath, {
				message: `Includes multiple create statements. They should be in individual sources. This file will not be parsed.`,
				type: `warning`,
			});

			return;
		}

		for (const group of groups) {
			const statement = group.statements[0];
			const defs = statement.getObjectReferences();
			const mainDef = defs[0];

			if (mainDef && mainDef.createType && mainDef.object.name) {
				const tokens = mainDef.tokens;
				if (mainDef.object.schema) {
					this.logger.fileLog(relativePath, {
						message: `${mainDef.object.schema}/${mainDef.object.name} (${mainDef.createType}) reference not included as possible reference to library found.`,
						range: {
							start: tokens[0].range.start,
							end: tokens[tokens.length - 1].range.end
						},
						type: `warning`,
					});

				} else {
					switch (statement.type) {
						// Alters are a little weird in that they can exist
						// in any file, so we can't assume the current source
						// is the name of the object. Sad times
						case StatementType.Alter:
							// We don't do anything for alter currently
							// because it's too easy to create circular deps.
							// This is bad!!
							this.logger.fileLog(relativePath, {
								message: `${mainDef.object.name} (${mainDef.createType}) alter not tracked due to possible circular dependency.`,
								range: {
									start: tokens[0].range.start,
									end: tokens[tokens.length - 1].range.end
								},
								type: `info`,
							});

							// let currentTarget: ILEObjectTarget|undefined;
							// const resolvedPath = this.resolveLocalObjectQuery(mainDef.object.name + `.*`);
							// const currentRelative = path.basename(resolvedPath);
							// if (resolvedPath) { 
							// 		currentTarget = {
							// 		...this.resolveObject(resolvedPath),
							// 		deps: []
							// 	};
							// }

							// if (currentTarget) {
							// 	info(`${currentTarget.name}.${currentTarget.type}`);
							// 	info(`\tSource: ${currentTarget.relativePath}`);

							// 	if (defs.length > 1) {
							// 		for (const def of defs.slice(1)) {
							// 			const subResolvedPath = this.resolveLocalObjectQuery(def.object.name + `.*`, currentRelative);
							// 			if (subResolvedPath) currentTarget.deps.push(this.resolveObject(subResolvedPath))
							// 			else info(`\tNo object found for reference '${def.object.name}'`);
							// 		}
							// 	}

							// 	if (currentTarget.deps.length > 0) {
							// 		info(`Depends on: ${currentTarget.deps.map(d => `${d.name}.${d.type}`).join(` `)}`);

							// 		this.pushDep(currentTarget);
							// 	}
							// }
							break;

						// Creates should be in their own unique file
						case StatementType.Create:
							let hasLongName = mainDef.object.name && mainDef.object.name.length > 10 ? mainDef.object.name : undefined;
							let objectName = mainDef.object.system || trimQuotes(mainDef.object.name, `"`);

							const extension = pathDetail.ext.substring(1);

							let ileObject: ILEObject = {
								systemName: objectName.toUpperCase(),
								longName: hasLongName,
								type: this.getObjectType(relativePath, mainDef.createType),
								text: options.text,
								relativePath,
								extension
							}

							let suggestRename = false;
							const sqlFileName = pathDetail.name;

							// First check the file name
							if (ileObject.systemName.length <= 10) {
								if (ileObject.systemName.toUpperCase() !== sqlFileName.toUpperCase() && ileObject.longName !== sqlFileName) {
									suggestRename = true;
								}
							}

							// Then make an extension suggestion
							if (extension.toUpperCase() === `SQL` && mainDef.createType) {
								suggestRename = true;
							}

							// Let them know to use a system name in the create statement if one is not present
							if (ileObject.systemName.length > 10 && mainDef.object.system === undefined) {
								this.logger.fileLog(ileObject.relativePath, {
									message: `${ileObject.systemName} (${ileObject.type}) name is longer than 10 characters. Consider using 'FOR SYSTEM NAME' in the CREATE statement.`,
									type: `warning`,
									range: {
										start: tokens[0].range.start,
										end: tokens[tokens.length - 1].range.end
									},
								});

								suggestRename = false;
							}

							let newTarget: ILEObjectTarget = {
								...ileObject,
								deps: []
							};

							infoOut(`${newTarget.systemName}.${newTarget.type}: ${newTarget.relativePath}`);

							// Now, let's go through all the other statements in this group (BEGIN/END)
							// and grab any references to other objects :eyes:
							let otherDefs = defs.slice(1);

							for (let i = 1; i < group.statements.length; i++) {
								const currentStatement = group.statements[i];
								if ([StatementType.Alter, StatementType.Insert, StatementType.Delete, StatementType.With, StatementType.Select, StatementType.Call].includes(currentStatement.type)) {
									otherDefs.push(...group.statements[i].getObjectReferences());
								}
							}

							for (const def of otherDefs) {
								const refTokens = def.tokens;
								const simpleName = trimQuotes(def.object.name, `"`);
								// TODO: do we need to look for SRVPGM (function) or PGM (procedure) here?
								const resolvedObject = this.searchForAnyObject({ name: simpleName, types: [`FILE`, `SRVPGM`, `PGM`] });
								if (resolvedObject) newTarget.deps.push(resolvedObject);
								else {
									this.logger.fileLog(newTarget.relativePath, {
										message: `No object found for reference '${def.object.name}'`,
										type: `warning`,
										range: {
											start: refTokens[0].range.start,
											end: refTokens[refTokens.length - 1].range.end
										},
									});
								}
							}

							if (newTarget.deps.length > 0) {
								infoOut(`Depends on: ${newTarget.deps.map(d => `${d.systemName}.${d.type}`).join(` `)}`);
							}

							// So we can later resolve the path to the created object
							this.storeResolved(localPath, ileObject);

							this.addNewTarget(newTarget);

							// If the extension is SQL, let's make better suggestions
							// based on the create type in the CREATE statement
							if (suggestRename) {
								const newExtension = sqlTypeExtension[mainDef.createType.toUpperCase()];

								if (newExtension) {
									const possibleName = (ileObject.longName ? ileObject.longName : ileObject.systemName.toLowerCase()) + `.` + newExtension;

									if (this.suggestions.renames) {
										const renameLogPath = relativePath;

										// We need to make sure the .rpgleinc rename is most important
										if (this.logger.exists(renameLogPath, `rename`)) {
											this.logger.flush(renameLogPath);
										}

										this.logger.fileLog(renameLogPath, {
											message: `Rename suggestion`,
											type: `rename`,
											change: {
												rename: {
													path: localPath,
													newName: possibleName
												}
											}
										});
									} else {
										this.logger.fileLog(relativePath, {
											message: `Extension should be based on type. Suggested name is '${possibleName}'`,
											type: `warning`,
										});
									}
								}
							}

							break;
					}

				}
			}
		}
	}

	private createRpgTarget(localPath: string, cache: Cache, options: FileOptions = {}) {
		const pathDetail = path.parse(localPath);
		const ileObject = this.resolvePathToObject(localPath, options.text);

		// define internal imports
		ileObject.imports = cache.procedures
			.filter((proc: any) => proc.keyword[`EXTPROC`])
			.map(ref => {
				const keyword = ref.keyword;
				let importName: string = ref.name;
				const extproc: string | boolean = keyword[`EXTPROC`];
				if (extproc) {
					if (extproc === true) importName = ref.name;
					else importName = extproc;
				}

				if (importName.includes(`:`)) {
					const parmParms = importName.split(`:`);
					importName = parmParms.filter(p => !p.startsWith(`*`)).join(``);
				}

				importName = trimQuotes(importName);

				return importName;
			});
	
		// define exported functions
		if (cache.keyword[`NOMAIN`]) {
			ileObject.exports = cache.procedures
				.filter((proc: any) => proc.keyword[`EXPORT`])
				.map(ref => ref.name.toUpperCase());
		}

		const target: ILEObjectTarget = {
			...ileObject,
			deps: []
		};

		infoOut(`${ileObject.systemName}.${ileObject.type}: ${ileObject.relativePath}`);

		cache.includes.forEach((include: IncludeStatement) => {
			// RPGLE includes are always returned as posix paths
			// even on Windows. We need to do some magic to convert here for Windows systems
			include.toPath = toLocalPath(include.toPath);

			const includeDetail = path.parse(include.toPath);

			if (includeDetail.ext !== `.rpgleinc`) {
				const possibleName = includeDetail.name.toLowerCase().endsWith(`.pgm`) ? includeDetail.name.substring(0, includeDetail.name.length - 4) : includeDetail.name;

				if (this.suggestions.renames) {
					const renameLogPath = this.getRelative(include.toPath);

					// We need to make sure the .rpgleinc rename is most important
					if (this.logger.exists(renameLogPath, `rename`)) {
						this.logger.flush(renameLogPath);
					}

					this.logger.fileLog(renameLogPath, {
						message: `Rename suggestion`,
						type: `rename`,
						change: {
							rename: {
								path: include.toPath,
								newName: `${possibleName}.rpgleinc`
							}
						}
					});
				} else {
					this.logger.fileLog(this.getRelative(include.toPath), {
						message: `referenced as include, but should use the '.rpgleinc' extension.`,
						type: `warning`,
					});
				}
			}

			if (this.suggestions.includes) {
				this.logger.fileLog(ileObject.relativePath, {
					message: `Will update to use unix style path.`,
					type: `includeFix`,
					line: include.line,
					change: {
						lineContent: (options.isFree ? `` : ``.padEnd(6)) + `/copy '${asPosix(this.getRelative(include.toPath))}'`
					}
				});
			} else {
				this.logger.fileLog(ileObject.relativePath, {
					message: `Include at line ${include.line} found, to path '${asPosix(this.getRelative(include.toPath))}'`,
					type: `info`,
					line: include.line,
				});
			}
		});

		// This usually means .pgm is in the name
		if (ileObject.type === `PGM` && cache.keyword[`NOMAIN`]) {
			const possibleName = pathDetail.name.toLowerCase().endsWith(`.pgm`) ? pathDetail.name.substring(0, pathDetail.name.length - 4) : pathDetail.name;

			if (this.suggestions.renames) {
				this.logger.fileLog(ileObject.relativePath, {
					message: `Rename suggestion`,
					type: `rename`,
					change: {
						rename: {
							path: localPath,
							newName: possibleName + pathDetail.ext
						}
					}
				})
			} else {
				this.logger.fileLog(ileObject.relativePath, {
					message: `type detected as ${ileObject.type} but NOMAIN keyword found.`,
					type: `warning`,
				});
			}
		}

		// This usually means it's source name is a module (no .pgm) but doesn't have NOMAIN.
		// We need to do this for other language too down the line
		if (ileObject.type === `MODULE` && !cache.keyword[`NOMAIN`]) {
			if (this.suggestions.renames) {
				this.logger.fileLog(ileObject.relativePath, {
					message: `Rename suggestion`,
					type: `rename`,
					change: {
						rename: {
							path: localPath,
							newName: pathDetail.name + `.pgm` + pathDetail.ext
						}
					}
				});
			} else {
				this.logger.fileLog(ileObject.relativePath, {
					message: `type detected as ${ileObject.type} but NOMAIN keyword was not found. Is it possible the extension should include '.pgm'?`,
					type: `warning`,
				});
			}

		}

		if (cache.keyword[`BNDDIR`]) {
			this.logger.fileLog(ileObject.relativePath, {
				message: `has the BNDDIR keyword. 'binders' property in iproj.json should be used instead.`,
				type: `info`,
			});
		}

		// Find external programs
		cache.procedures
			.filter((proc: any) => proc.keyword[`EXTPGM`])
			.map((ref): RpgLookup => {
				const keyword = ref.keyword;
				let fileName = ref.name;
				const extpgm = keyword[`EXTPGM`];
				if (extpgm) {
					if (extpgm === true) fileName = ref.name;
					else fileName = trimQuotes(extpgm);
				}

				return {
					lookup: fileName.toUpperCase(),
					line: ref.position ? ref.position.line : undefined
				};
			})
			.forEach((ref: RpgLookup) => {
				// Don't add ignored objects (usually system APIs)
				if (ignoredObjects.includes(ref.lookup)) return;
				// Don't add itself
				if (ref.lookup === ileObject.systemName) return;

				const resolvedObject = this.searchForObject({ systemName: ref.lookup, type: `PGM` });
				if (resolvedObject) {
					// because of legacy fixed CALL, there can be dupliicate EXTPGMs with the same name :(
					if (!target.deps.some(d => d.systemName === resolvedObject.systemName && d.type && resolvedObject.type)) {
						target.deps.push(resolvedObject)
					}
				}

				else {
					this.logger.fileLog(ileObject.relativePath, {
						message: `No object found for reference '${ref.lookup}'`,
						type: `warning`,
						line: ref.line
					});
				}
			});

		// Scan the multiple scopes available in an RPGLE program
		const scopes = [cache, ...cache.procedures.map(p => p.scope)].filter(s => s);

		for (const scope of scopes) {

			// Find external data structure sources
			scope.structs
				.filter((struct: any) => struct.keyword[`EXTNAME`])
				.map((struct): RpgLookup => {
					const keyword = struct.keyword;
					const value = trimQuotes(keyword[`EXTNAME`]);

					return {
						lookup: value.split(`:`)[0].toUpperCase(),
						line: struct.position ? struct.position.line : undefined
					};
				})
				.forEach((ref: RpgLookup) => {
					const resolvedObject = this.searchForObject({ systemName: ref.lookup, type: `FILE` });
					if (resolvedObject) target.deps.push(resolvedObject)
					else {
						this.logger.fileLog(ileObject.relativePath, {
							message: `No object found for reference '${ref.lookup}'`,
							type: `warning`,
							line: ref.line
						});
					}
				});

			// Find external files
			scope.files
				.map((file): RpgLookup => {
					let possibleName: string = file.name;
					const keyword = file.keyword;

					const extNameValue = keyword[`EXTFILE`];
					if (extNameValue) {
						possibleName = trimQuotes(extNameValue).split(`:`)[0]
					}

					if (possibleName.toLowerCase() === `*extdesc`) {
						const extDescValue = keyword[`EXTDESC`];
						if (extDescValue) {
							possibleName = trimQuotes(extDescValue);
						} else {
							this.logger.fileLog(ileObject.relativePath, {
								message: `*EXTDESC is used for '${file.name}' but EXTDESC keyword not found`,
								type: `warning`,
							});
						}
					}

					return {
						lookup: possibleName.toUpperCase(),
						line: file.position ? file.position.line : undefined
					};
				})
				.forEach((ref: RpgLookup) => {
					if (ignoredObjects.includes(ref.lookup)) return;

					const resolvedObject = this.searchForObject({ systemName: ref.lookup, type: `FILE` });
					if (resolvedObject) target.deps.push(resolvedObject)
					else {
						this.logger.fileLog(ileObject.relativePath, {
							message: `No object found for reference '${ref.lookup}'`,
							type: `warning`,
							line: ref.line
						});
					}
				});

			// We ignore anything with hardcoded schemas
			scope.sqlReferences
				.filter(ref => !ref.description)
				.map((ref): RpgLookup => ({
					lookup: trimQuotes(ref.name, `"`).toUpperCase(),
					line: ref.position ? ref.position.line : undefined
				}))
				.forEach((ref: RpgLookup) => {
					const previouslyScanned = target.deps.some((r => r.systemName === ref.lookup && r.type === `FILE`));
					if (previouslyScanned) return;
					const resolvedObject = this.searchForObject({ systemName: ref.lookup, type: `FILE` });
					if (resolvedObject) target.deps.push(resolvedObject)
					else {
						this.logger.fileLog(ileObject.relativePath, {
							message: `No object found for reference '${ref.lookup}'`,
							type: `warning`,
							line: ref.line
						});
					}
				});

			// Find external data areas
			scope.structs
				.filter((struct: any) => struct.keyword[`DTAARA`])
				.map((ref): RpgLookup => {
					const keyword = ref.keyword;
					let fileName: string = ref.name;
					const dtaara = keyword[`DTAARA`];
					if (dtaara) {
						if (dtaara === true) fileName = ref.name;
						else fileName = trimQuotes(dtaara);
					}

					return {
						lookup: fileName.toUpperCase(),
						line: ref.position ? ref.position.line : undefined
					};
				})
				.forEach((ref: RpgLookup) => {
					if (ignoredObjects.includes(ref.lookup.toUpperCase())) return;

					const resolvedObject = this.searchForObject({ systemName: ref.lookup, type: `DTAARA` });
					if (resolvedObject) target.deps.push(resolvedObject)
					else {
						this.logger.fileLog(ileObject.relativePath, {
							message: `No object found for reference '${ref.lookup}'`,
							type: `warning`,
							line: ref.line
						});
					}
				});

			scope.variables
				.filter((struct: any) => struct.keyword[`DTAARA`])
				.map((ref): RpgLookup => {
					const keyword = ref.keyword;
					let fileName: string = ref.name;
					const dtaara = keyword[`DTAARA`];
					if (dtaara) {
						if (dtaara === true) fileName = ref.name;
						else fileName = trimQuotes(dtaara);
					}

					return {
						lookup: fileName.toUpperCase(),
						line: ref.position ? ref.position.line : undefined
					};
				})
				.forEach((ref: RpgLookup) => {
					const resolvedObject = this.searchForObject({ systemName: ref.lookup, type: `DTAARA` });
					if (resolvedObject) target.deps.push(resolvedObject)
					else {
						this.logger.fileLog(ileObject.relativePath, {
							message: `No object found for reference '${ref.lookup}'`,
							type: `warning`,
							line: ref.line
						});
					}
				});
		}

		// TODO: did we duplicate this?
		// We also look to see if there is a `.cmd` object with the same name
		const resolvedObject = this.searchForObject({ systemName: ileObject.systemName, type: `CMD` });
		if (resolvedObject) this.createOrAppend(resolvedObject, target);

		if (target.deps.length > 0)
			infoOut(`Depends on: ${target.deps.map(d => `${d.systemName}.${d.type}`).join(` `)}`);

		this.addNewTarget(target);
	}

	getTarget(object: ILEObject): ILEObjectTarget | undefined {
		return this.targets[`${object.systemName}.${object.type}`];
	}

	getTargets(): ILEObjectTarget[] {
		return Object.values(this.targets).filter(x => x);
	}

	// Generates targets for service programs and binding directories
	public resolveBinder() {
		// Right now, we really only support single module programs and service programs

		const allTargets = this.getTargets();

		// We can simply check for any modules since we turn them into service programs
		this.needsBinder = allTargets.some(d => d.type === `SRVPGM`);

		infoOut(``);

		// We need to loop through all the user-defined server programs (binder source)
		// And resolve the service program program exports to module exports to bind them together nicely
		const allSrvPgms = this.getTargetsOfType(`SRVPGM`);
		const allModules = this.getTargetsOfType(`MODULE`);

		for (const target of allSrvPgms) {
			if (target.exports) {
				infoOut(`Resolving modules for ${target.systemName}.${target.type}`);

				target.deps = [];

				for (const exportName of target.exports) {
					// We loop through each export of the service program and find the module that exports it
					const foundModule = allModules.find(mod => mod.exports && mod.exports.includes(exportName));
					if (foundModule) {
						const alreadyBound = target.deps.some(dep => dep.systemName === foundModule.systemName && dep.type === `MODULE`);
						if (!alreadyBound) {
							infoOut(`Adding module ${foundModule.systemName}.${foundModule.type}`);
							target.deps.push(foundModule);
						}
					}
				}

				if (target.deps.length > 0) {
					// Add this new service program to the project binding directory
					this.createOrAppend(bindingDirectoryTarget, target);

					// Make sure we can resolve to this service program
					target.exports.forEach(e => {
						this.resolvedExports[e.toUpperCase()] = target;
					});
				} else {
					// This service program target doesn't have any deps... so, it's not used?
					this.removeObject(target);

					if (target.relativePath) {
						this.logger.fileLog(target.relativePath, {
							message: `Removed as target because no modules were found with matching exports.`,
							type: `info`
						});
					}
				}

				infoOut(``);
			}
		}

		// We loop through all programs and module and study their imports.
		// We do this in case they depend on another service programs based on import
		for (let currentTarget of allTargets) {
			if ([`PGM`, `MODULE`].includes(currentTarget.type) && currentTarget.imports) {
				let newImports: ILEObject[] = [];

				// Remove any service program deps so we can resolve them cleanly
				currentTarget.deps = currentTarget.deps.filter(d => ![`SRVPGM`].includes(d.type));

				currentTarget.imports.forEach(importName => {
					// Find if this import resolves to another object
					const possibleSrvPgmDep = this.resolvedExports[importName.toUpperCase()];
					// We can't add a module as a dependency at this step.
					if (possibleSrvPgmDep && possibleSrvPgmDep.type === `SRVPGM`) {
						// Make sure we haven't imported it before!
						if (!newImports.some(i => i.systemName === possibleSrvPgmDep.systemName && i.type === possibleSrvPgmDep.type)) {
							newImports.push(possibleSrvPgmDep);
						}

					} else if ([`PGM`, `MODULE`].includes(currentTarget.type)) {
						// Perhaps we're looking at a program object, which actually should be a multi
						// module program, so we do a lookup for additional modules.
						const possibleModuleDep = allModules.find(mod => mod.exports && mod.exports.includes(importName.toUpperCase()))
						if (possibleModuleDep) {
							if (!newImports.some(i => i.systemName === possibleModuleDep.systemName && i.type === possibleModuleDep.type)) {
								newImports.push(possibleModuleDep);

								// TODO: consider other IMPORTS that `possibleModuleDep` needs.
							}
						}
					}
				});

				// If the program or module has imports that we ca resolve, then we add them as deps
				if (newImports.length > 0) {
					infoOut(`${currentTarget.systemName}.${currentTarget.type} has additional dependencies: ${newImports.map(i => `${i.systemName}.${i.type}`)}`);
					currentTarget.deps.push(...newImports);

					if (currentTarget.type === `PGM`) {
						// If this program has MODULE dependecies, that means we need to change the way it's compiled
						// to be a program made up of many modules, usually done with CRTPGM
						if (currentTarget.deps.some(d => d.type === `MODULE`)) {
							this.convertBoundProgramToMultiModuleProgram(currentTarget);

							// Then, also include any of the modules dep modules into the currentTarget deps!!
							const depTargets = currentTarget.deps
								.filter(d => d.type === `MODULE`)
								.map(m => this.getTarget(m));

							// Confusing names, it means: dependencies of the dependencies that are modules
							const depDeps = depTargets .map(m => m?.deps).flat().filter(d => d.type === `MODULE`);

							for (const newDep of depDeps) {
								if (newDep && !currentTarget.deps.some(d => d.systemName === newDep.systemName && d.type === newDep.type)) {
									currentTarget.deps.push(newDep);
								}
							}
						}
					}
				}
			}
		}

		const commandObjects = this.getResolvedObjects(`CMD`);
		for (let cmdObject of commandObjects) {
			// Check if a program exists with the same name.
			const programObject = this.getTarget({ systemName: cmdObject.systemName, type: `PGM` });
			if (programObject) {
				const newTarget = {
					...cmdObject,
					deps: [programObject]
				}

				this.addNewTarget(newTarget);
			} else {

				this.removeObject(cmdObject);
				this.logger.fileLog(cmdObject.relativePath, {
					message: `Removed as target because no program was found with a matching name.`,
					type: `info`
				});
			}
		}
	}

	private convertBoundProgramToMultiModuleProgram(currentTarget: ILEObjectTarget) {
		const basePath = currentTarget.relativePath;

		// First, let's change this current target to be solely a program
		// Change the extension so it's picked up correctly during the build process.
		currentTarget.extension = `pgm`;
		currentTarget.relativePath = undefined;

		// Store a fake path for this program object
		this.storeResolved(path.join(this.cwd, `${currentTarget.systemName}.PGM`), currentTarget);

		// Then we can create the new module object from this path
		const newModule: ILEObject = {
			systemName: currentTarget.systemName,
			imports: currentTarget.imports,
			exports: [],
			type: `MODULE`,
			relativePath: basePath,
			extension: path.extname(basePath).substring(1)
		};

		// Replace the old resolved object with the module
		this.storeResolved(path.join(this.cwd, basePath), newModule);

		// Create a new target for the module
		const newModTarget = this.createOrAppend(newModule);

		// Clean up imports for module and program
		newModTarget.imports = currentTarget.imports;
		currentTarget.imports = undefined;

		this.createOrAppend(currentTarget, newModule);
	}

	public createOrAppend(parentObject: ILEObject, newDep?: ILEObject) {
		let existingTarget = this.targets[`${parentObject.systemName}.${parentObject.type}`];

		if (!existingTarget) {
			existingTarget = {
				...parentObject,
				deps: []
			};

			this.addNewTarget(existingTarget);
		}

		if (newDep)
			existingTarget.deps.push(newDep);

		return existingTarget;
	}

	private addNewTarget(dep: ILEObjectTarget) {
		this.targets[`${dep.systemName}.${dep.type}`] = dep;
	}

	public binderRequired() {
		return this.needsBinder;
	}

	public getTargetsOfType(type: ObjectType): ILEObjectTarget[] {
		return this.getTargets().filter(d => d && d.type === type);
	}

	public getResolvedObjects(type?: ObjectType): ILEObject[] {
		const objects = Object.values(this.resolvedObjects);

		return objects.filter(o => o && (type === undefined || o.type === type));
	}

	public getResolvedObject(fullPath: string): ILEObject {
		return this.resolvedObjects[fullPath];
	}

	/**
	 * This API is a little trick.
	 * You can pass in a valid file extension, or if you pass
	 * solely just `pgm`, it will return all programs that
	 * have multiple modules.
	 */
	public getResolvedObjectsByFileExtension(ext: string): ILEObject[] {
		const extensionParts = ext.split(`.`);
		let extension = ext.toUpperCase(), shouldBeProgram = false, anyPrograms = false;

		if (extensionParts.length === 2 && extensionParts[0].toUpperCase() === `PGM`) {
			extension = extensionParts[1].toUpperCase();
			shouldBeProgram = true;
		} else if (extension === `PGM`) {
			anyPrograms = true;
		}

		return Object.values(this.resolvedObjects).filter(obj =>
			(obj.extension?.toUpperCase() === extension && (obj.type === `PGM`) === shouldBeProgram) ||
			(anyPrograms === true && obj.type === `PGM` && obj.extension.toUpperCase() === extension)
		);
	}

	public getExports() {
		return this.resolvedExports;
	}

	public getImpactFor(theObject: ILEObject) {
		const allDeps = this.getTargets();
		let currentTree: ILEObject[] = [];

		let currentItem: ImpactedObject = { ileObject: theObject, children: [] };

		function lookupObject(currentItem: ImpactedObject) {
			currentTree.push(currentItem.ileObject);

			for (const target of allDeps) {
				const containsLookup = target.deps.some(d => d.systemName === currentItem.ileObject.systemName && d.type === currentItem.ileObject.type);
				const circular = currentTree.some(d => d.systemName === target.systemName && d.type === target.type);

				if (containsLookup && !circular) {
					let newDependant: ImpactedObject = { ileObject: target, children: [] };
					lookupObject(newDependant);
					currentItem.children.push(newDependant);
				}
			}

			currentTree.pop();
		}

		lookupObject(currentItem);

		return currentItem;
	}

}

function trimQuotes(input: string, value = `'`) {
	if (input[0] === value) input = input.substring(1);
	if (input[input.length - 1] === value) input = input.substring(0, input.length - 1);
	return input;
}
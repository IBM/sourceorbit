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

export type ObjectType = "PGM" | "SRVPGM" | "MODULE" | "FILE" | "BNDDIR" | "DTAARA" | "CMD" | "MENU" | "DTAQ";

const ignoredObjects = [`QSYSPRT`, `QCMDEXC.PGM`, `*LDA.DTAARA`, `QDCXLATE.PGM`, `QUSRJOBI`, `QTQCVRT.PGM`];

const bindingDirectoryTarget: ILEObject = { name: `$(APP_BNDDIR)`, type: `BNDDIR` };

export interface ILEObject {
	name: string;
	type: ObjectType;
	relativePath?: string;
	extension?: string;

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

export class Targets {
	private rpgParser: Parser;

	private pathCache: { [path: string]: true|string[] } | undefined;
	private resolvedPaths: { [query: string]: string } = {};
	private resolvedObjects: { [localPath: string]: ILEObject } = {};
	private resolvedExports: { [name: string]: ILEObject } = {};
	private deps: { [name: string]: ILEObjectTarget } = {};
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
		const detail = path.parse(localPath);
		this.resolvedObjects[localPath] = ileObject;

		if (this.pathCache) {
			this.pathCache[localPath] = true;
			if (Array.isArray(this.pathCache[detail.dir])) {
				const paths = this.pathCache[detail.dir] as string[];
				const parentIndex = paths.findIndex(p => p === detail.base);
				if (parentIndex === -1) {
					paths.push(detail.base)
				}
			}
		}
	}

	public resolveObject(localPath: string) {
		if (this.resolvedObjects[localPath]) return this.resolvedObjects[localPath];

		const detail = path.parse(localPath);
		const relativePath = this.getRelative(localPath);

		const isProgram = detail.name.toUpperCase().endsWith(`.PGM`);
		const name = isProgram ? detail.name.substring(0, detail.name.length - 4) : detail.name;
		const extension = detail.ext.length > 1 ? detail.ext.substring(1) : detail.ext;
		const type: ObjectType = (isProgram ? "PGM" : this.getObjectType(relativePath, extension));

		const theObject: ILEObject = {
			name: name.toUpperCase(),
			type: type,
			relativePath,
			extension
		};

		this.storeResolved(localPath, theObject);

		return theObject;
	}

	public removeObjectByPath(localPath: string) {
		const resolvedObject = this.resolvedObjects[localPath];
		const pathDetail = path.parse(localPath);

		if (resolvedObject) {
			// First, delete the simple caches
			this.resolvedObjects[localPath] = undefined;

			if (this.pathCache) {
				this.pathCache[localPath] = undefined;
				if (Array.isArray(this.pathCache[pathDetail.dir])) {
					const paths = this.pathCache[pathDetail.dir] as string[];
					const parentIndex = paths.findIndex(p => p === pathDetail.base);
					if (parentIndex >= 0) {
						paths.splice(parentIndex, 1);
					}
				}
			}

			return this.removeObject(resolvedObject);
		}

		return []
	}

	public removeObject(resolvedObject: ILEObject) {
		let impactedTargets: ILEObject[] = [];

		for (const targetId in this.deps) {
			const target = this.deps[targetId];

			if (target) {
				const depIndex = target.deps.findIndex(d => (d.name === resolvedObject.name && d.type === resolvedObject.type) || d.relativePath === resolvedObject.relativePath);

				if (depIndex >= 0) {
					impactedTargets.push(target);
					target.deps.splice(depIndex, 1);

					if (target.relativePath) {
						this.logger.fileLog(target.relativePath, {
							type: `info`,
							message: `This object depended on ${resolvedObject.name}.${resolvedObject.type} before it was deleted.`
						})
					}
				}
			}
		}

		// Remove it as a global target
		this.deps[`${resolvedObject.name}.${resolvedObject.type}`] = undefined;
		this.resolvedPaths[`${resolvedObject.name}.${resolvedObject.type}`] = undefined;

		// Remove possible logs
		if (resolvedObject.relativePath) {
			this.logger.flush(resolvedObject.relativePath)
		}

		return impactedTargets;
	}

	/**
	 * Resolves a search to a filename. Basically a special blob
	 */
	public resolveLocalObjectQuery(name: string, baseName?: string): string {
		name = name.toUpperCase();

		if (this.resolvedPaths[name]) return this.resolvedPaths[name];

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
			ignore: baseName ? `**/${baseName}` : undefined,
			cache: this.pathCache
		});

		this.resolvedPaths[name] = results[0];

		return results[0];
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

	public async handlePath(filePath: string) {
		const pathDetail = path.parse(filePath);
		const relative = this.getRelative(filePath);

		let success = true;

		if (pathDetail.ext.length > 1) {
			if (!this.suggestions.renames) {
				// Don't clear the logs if we're suggestion renames.
				this.logger.flush();
			}

			const ext = pathDetail.ext.substring(1).toLowerCase();

			try {
				const content = await fs.readFile(filePath, { encoding: `utf-8` });
				const eol = content.indexOf(`\r\n`) >= 0 ? `\r\n` : `\n`;
				const isFree = (content.length >= 6 ? content.substring(0, 6).toLowerCase() === `**free` : false);

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
						this.createRpgTarget(filePath, rpgDocs, isFree);
					}

				}
				else if (clExtensions.includes(ext)) {
					const clDocs = new CLParser();
					const tokens = clDocs.parseDocument(content);

					const module = new Module();
					module.parseStatements(tokens);

					this.createClTarget(filePath, module);
				}
				else if (ddsExtension.includes(ext)) {
					const ddsFile = new dds();
					ddsFile.parse(content.split(eol));

					this.createDdsFileTarget(filePath, ddsFile);
				}
				else if (sqlExtensions.includes(ext)) {
					const sqlDoc = new Document(content);
					this.createSqlTargets(filePath, sqlDoc);
				}
				else if (srvPgmExtensions.includes(ext)) {
					const clDocs = new CLParser();
					const tokens = clDocs.parseDocument(content);

					const module = new Module();
					module.parseStatements(tokens);

					this.createSrvPgmTarget(filePath, module);
				}
				else if (cmdExtensions.includes(ext)) {
					this.createCmdTarget(filePath);
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

	private createCmdTarget(localPath) {
		this.resolveObject(localPath);

		// Since cmd source doesn't explicity contains deps, we resolve later on
	}

	private createSrvPgmTarget(localPath: string, module: Module) {
		const ileObject = this.resolveObject(localPath);
		const target: ILEObjectTarget = {
			...ileObject,
			deps: [],
			exports: []
		};

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

		this.pushDep(target);
	}

	/**
	 * Handles all DDS types: pf, lf, dspf
	 */
	private createDdsFileTarget(localPath: string, dds: dds) {
		const sourceName = path.basename(localPath);
		const ileObject = this.resolveObject(localPath);
		const target: ILEObjectTarget = {
			...ileObject,
			deps: []
		};

		infoOut(`${ileObject.name}.${ileObject.type}: ${ileObject.relativePath}`);

		const ddsRefKeywords = [`PFILE`, `REF`, `JFILE`];

		for (const recordFormat of dds.formats) {

			for (const keyword of ddsRefKeywords) {
				const keywordObj = recordFormat.keywords.find(k => k.name === keyword);
				if (keywordObj) {
					const wholeValue: string = keywordObj.value;
					const parts = wholeValue.split(` `).filter(x => x.length > 0);
					for (const value of parts) {
						const qualified = value.split(`/`);

						let objectName;
						if (qualified.length === 2 && qualified[0].toLowerCase() === `*libl`) {
							objectName = qualified[1];
						} else if (qualified.length === 1) {
							objectName = qualified[0];
						}

						if (objectName) {
							const resolvedPath = this.resolveLocalObjectQuery(objectName, sourceName);
							if (resolvedPath) target.deps.push(this.resolveObject(resolvedPath))
							else {
								this.logger.fileLog(ileObject.relativePath, {
									message: `no object found for reference '${objectName}'`,
									type: `warning`,
									line: recordFormat.range.start
								});
							}
						} else {
							this.logger.fileLog(ileObject.relativePath, {
								message: `${keyword} reference not included as possible reference to library found.`,
								type: `info`,
								line: recordFormat.range.start
							});
						}
					}
				}
			}
		}

		if (target.deps.length > 0)
			infoOut(`Depends on: ${target.deps.map(d => `${d.name}.${d.type}`).join(` `)}`);

		this.pushDep(target);
	}

	private createClTarget(localPath: string, module: Module) {
		const pathDetail = path.parse(localPath);
		const sourceName = pathDetail.base;
		const ileObject = this.resolveObject(localPath);
		const target: ILEObjectTarget = {
			...ileObject,
			deps: []
		};

		infoOut(`${ileObject.name}.${ileObject.type}: ${ileObject.relativePath}`);

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

				const resolvedPath = this.resolveLocalObjectQuery(possibleObject.name, sourceName);
				if (resolvedPath) target.deps.push(this.resolveObject(resolvedPath));
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

					const resolvedPath = this.resolveLocalObjectQuery(name + `.pgm`, sourceName);
					if (resolvedPath) target.deps.push(this.resolveObject(resolvedPath))
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
		const possibleCommandPath = this.resolveLocalObjectQuery(`${ileObject.name}.cmd`, sourceName);
		if (possibleCommandPath) {
			const resolvedObject = this.resolveObject(possibleCommandPath);
			if (resolvedObject) this.createOrAppend(resolvedObject, target);
		}

		if (target.deps.length > 0)
			infoOut(`Depends on: ${target.deps.map(d => `${d.name}.${d.type}`).join(` `)}`);

		this.pushDep(target);
	}

	private createSqlTargets(localPath: string, document: Document) {
		const pathDetail = path.parse(localPath);
		const relativePath = this.getRelative(localPath);

		const groups = document.getStatementGroups();

		// TODO: Note, this returns high level definitions.
		// If the index/view/etc specifies a table dep,
		// they will not appear as a dependency

		const createCount = groups.filter(g => g.statements[0].type === StatementType.Create).length;

		if (createCount > 1) {
			this.logger.fileLog(relativePath, {
				message: `includes multiple create statements. They should be in individual sources. This file will not be parsed.`,
				type: `warning`,
			});

			return;
		}

		for (const group of groups) {
			const statement = group.statements[0];
			const defs = statement.getObjectReferences();
			const mainDef = defs[0];

			if (mainDef && mainDef.type && mainDef.object.name) {
				const tokens = mainDef.tokens;
				if (mainDef.object.schema) {
					this.logger.fileLog(relativePath, {
						message: `${mainDef.object.schema}/${mainDef.object.name} (${mainDef.type}) reference not included as possible reference to library found.`,
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
								message: `${mainDef.object.name} (${mainDef.type}) alter not tracked due to possible circular dependency.`,
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
							let objectName = trimQuotes(mainDef.object.name, `"`);

							let ileObject = {
								name: objectName,
								type: this.getObjectType(relativePath, mainDef.type),
								relativePath,
							}

							// TODO: better support for 'for system name' in SQL

							const fileObjName = pathDetail.name.toUpperCase();

							if (ileObject.name.length > 10) {
								this.logger.fileLog(ileObject.relativePath, {
									message: `${ileObject.name} (${ileObject.type}) name is longer than 10 characters. Assuming system name (object name) is '${fileObjName}'`,
									type: `warning`,
									range: {
										start: tokens[0].range.start,
										end: tokens[tokens.length - 1].range.end
									},
								});

								ileObject.name = fileObjName;
							}
							else if (fileObjName.length <= 10 && ileObject.name !== fileObjName) {
								// We must do this because we resolve objects by file name!
								this.logger.fileLog(ileObject.relativePath, {
									message: `${ileObject.name} does not match file basename. Assuming system name (object name) is '${fileObjName}'`,
									type: `warning`,
									range: {
										start: tokens[0].range.start,
										end: tokens[tokens.length - 1].range.end
									},
								});

								ileObject.name = fileObjName;
							}

							let newTarget: ILEObjectTarget = {
								...ileObject,
								deps: []
							};

							infoOut(`${newTarget.name}.${newTarget.type}: ${newTarget.relativePath}`);

							if (defs.length > 1) {
								for (const def of defs.slice(1)) {
									const refTokens = def.tokens;
									const simpleName = trimQuotes(def.object.name, `"`);
									const resolvedPath = this.resolveLocalObjectQuery(simpleName + `.*`, pathDetail.base);
									if (resolvedPath) newTarget.deps.push(this.resolveObject(resolvedPath))
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
							}

							if (newTarget.deps.length > 0) {
								infoOut(`Depends on: ${newTarget.deps.map(d => `${d.name}.${d.type}`).join(` `)}`);
							}

							// So we can later resolve the path to the created object
							this.storeResolved(localPath, ileObject);

							this.pushDep(newTarget);
							break;
					}

				}
			}
		}
	}

	private createRpgTarget(localPath: string, cache: Cache, isFree = false) {
		const pathDetail = path.parse(localPath);
		const sourceName = pathDetail.base;
		const ileObject = this.resolveObject(localPath);
		const target: ILEObjectTarget = {
			...ileObject,
			deps: []
		};

		infoOut(`${ileObject.name}.${ileObject.type}: ${ileObject.relativePath}`);

		cache.includes.forEach((include: IncludeStatement) => {
			const includeDetail = path.parse(include.toPath);

			if (includeDetail.ext !== `.rpgleinc`) {
				const possibleName = includeDetail.name.toLowerCase().endsWith(`.pgm`) ? includeDetail.name.substring(0, includeDetail.name.length - 4) : includeDetail.name;

				if (this.suggestions.renames) {
					this.logger.fileLog(this.getRelative(include.toPath), {
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
						lineContent: (isFree ? `` : ``.padEnd(6)) + `/copy '${this.getRelative(include.toPath)}'`
					}
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
				type: `warning`,
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
					lookup: fileName + `.pgm`,
					line: ref.position ? ref.position.line : undefined
				};
			})
			.forEach((ref: RpgLookup) => {
				if (ignoredObjects.includes(ref.lookup.toUpperCase())) return;
				if (path.basename(ref.lookup, `.pgm`).toUpperCase() === ileObject.name) return;

				const resolvedPath = this.resolveLocalObjectQuery(ref.lookup, sourceName);
				if (resolvedPath) {
					const resolvedObject = this.resolveObject(resolvedPath);

					// because of legacy fixed CALL, there can be dupliicate EXTPGMs with the same name :(
					if (!target.deps.some(d => d.name === resolvedObject.name && d.type && resolvedObject.type)) {
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

		// Find external data structure sources
		cache.structs
			.filter((struct: any) => struct.keyword[`EXTNAME`])
			.map((struct): RpgLookup => {
				const keyword = struct.keyword;
				const value = trimQuotes(keyword[`EXTNAME`]);

				return {
					lookup: value.split(`:`)[0].toLowerCase(),
					line: struct.position ? struct.position.line : undefined
				};
			})
			.forEach((ref: RpgLookup) => {
				const resolvedPath = this.resolveLocalObjectQuery(ref.lookup, sourceName);
				if (resolvedPath) target.deps.push(this.resolveObject(resolvedPath))
				else {
					this.logger.fileLog(ileObject.relativePath, {
						message: `No object found for reference '${ref.lookup}'`,
						type: `warning`,
						line: ref.line
					});
				}
			});



		// Find external files
		cache.files
			.map((file): RpgLookup => {
				let possibleName = file.name;
				const keyword = file.keyword;

				const extNameValue = keyword[`EXTFILE`];
				if (extNameValue) {
					possibleName = trimQuotes(extNameValue).split(`:`)[0].toLowerCase()
				}

				if (possibleName === `*extdesc`) {
					const extDescValue = keyword[`EXTDESC`];
					if (extDescValue) {
						possibleName = trimQuotes(extDescValue);
					} else {
						this.logger.fileLog(ileObject.relativePath, {
							message: `*EXTDESC is used for '${file.name}' but EXTDESC keyword not found/`,
							type: `warning`,
						});
					}
				}

				return {
					lookup: possibleName,
					line: file.position ? file.position.line : undefined
				};
			})
			.forEach((ref: RpgLookup) => {
				if (ignoredObjects.includes(ref.lookup.toUpperCase())) return;

				const resolvedPath = this.resolveLocalObjectQuery(ref.lookup, sourceName);
				if (resolvedPath) target.deps.push(this.resolveObject(resolvedPath))
				else {
					this.logger.fileLog(ileObject.relativePath, {
						message: `No object found for reference '${ref.lookup}'`,
						type: `warning`,
						line: ref.line
					});
				}
			})

		// We ignore anything with hardcoded schemas
		cache.sqlReferences
			.filter(ref => !ref.description)
			.map((ref): RpgLookup => ({
				lookup: trimQuotes(ref.name, `"`),
				line: ref.position ? ref.position.line : undefined
			}))
			.forEach((ref: RpgLookup) => {
				const resolvedPath = this.resolveLocalObjectQuery(ref.lookup, sourceName);
				if (resolvedPath) target.deps.push(this.resolveObject(resolvedPath))
				else {
					this.logger.fileLog(ileObject.relativePath, {
						message: `No object found for reference '${ref.lookup}'`,
						type: `warning`,
						line: ref.line
					});
				}
			});

		// Find external data areas
		cache.structs
			.filter((struct: any) => struct.keyword[`DTAARA`])
			.map((ref): RpgLookup => {
				const keyword = ref.keyword;
				let fileName = ref.name;
				const dtaara = keyword[`DTAARA`];
				if (dtaara) {
					if (dtaara === true) fileName = ref.name;
					else fileName = trimQuotes(dtaara);
				}

				return {
					lookup: fileName + `.dtaara`,
					line: ref.position ? ref.position.line : undefined
				};
			})
			.forEach((ref: RpgLookup) => {
				if (ignoredObjects.includes(ref.lookup.toUpperCase())) return;

				const resolvedPath = this.resolveLocalObjectQuery(ref.lookup, sourceName);
				if (resolvedPath) target.deps.push(this.resolveObject(resolvedPath))
				else {
					this.logger.fileLog(ileObject.relativePath, {
						message: `No object found for reference '${ref.lookup}'`,
						type: `warning`,
						line: ref.line
					});
				}
			});

		cache.variables
			.filter((struct: any) => struct.keyword[`DTAARA`])
			.map((ref): RpgLookup => {
				const keyword = ref.keyword;
				let fileName = ref.name;
				const dtaara = keyword[`DTAARA`];
				if (dtaara) {
					if (dtaara === true) fileName = ref.name;
					else fileName = trimQuotes(dtaara);
				}

				return {
					lookup: fileName + `.dtaara`,
					line: ref.position ? ref.position.line : undefined
				};
			})
			.forEach((ref: RpgLookup) => {
				const resolvedPath = this.resolveLocalObjectQuery(ref.lookup, sourceName);
				if (resolvedPath) target.deps.push(this.resolveObject(resolvedPath))
				else {
					this.logger.fileLog(ileObject.relativePath, {
						message: `No object found for reference '${ref.lookup}'`,
						type: `warning`,
						line: ref.line
					});
				}
			});

		// TODO: did we duplicate this?
		// We also look to see if there is a `.cmd` object with the same name
		const possibleCommandPath = this.resolveLocalObjectQuery(`${ileObject.name}.cmd`, sourceName);
		if (possibleCommandPath) {
			const resolvedObject = this.resolveObject(possibleCommandPath);
			if (resolvedObject) this.createOrAppend(resolvedObject, target);
		}

		// define internal imports
		target.imports = cache.procedures
			.filter((proc: any) => proc.keyword[`EXTPROC`])
			.map(ref => {
				const keyword = ref.keyword;
				let importName: string = ref.name;
				const extproc = keyword[`EXTPROC`];
				if (extproc) {
					if (extproc === true) importName = ref.name;
					else importName = trimQuotes(extproc);
				}

				return importName;
			});

		// define exported functions
		if (cache.keyword[`NOMAIN`]) {
			target.exports = cache.procedures
				.filter((proc: any) => proc.keyword[`EXPORT`])
				.map(ref => ref.name.toUpperCase());
		}

		if (target.deps.length > 0)
			infoOut(`Depends on: ${target.deps.map(d => `${d.name}.${d.type}`).join(` `)}`);

		this.pushDep(target);
	}

	getDep(object: ILEObject): ILEObjectTarget | undefined {
		return this.deps[`${object.name}.${object.type}`];
	}

	getDeps(): ILEObjectTarget[] {
		return Object.values(this.deps).filter(x => x);
	}

	// Generates targets for service programs and binding directories
	public resolveBinder() {
		// Right now, we really only support single module programs and service programs

		const deps = this.getDeps();

		// We can simply check for any modules since we turn them into service programs
		this.needsBinder = deps.some(d => d.type === `MODULE`);

		infoOut(``);

		// We need to loop through all the user-defined server programs (binder source)
		// And resolve the service program program exports to module exports to bind them together nicely
		const allModules = this.getParentObjects("MODULE");

		for (const target of deps) {
			if (target.type === `SRVPGM` && target.exports) {
				infoOut(`Resolving modules for ${target.name}.${target.type}`);

				target.deps = [];

				for (const exportName of target.exports) {
					const foundModule = allModules.find(mod => mod.exports && mod.exports.includes(exportName));
					if (foundModule) {
						const alreadyBound = target.deps.some(dep => dep.name === foundModule.name && dep.type === `MODULE`);
						if (!alreadyBound) {
							infoOut(`Adding module ${foundModule.name}.${foundModule.type}`);
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
					// This target doesn't have any deps... so, it's not used?
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

		// Next, we loop through all the modules we know of and if that module
		// is not a dependency on any service program, then we assume it's a
		// service program object with EXPORT(*ALL)
		const allServicePrograms = this.getParentObjects("SRVPGM");
		for (const module of allModules) {
			const isBoundSomewhere = allServicePrograms.some(srvpgm => srvpgm.deps.some(dep => dep.name === module.name && dep.type === `MODULE`));
			if (!isBoundSomewhere) {
				infoOut(`Assuming ${module.name}.${module.type} is a service program (SRVPGM)`);

				const newServiceProgramTarget: ILEObject = {
					...module,
					type: `SRVPGM`,
					relativePath: undefined,
					extension: undefined,
					exports: module.exports
				};

				// This creates the service program target if it does not exist.
				const serviceProgramTarget = this.createOrAppend(newServiceProgramTarget, module);

				// Add this new service program to the project binding directory
				this.createOrAppend(bindingDirectoryTarget, serviceProgramTarget);

				// Resolve the exports to this new service program
				if (serviceProgramTarget.exports) {
					serviceProgramTarget.exports.forEach(e => {
						this.resolvedExports[e.toUpperCase()] = serviceProgramTarget;
					});
				}

				infoOut(``);
			}
		}

		// We loop through all programs and service programs and study their imports.
		// We do this in case they depend on another service programs based on import
		for (let target of deps) {
			if ([`PGM`, `MODULE`].includes(target.type) && target.imports) {
				let newImports: ILEObject[] = [];

				// Remove any service program deps so we can resolve them cleanly
				target.deps = target.deps.filter(d => ![`SRVPGM`].includes(d.type));

				target.imports.forEach(importName => {
					// Find if this import resolves to another object
					const possibleDep = this.resolvedExports[importName.toUpperCase()];
					// We can't add a module as a dependency at this step.
					if (possibleDep && possibleDep.type === `SRVPGM`) {
						// Make sure we haven't imported it before!
						if (!newImports.some(i => i.name === possibleDep.name && i.type === possibleDep.type)) {
							newImports.push(possibleDep);
						}
					}
				});

				if (newImports.length > 0) {
					infoOut(`${target.name}.${target.type} has additional dependencies: ${newImports.map(i => `${i.name}.${i.type}`)}`);
					target.deps.push(...newImports);
				}
			}
		}

		const commandObjects = this.getResolvedObjects(`CMD`);

		for (let cmdObject of commandObjects) {
			// Check if a program exists with the same name.
			const programObject = this.getDep({ name: cmdObject.name, type: `PGM` });
			if (programObject) {
				const newTarget = {
					...cmdObject,
					deps: [programObject]
				}

				this.pushDep(newTarget);
			} else {

				this.removeObject(cmdObject);
				this.logger.fileLog(cmdObject.relativePath, {
					message: `Removed as target because no program was found with a matching name.`,
					type: `info`
				});
			}
		}
	}

	public createOrAppend(parentObject: ILEObject, newDep?: ILEObject) {
		let existingTarget = this.deps[`${parentObject.name}.${parentObject.type}`];

		if (!existingTarget) {
			existingTarget = {
				...parentObject,
				deps: []
			};

			this.pushDep(existingTarget);
		}

		if (newDep)
			existingTarget.deps.push(newDep);

		return existingTarget;
	}

	private pushDep(dep: ILEObjectTarget) {
		this.deps[`${dep.name}.${dep.type}`] = dep;
	}

	public binderRequired() {
		return this.needsBinder;
	}

	public getParentObjects(type: ObjectType): ILEObjectTarget[] {
		return this.getDeps().filter(d => d && d.type === type);
	}

	public getResolvedObjects(type?: ObjectType): ILEObject[] {
		const objects = Object.values(this.resolvedObjects);

		return objects.filter(o => o && (type === undefined || o.type === type));
	}

	public getResolvedObject(fullPath: string): ILEObject {
		return this.resolvedObjects[fullPath];
	}

	public getObjectsByExtension(ext: string): ILEObject[] {
		const upperExt = ext.toUpperCase();
		return Object.
			keys(this.resolvedObjects).
			filter(filePath => {
				const basename = path.basename(filePath);
				const dotIndex = basename.indexOf(`.`);
				if (dotIndex >= 0) {
					const lastPart = basename.substring(dotIndex + 1);
					return (lastPart.toUpperCase() === upperExt);
				}
			}).
			map(filePath => this.resolvedObjects[filePath]);
	}

	public getExports() {
		return this.resolvedExports;
	}

	public getImpactFor(theObject: ILEObject) {
		let lines: string[] = [];

		const allDeps = this.getDeps();
		let currentTree: ILEObject[] = [];

		let currentItem: ImpactedObject = { ileObject: theObject, children: [] };

		function lookupObject(currentItem: ImpactedObject) {
			currentTree.push(currentItem.ileObject);

			for (const target of allDeps) {
				const containsLookup = target.deps.some(d => d.name === currentItem.ileObject.name && d.type === currentItem.ileObject.type);
				const circurlar = currentTree.some(d => d.name === target.name && d.type === target.type);

				if (containsLookup && !circurlar) {
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
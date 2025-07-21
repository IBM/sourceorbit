import path from 'path';
import { infoOut } from '../cli';
import Document from "vscode-db2i/src/language/sql/document";
import { ObjectRef, StatementType } from 'vscode-db2i/src/language/sql/types';
import { Logger } from '../logger';
import { getReferenceObjectsFrom, getSystemNameFromPath, globalEntryIsValid, toLocalPath } from '../utils';
import { ReadFileSystem } from '../readFileSystem';
import { TargetsLanguageProvider } from './languages';
import { sqlExtensions } from './languages/sql';

export type ObjectType = "PGM" | "SRVPGM" | "MODULE" | "FILE" | "BNDDIR" | "DTAARA" | "CMD" | "MENU" | "DTAQ";

const ignoredObjects = [`QSYSPRT`, `QCMDEXC`, `*LDA.DTAARA`, `QDCXLATE`, `QUSRJOBI`, `QTQCVRT`, `QWCRDTAA`, `QUSROBJD`, `QUSRMBRD`, `QUSROBJD`, `QUSLOBJ`, `QUSRTVUS`, `QUSCRTUS`];

const DEFAULT_BINDER_TARGET: ILEObject = { systemName: `$(APP_BNDDIR)`, type: `BNDDIR` };

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

	/** headers. only supports RPGLE and is not recursive */
	headers?: string[];
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

export interface FileOptions {
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
	static LanguageProvider: TargetsLanguageProvider = new TargetsLanguageProvider();

	/* pathCache and resolvedSearches are used for file resolving. */
	private pathCache: { [path: string]: true | string[] } | undefined;
	private resolvedSearches: { [query: string]: string } = {};

	private assumePrograms = false;

	private resolvedObjects: { [localPath: string]: ILEObject } = {};
	private resolvedExports: { [name: string]: ILEObject } = {};
	private targets: { [name: string]: ILEObjectTarget } = {};

	private needsBinder = false;
	private projectBindingDirectory = DEFAULT_BINDER_TARGET;

	private actionSuggestions: TargetSuggestions = {};

	public logger: Logger;

	constructor(private cwd: string, private fs: ReadFileSystem) {
		this.logger = new Logger();
	}

	static get ignoredObjects() {
		return ignoredObjects;
	}

	getSearchGlob(): string {
		return Targets.LanguageProvider.getGlob();
	}

	public getCwd() {
		return this.cwd;
	}

	get rfs() {
		return this.fs;
	}

	public get suggestions() {
		return this.actionSuggestions;
	}

	public setAssumePrograms(assumePrograms: boolean) {
		this.assumePrograms = assumePrograms;
	}

	public setSuggestions(newSuggestions: TargetSuggestions) {
		this.actionSuggestions = newSuggestions;
	}

	public getBinderTarget() {
		return this.projectBindingDirectory;
	}

	public getRelative(fullPath: string) {
		return path.relative(this.cwd, fullPath);
	}

	storeResolved(localPath: string, ileObject: ILEObject) {
		this.resolvedObjects[localPath] = ileObject;
	}

	public async loadProject(withRef?: string) {
		if (withRef) {
			await this.handleRefsFile(path.join(this.cwd, withRef));
		}

		const initialFiles = await this.fs.getFiles(this.cwd, this.getSearchGlob());
		await this.loadObjectsFromPaths(initialFiles);
		await Promise.allSettled(initialFiles.map(f => this.parseFile(f)));
	}

	private extCanBeProgram(ext: string): boolean {
		return [`MODULE`, `PGM`].includes(Targets.LanguageProvider.getObjectType(ext));
	}

	public async resolvePathToObject(localPath: string, newText?: string) {
		if (this.resolvedObjects[localPath]) {
			if (newText) this.resolvedObjects[localPath].text = newText;
			return this.resolvedObjects[localPath];
		}

		const detail = path.parse(localPath);
		const relativePath = this.getRelative(localPath);

		const extension = detail.ext.length > 1 ? detail.ext.substring(1) : detail.ext;
		const hasProgramAttribute = detail.name.toUpperCase().endsWith(`.PGM`);
		const isProgram = this.assumePrograms ? this.extCanBeProgram(extension) : hasProgramAttribute;
		const name = getSystemNameFromPath(hasProgramAttribute ? detail.name.substring(0, detail.name.length - 4) : detail.name);
		const type: ObjectType = (isProgram ? "PGM" : this.getObjectType(relativePath, extension));

		const theObject: ILEObject = {
			systemName: name,
			type: type,
			text: newText,
			relativePath,
			extension
		};

		// If this file is an SQL file, we need to look to see if it has a long name as we need to resolve all names here
		if (sqlExtensions.includes(extension.toLowerCase())) {
			const ref = await this.sqlObjectDataFromPath(localPath);
			if (ref) {
				if (ref.object.system) theObject.systemName = ref.object.system.toUpperCase();
				if (ref.object.name) theObject.longName = ref.object.name;
				// theObject.type = ref.type;
			}
		}

		if (type === `BNDDIR`) {
			this.projectBindingDirectory = theObject;
		}

		// This allows us to override the .objrefs if the source actually exists.
		if (this.isReferenceObject(theObject, true)) {
			this.logger.fileLog(relativePath, {
				type: `info`,
				message: `The object ${theObject.systemName}.${theObject.type} is defined in the references file even though the source exists for it.`
			});
		}

		this.storeResolved(localPath, theObject);

		return theObject;
	}

	/**
	 * This can be expensive. It should only be called:
	 * before loadObjectsFromPaths and parseFile are called.
	 * @param filePath Fully qualified path to the file. Assumed to exist.
	 */
	public async handleRefsFile(filePath: string) {
		const content = await this.fs.readFile(filePath);

		const pseudoObjects = getReferenceObjectsFrom(content);

		for (const ileObject of pseudoObjects) {
			this.handleNewRefObject(ileObject);
		};
	}

	public handleNewRefObject(ileObject: ILEObject) {
		if (!this.searchForObject(ileObject)) {
			const key = `${ileObject.systemName}.${ileObject.type}`;
			ileObject.reference = true;
			this.resolvedObjects[key] = ileObject;
		}
	}

	public isReferenceObject(ileObject: ILEObject, remove?: boolean) {
		const key = `${ileObject.systemName}.${ileObject.type}`;
		const existing = this.resolvedObjects[key];
		const isRef = Boolean(existing && existing.reference);
		
		if (isRef && remove) {
			this.resolvedObjects[key] = undefined;
		}

		return isRef;
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
		return this.getResolvedObjects().find(o => (lookFor.systemName === o.systemName || (o.longName && lookFor.systemName === o.longName)) && o.type === lookFor.type);
	}

	public searchForAnyObject(lookFor: { name: string, types?: ObjectType[] }) {
		lookFor.name = lookFor.name.toUpperCase();
		return this.getResolvedObjects().find(o => (o.systemName === lookFor.name || o.longName?.toUpperCase() === lookFor.name) && (lookFor.types === undefined || lookFor.types.includes(o.type)));
	}

	public async resolveLocalFile(name: string, baseFile?: string): Promise<string> {
		name = name.toUpperCase();

		if (this.resolvedSearches[name]) return this.resolvedSearches[name];

		if (!this.pathCache) {
			this.pathCache = {};

			(await this.fs.getFiles(this.getCwd(), `**/*`, {
				cwd: this.cwd,
				absolute: true,
				nocase: true,
			})).forEach(localPath => {
				this.pathCache[localPath] = true;
			});
		}

		const searchCache = (): string|undefined => {
			for (let entry in this.pathCache) {
				if (Array.isArray(this.pathCache[entry])) {
					const subEntry = this.pathCache[entry].find(e => globalEntryIsValid(e, name));
					if (subEntry) { 
						return subEntry;
					}
				} else {
					if (globalEntryIsValid(entry, name)) {
						return entry;
					}
				}
			}
		}

		const result = searchCache();

		if (result) {
			// To local path is required because glob returns posix paths
			const localPath = toLocalPath(result)
			this.resolvedSearches[name] = localPath;
			return localPath;
		}
	}

	// TODO: move this to language provider
	getObjectType(relativePath: string, ext: string): ObjectType {
		const objType = Targets.LanguageProvider.getObjectType(ext);

		if (!objType) {
			this.logger.fileLog(relativePath, {
				type: `warning`,
				message: `'${ext}' not found a matching object type. Defaulting to '${ext}'`
			});

			return (ext.toUpperCase() as ObjectType);
		}

		return objType;
	}

	public loadObjectsFromPaths(paths: string[]) {
		// optimiseFileList(paths); //Ensure we load SQL files first
		return Promise.all(paths.map(p => this.resolvePathToObject(p)));
	}

	public async parseFile(filePath: string) {
		const pathDetail = path.parse(filePath);
		const relative = this.getRelative(filePath);

		let success = true;

		if (pathDetail.ext.length > 1) {
			if (!this.actionSuggestions.renames) {
				// Don't clear the logs if we're suggestion renames.
				this.logger.flush(relative);
			}

			const ext = pathDetail.ext.substring(1).toLowerCase();

			try {
				const content = await this.fs.readFile(filePath);

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

				await Targets.LanguageProvider.handleLanguage(this, filePath, content, options);
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
					const foundModule = allModules.find(mod => mod.exports && mod.exports.includes(exportName.toUpperCase()));
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
					this.createOrAppend(this.projectBindingDirectory, target);

					// Make sure we can resolve to this service program
					for (const e of target.exports) {
						this.resolvedExports[e.toUpperCase()] = target;
					}
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

				for (const importName of currentTarget.imports) {
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
				};

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
							const depDeps = depTargets.map(m => m?.deps).flat().filter(d => d.type === `MODULE`);

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
			headers: currentTarget.headers,
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
		currentTarget.headers = undefined;

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

	public addNewTarget(dep: ILEObjectTarget) {
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

	/**
	 * Returns a list of objects that will be impacted if the given object is changed.
	 */
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

	/**
	 * Returns a list of all the required objects to build this target
	 */
	public getRequiredObjects(bases: (ILEObject|ILEObjectTarget)[]) {
		let deps: ILEObject[] = [];

		const addDep = (dep: ILEObject|ILEObjectTarget) => {
			if (deps.some(s => s.systemName === dep.systemName && s.type === dep.type)) return; // Already added
			if (dep.reference) return; // Skip references

			const possibleTarget = ('deps' in dep ? dep : this.getTarget(dep));
			if (possibleTarget) {
				if (possibleTarget.deps && possibleTarget.deps.length > 0) {
					for (const cDep of possibleTarget.deps) {
						const d = this.getTarget(cDep) || cDep;
						addDep(d);
					}
				}
			}

			deps.push(dep);
		}

		for (const required of bases) {
			addDep(required);
		}

		return deps;
	}

	/**
	 * This is used when loading in all objects.
	 * SQL sources can have two object names: a system name and a long name
	 * Sadly the long name is not typically part of the path name, so we need to
	 * find the name inside of the source code.
	 */
	async sqlObjectDataFromPath(fullPath: string): Promise<ObjectRef> {
		const relativePath = this.getRelative(fullPath);

		if (await this.fs.exists(fullPath)) {
			const content = await this.fs.readFile(fullPath);
			const document = new Document(content);

			const groups = document.getStatementGroups();

			if (groups.length === 0) {
				this.logger.fileLog(relativePath, {
					message: `No SQL statements found in file.`,
					type: `info`
				});

				return;
			}

			const createCount = groups.filter(g => g.statements[0].type === StatementType.Create).length;

			if (createCount > 1) {
				this.logger.fileLog(relativePath, {
					message: `Includes multiple create statements. They should be in individual sources. This file will not be parsed.`,
					type: `warning`,
				});
			}

			const firstGroup = groups[0];
			const create = firstGroup.statements.find(s => s.type === StatementType.Create);

			if (create) {
				const defs = create.getObjectReferences();
				const mainDef = defs.find(d => d.createType);

				return mainDef;
			}
		}
	}
}
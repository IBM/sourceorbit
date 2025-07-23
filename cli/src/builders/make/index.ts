import { existsSync, readFileSync } from 'fs';
import path from 'path';
import { ILEObject, ILEObjectTarget, ImpactedObject, ObjectType, Targets } from '../../targets';
import { asPosix, fromCl, getFiles, toCl } from '../../utils';
import { warningOut } from '../../cli';
import { name, target } from '../../../webpack.config';
import { FolderOptions, getFolderOptions } from './folderSettings';
import { readAllRules } from './customRules';
import { CompileData, CommandParameters, getTrueBasename } from '../environment';
import { iProject } from '../iProject';
import { ReadFileSystem } from '../../readFileSystem';
import { ProjectActions } from '../actions';

interface Step {
	object: {
		name: string;
		type: ObjectType;
	}
	relativePath?: string
	command: string;
}

/**
 * parents: this property controls the all target. It will include all the parents of partial build objects.
 * partial: if this property is true, the makefile will only include targets for the partial build objects (and optionally their parents)
 */
type PartialOptions = { partial: boolean, parents: boolean };

interface PartialTargets {
	partial: ILEObject[];
	children: ILEObject[];
}

export class MakeProject {
	private partialOptions: PartialOptions = { partial: false, parents: false };
	private settings: iProject = new iProject();
	private projectActions: ProjectActions;
	private actionsEnabled: boolean = false;

	private folderSettings: {[folder: string]: FolderOptions} = {};

	constructor(private cwd: string, private targets: Targets, private rfs: ReadFileSystem) {
		this.projectActions = new ProjectActions(this.targets, this.rfs);
	}

	public setPartialOptions(options: PartialOptions) {
		this.partialOptions = options;
	}

	public useActions() {
		this.actionsEnabled = true;
	}

	async setupSettings() {
		if (this.actionsEnabled) {
			await this.projectActions.loadAllActions();
		}

		// First, let's setup the project settings
		try {
			const content = await this.rfs.readFile(path.join(this.cwd, `iproj.json`));
			const asJson: iProject = JSON.parse(content);

			this.settings.applySettings(asJson);
			warningOut(`make: Loaded project settings.`);
		} catch (e) {
			warningOut(`make: Failed to read 'iproj.json'.`);
		}

		this.folderSettings = getFolderOptions(this.cwd);

		readAllRules(this.targets, this);
	}

	public getSettings() {
		return this.settings;
	}

	getSteps(target: ILEObject|ILEObjectTarget): Step[] {
		const steps: Step[] = [];
		
		const commandOptions = {
			forAction: true,
			bindingDirectory: this.targets.getBinderTarget()
		};

		const addStep = (ileObject: ILEObject) => {
			let data = ileObject.relativePath ? this.settings.getCompileDataForSource(ileObject.relativePath) : this.settings.getCompileDataForType(ileObject.type);
			const customAttributes = this.getObjectAttributes(data, ileObject);

			if (ileObject.relativePath) {
				const possibleAction = this.projectActions.getActionForPath(ileObject.relativePath);
				if (possibleAction) {
					const clData = fromCl(possibleAction.command);
					// If there is an action for this object, we want to apply the action's parameters
					// to the custom attributes.

					data = {
						...data,
						command: clData.command,
						parameters: clData.parameters
					}
				}
			}

			if (customAttributes) {
				data.parameters = {
					...data.parameters,
					...customAttributes
				};
			}

			const qsysTempName = `QTMPSRC`;

			if (data.member || data.parameters?.srcfile) {
				data.member = true;
				data.parameters[`srcfile`] = `$(BIN_LIB)/${qsysTempName}`;
				data.parameters[`srcmbr`] = ileObject.systemName;

				steps.push(
					{
						object: {name: ileObject.systemName, type: ileObject.type},
						command: MakeProject.resolveCommand(`CPYFRMSTMF FROMSTMF('${asPosix(ileObject.relativePath)}') TOMBR('/QSYS.LIB/&CURLIB.LIB/${qsysTempName}.FILE/${data.parameters[`srcmbr`]}.MBR') MBROPT(*REPLACE)`, ileObject, commandOptions)
					}
				);
			}

			const command = MakeProject.resolveCommand(toCl(data.command, data.parameters), ileObject, commandOptions);

			steps.push({
				object: {name: ileObject.systemName, type: ileObject.type},
				relativePath: ileObject.relativePath,
				command
			});

			if (data.postCommands?.length > 0) {
				for (const postCommand of data.postCommands) {
					steps.push({
						object: {name: ileObject.systemName, type: ileObject.type},
						relativePath: ileObject.relativePath,
						command: MakeProject.resolveCommand(MakeProject.stripSystem(postCommand), ileObject, commandOptions)
					});
				}
			}
		}

		const addDepSteps = (dep: ILEObject|ILEObjectTarget) => {
			if (steps.some(s => s.object.name === dep.systemName && s.object.type === dep.type)) return; // Already added
			if (dep.reference) return; // Skip references

			if (`deps` in dep) {
				if (dep.deps && dep.deps.length > 0) {
					for (const cDep of dep.deps) {
						const d = this.targets.getTarget(cDep) || cDep;
						addDepSteps(d);
					}
				}
			}

			addStep(dep);
		}

		addDepSteps(target);

		return steps;
	}

	public getObjectAttributes(compileData: CompileData, ileObject: ILEObject): CommandParameters {
		let customAttributes = this.settings.objectAttributes[`${ileObject.systemName}.${ileObject.type}`] || {};

		if (ileObject.relativePath) {
			// We need to take in the current folders .ibmi.json file for any specific values
			const folder = path.dirname(ileObject.relativePath);
			const folderSettings = this.folderSettings[folder];
			if (folderSettings) {
				// If there is a tgtccsid, we only want to apply it to commands
				// that allow tgtccsid as a valid parameter
				if (folderSettings.build?.tgtCcsid) {
					if (compileData.parameters?.tgtccsid) {
						customAttributes.tgtccsid = folderSettings.build.tgtCcsid;
					} else if (compileData.member) {
						// Special attribute
						customAttributes.memberCcsid = folderSettings.build.tgtCcsid;
					}

					if (compileData.parameters?.compileopt) {
						customAttributes.compileopt = compileData.parameters?.compileopt.replace(/tgtccsid\([\S]*\)/i, `TGTCCSID(${folderSettings.build.tgtCcsid})`);
					}
				}
			}
		}

		return customAttributes;
	}

	public getMakefile(specificObjects?: ILEObject[]) {
		return [
			...this.generateHeader(),
			``,
			...this.generateTargets(specificObjects),
			``,
			...this.generateGenericRules(specificObjects)
		];
	}

	public generateHeader(): string[] {
		const bindingDirectory = this.targets.getBinderTarget();

		let baseBinders = [
			...(this.targets.binderRequired() ? [`($(BIN_LIB)/$(APP_BNDDIR))`] : []),
			...this.settings.binders.map(b => `(${b})`)
		];

		if (baseBinders.length === 0) baseBinders.push(`*NONE`);

		return [
			`BIN_LIB=DEV`,
			`APP_BNDDIR=${this.targets.binderRequired() ? bindingDirectory.systemName : ``}`,
			`LIBL=$(BIN_LIB)`,
			``,
			`INCDIR="${this.settings.includePaths ? this.settings.includePaths.join(`:`) : `.`}"`,
			`BNDDIR=${baseBinders.join(` `)}`,
			`PREPATH=/QSYS.LIB/$(BIN_LIB).LIB`,
			`SHELL=/QOpenSys/usr/bin/qsh`,
		];
	}

	/**
	 * Used to return the objects required to do a partial build.
	 * If `partial` is true, it will return the object and all objects depending on it recursively.
	 * If `parents` is true, it will return all parent objects of the partial build objects, and their children/
	 */
	private getPartialTargets(partialBuild?: ILEObject[]): PartialTargets|undefined {
		if (!partialBuild) {
			return;
		}

		let allParents: ILEObject[]|undefined;

		// we also want to build their parents too. We update `partialBuild`
		// to include all the parents of the specific objects.
		if (this.partialOptions.parents) {
			allParents = [];
			const impacts = partialBuild.map(o => this.targets.getImpactFor(o));

			const addImpact = (impactedObj: ImpactedObject) => {
				if (!allParents.some(o => o.systemName === impactedObj.ileObject.systemName && o.type === impactedObj.ileObject.type)) {
					allParents.push(impactedObj.ileObject);
				}

				impactedObj.children.forEach(child => addImpact(child));
			}

			impacts.forEach(impact => addImpact(impact));

			partialBuild = allParents;
		}

		let allChildren: ILEObject[]|undefined = this.partialOptions.partial ? this.targets.getRequiredObjects(partialBuild) : undefined;

		return {
			partial: partialBuild,
			children: allChildren
		}
	}

	public generateTargets(partialBuild?: ILEObject[]): string[] {
		let lines = [];

		// A 'partial build' means we only want to build specific objects
		const buildObjects = this.getPartialTargets(partialBuild);

		if (buildObjects) {
			partialBuild = buildObjects.partial;
		}

		// If we are in partial mode, we only want to generate targets for the specific objects
		const all = partialBuild || [
			...(this.targets.binderRequired() ? [this.targets.getBinderTarget()] : []),
			...this.targets.getTargetsOfType(`PGM`),
			...this.targets.getTargetsOfType(`CMD`)
		];

		if (all.length > 0) {
			lines.push(
				`all: .logs .evfevent library ${all.map(dep => `$(PREPATH)/${dep.systemName}.${dep.type}`).join(` `)}`,
				``
			)
		}

		if (buildObjects) {
			// If we don't want the children to get built, we only generate the targets for the specific objects
			for (const obj of buildObjects.children || []) {
				if (obj.reference) continue; // Skip references

				const target = this.targets.getTarget(obj);
				if (target && target.deps && target.deps.length > 0) {
					lines.push(
						`$(PREPATH)/${target.systemName}.${target.type}: ${target.deps.filter(d => d.reference !== true).map(dep => `$(PREPATH)/${dep.systemName}.${dep.type}`).join(` `)}`
					)
				}
			}
		} else {
			// If we don't want the children to get built, we don't generate the dependency targets
			for (const target of this.targets.getTargets()) {
				if (target && target.deps.length > 0) {
					lines.push(
						`$(PREPATH)/${target.systemName}.${target.type}: ${target.deps.filter(d => d.reference !== true).map(dep => `$(PREPATH)/${dep.systemName}.${dep.type}`).join(` `)}`
					)
				}
			};
		}

		lines.push(
			``,
			`.logs:`,
			`\tmkdir .logs`,
			`.evfevent:`,
			`\tmkdir .evfevent`,
			`library:`,
			`\t-system -q "CRTLIB LIB($(BIN_LIB))"`
		);

		return lines;
	}

	public generateGenericRules(partialBuild?: ILEObject[]): string[] {
		let lines = [];

		const buildObjects = this.getPartialTargets(partialBuild);

		if (buildObjects) {
			partialBuild = buildObjects.partial;
		}

		for (const entry of Object.entries(this.settings.compiles)) {
			let [type, data] = entry;

			// commandSource means 'is this object built from CL commands in a file'
			if (data.commandSource) {
				const objects = this.targets.getResolvedObjects(data.becomes);

				for (const ileObject of objects) {
					if (ileObject.reference) continue;
					if (ileObject.relativePath) {
						const sourcePath = path.join(this.cwd, ileObject.relativePath);
						const exists = existsSync(sourcePath); // Is this even needed? We already have relativePath??

						if (exists) {
							try {
								const content = readFileSync(sourcePath, { encoding: `utf-8` });
								const eol = content.indexOf(`\r\n`) >= 0 ? `\r\n` : `\n`;
								const commands = content.split(eol).filter(l => !l.startsWith(`/*`)); // Remove comments

								const customAttributes = this.settings.objectAttributes[`${ileObject.systemName}.${ileObject.type}`];

								lines.push(
									`$(PREPATH)/${ileObject.systemName}.${data.becomes}: ${asPosix(ileObject.relativePath)}`,
									...(commands.map(l => `\t-system -q "${toCl(l, customAttributes)}"`)),
									``,
								);

							} catch (e) {
								console.log(`Failed to parse '${ileObject.relativePath}'`);
								process.exit();
							}
						}
					}
				}

			} else {
				// Only used for member copies
				const objects = this.targets.getResolvedObjectsByFileExtension(type);

				if (objects.length > 0) {
					for (const ileObject of objects) {
						if (ileObject.reference) continue;

						if (buildObjects && !buildObjects.children.some(o => o.systemName === ileObject.systemName && o.type === ileObject.type)) {
							continue; // Skip this object
						}
						
						// This is used when your object really has source

						const possibleTarget: ILEObjectTarget = this.targets.getTarget(ileObject) || (ileObject as ILEObjectTarget);
						const customAttributes = this.getObjectAttributes(data, possibleTarget);

						if (ileObject.relativePath) {
							const possibleAction = this.projectActions.getActionForPath(ileObject.relativePath);
							if (possibleAction) {
								const clData = fromCl(possibleAction.command);
								// If there is an action for this object, we want to apply the action's parameters
								// to the custom attributes.

								data = {
									...data,
									command: clData.command,
									parameters: clData.parameters
								}
							}
						}
						
						lines.push(...MakeProject.generateSpecificTarget(data, possibleTarget, customAttributes));
					}

				} else
				if (data.sourceOptional) {
					// This is usually used as a generic target.
					lines.push(
						`$(PREPATH)/%.${data.becomes}: ${data.targetSource ? asPosix(data.targetSource) : ``}`,
						...(data.preCommands ? data.preCommands.map(cmd => `\t${cmd}`) : []),
						...(data.command ?
							[
								`\tliblist -c $(BIN_LIB);\\`,
								`\tliblist -a $(LIBL);\\`,
								`\tsystem "${toCl(data.command, data.parameters)}"` // TODO: write the spool file somewhere?
							]
							: []
						),
						...(data.postCommands ? data.postCommands.map(cmd => `\t${cmd}`) : []),
					);
				}
			}

			lines.push(``);

		}

		return lines;
	}

	static generateSpecificTarget(data: CompileData, ileObject: ILEObjectTarget, customAttributes?: CommandParameters): string[] {
		let lines: string[] = [];

		if (!data.command) {
			return [];
		}

		if (customAttributes) {
			data.parameters = {
				...data.parameters,
				...customAttributes
			};
		}

		let sourceFileCcsid = `*JOB`;
		
		if (data.parameters.memberCcsid) {
			sourceFileCcsid = data.parameters.memberCcsid;
			delete data.parameters.memberCcsid;
		}

		if (!data.command) {
			return undefined;
		}

		const qsysTempName = `QTMPSRC`;

		if (data.member) {
			data.parameters[`srcfile`] = `$(BIN_LIB)/${qsysTempName}`;
			data.parameters[`srcmbr`] = ileObject.systemName;
		}

		const resolvedCommand = MakeProject.resolveCommand(toCl(data.command, data.parameters), ileObject);
		const objectKey = `${ileObject.systemName}.${ileObject.type}`;

		lines.push(
			`$(PREPATH)/${objectKey}: ${asPosix(ileObject.relativePath)}`,
			...(qsysTempName && data.member ?
				[
					// TODO: consider CCSID when creating the source file
					`\t-system -qi "CRTSRCPF FILE(${data.parameters[`srcfile`]}) RCDLEN(112) CCSID(${sourceFileCcsid})"`,
					`\tsystem "CPYFRMSTMF FROMSTMF('${asPosix(ileObject.relativePath)}') TOMBR('$(PREPATH)/${qsysTempName}.FILE/${data.parameters[`srcmbr`]}.MBR') MBROPT(*REPLACE)"`
				] : []),
			...(data.preCommands ? data.preCommands.map(cmd => `\t${MakeProject.resolveCommand(cmd, ileObject)}`) : []),
			...(data.command ?
				[
					`\tliblist -c $(BIN_LIB);\\`,
					`\tliblist -a $(LIBL);\\`,
					`\tsystem "${resolvedCommand}" > .logs/${ileObject.systemName.toLowerCase()}.splf${resolvedCommand.includes(`*EVENTF`) ? ` || \\\n\t(system "CPYTOSTMF FROMMBR('$(PREPATH)/EVFEVENT.FILE/${ileObject.systemName}.MBR') TOSTMF('.evfevent/${ileObject.systemName.toLowerCase()}.evfevent') DBFCCSID(*FILE) STMFCCSID(1208) STMFOPT(*REPLACE)"; $(SHELL) -c 'exit 1')` : ``}`
				]
				: []
			),
			...(data.postCommands ? data.postCommands.map(cmd => `\t${MakeProject.resolveCommand(cmd, ileObject)}`) : []),
		);

		return lines;
	}

	private static stripSystem(command: string) {
		const firstIndex = command.indexOf(`"`);
		
		if (firstIndex >= 0) {
			if (command.substring(0, firstIndex).indexOf(`system`)) {
				const lastIndex = command.lastIndexOf(`"`);
				command = command.substring(firstIndex + 1, lastIndex);
			}
		}

		return command;
	}

	private static resolveCommand(command: string, ileObject: ILEObjectTarget|ILEObject, opts: {forAction?: boolean, bindingDirectory?: ILEObject} = {}) {
		const simpleReplace = (str: string, search: string, replace: string) => {
			return str.replace(new RegExp(search, `gi`), replace);
		}

		const qsysTempName = `QTMPSRC`;

		const isForAction = opts.forAction === true;
		const libraryValue = isForAction ? `*CURLIB` : `$(BIN_LIB)`;

		command = command.replace(new RegExp(`\\*CURLIB`, `g`), libraryValue);
		command = command.replace(new RegExp(`\\$\\*`, `g`), ileObject.systemName);
		command = command.replace(new RegExp(`\\$<`, `g`), asPosix(ileObject.relativePath));
		command = command.replace(new RegExp(`\\$\\(SRCPF\\)`, `g`), qsysTempName);

		// Additionally, we have to support Actions variables
		if (!isForAction) {
			command = simpleReplace(command, `&BUILDLIB`, libraryValue);
			command = simpleReplace(command, `&CURLIB`, libraryValue);
			command = simpleReplace(command, `&LIBLS`, ``);
			command = simpleReplace(command, `&BRANCHLIB`, libraryValue);

			const pathDetail = path.parse(ileObject.relativePath || ``);

			command = simpleReplace(command, `&RELATIVEPATH`, asPosix(ileObject.relativePath));
			command = simpleReplace(command, `&BASENAME`, pathDetail.base);
			command = simpleReplace(command, `{filename}`, pathDetail.base);
			command = simpleReplace(command, `&NAME`, getTrueBasename(pathDetail.name));
			command = simpleReplace(command, `&EXTENSION`, pathDetail.ext.startsWith(`.`) ? pathDetail.ext.substring(1) : pathDetail.ext);
		}

		if (`deps` in ileObject) {
			if (ileObject.deps && ileObject.deps.length > 0) {
				// This piece of code adds special variables that can be used for building dependencies
				const uniqueObjectTypes = ileObject.deps.map(d => d.type).filter((value, index, array) => array.indexOf(value) === index);

				for (const objType of uniqueObjectTypes) {
					const specificDeps = ileObject.deps.filter(d => d.type === objType);
					command = command.replace(new RegExp(`\\*${objType}S`, `g`), specificDeps.map(d => d.systemName).join(` `));
				}
			}
		}

		if (isForAction) {
			command = simpleReplace(command, `\\$\\(BIN_LIB\\)`, libraryValue);
			command = simpleReplace(command, `\\$\\(BNDDIR\\)`, opts.bindingDirectory ? opts.bindingDirectory.systemName : `*NONE`);
			command = simpleReplace(command, `\\$\\(APP_BNDDIR\\)`, `APP`); // Default name
			command = simpleReplace(command, `&SRCFILE`, `${libraryValue}/${qsysTempName}`);
			command = simpleReplace(command, `&SRCPF`, qsysTempName);
			command = simpleReplace(command, `&SRCLIB`, libraryValue);
		}
		
		return command;
	}
}
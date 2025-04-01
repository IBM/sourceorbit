import { existsSync, readFileSync } from 'fs';
import path from 'path';
import { ILEObject, ILEObjectTarget, ImpactedObject, ObjectType, Targets } from '../../targets';
import { asPosix, getFiles, toCl } from '../../utils';
import { warningOut } from '../../cli';
import { name } from '../../../webpack.config';
import { FolderOptions, getFolderOptions } from './folderSettings';
import { readAllRules } from './customRules';
import { CompileData, CommandParameters } from '../environment';
import { iProject } from '../iProject';

export class MakeProject {
	private noChildren: boolean = false;
	private settings: iProject = new iProject();
	private folderSettings: {[folder: string]: FolderOptions} = {};

	constructor(private cwd: string, private targets: Targets) {
		this.setupSettings();
	}

	public setNoChildrenInBuild(noChildren: boolean) {
		this.noChildren = noChildren;
	}

	private setupSettings() {
		// First, let's setup the project settings
		try {
			const content = readFileSync(path.join(this.cwd, `iproj.json`), { encoding: `utf-8` });
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

	public getObjectAttributes(compileData: CompileData, ileObject: ILEObject): CommandParameters {
		let customAttributes = this.settings.objectAttributes[`${ileObject.systemName}.${ileObject.type}`] || {};

		if (ileObject.source) {
			// We need to take in the current folders .ibmi.json file for any specific values
			const folder = ileObject.source && ileObject.source.relativePath ? path.dirname(ileObject.source.relativePath) : undefined;
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
			...this.generateGenericRules()
		];
	}

	public generateHeader(): string[] {
		let baseBinders = [
			...(this.targets.binderRequired() ? [`($(BIN_LIB)/$(APP_BNDDIR))`] : []),
			...this.settings.binders.map(b => `(${b})`)
		];

		if (baseBinders.length === 0) baseBinders.push(`*NONE`);

		return [
			`BIN_LIB=DEV`,
			`APP_BNDDIR=APP`,
			`LIBL=$(BIN_LIB)`,
			``,
			`INCDIR="${this.settings.includePaths ? this.settings.includePaths.join(`:`) : `.`}"`,
			`BNDDIR=${baseBinders.join(` `)}`,
			`PREPATH=/QSYS.LIB/$(BIN_LIB).LIB`,
			`SHELL=/QOpenSys/usr/bin/qsh`,
		];
	}

	public generateTargets(partialBuild?: ILEObject[]): string[] {
		let lines = [];

		// A 'partial build' means we only want to build specific objects
		// and we also want to build their parents too. We update `partialBuild`
		// to include all the parents of the specific objects.
		if (partialBuild) {
			const impacts = partialBuild.map(o => this.targets.getImpactFor(o));

			let allAffected: ILEObject[] = [];

			const addImpact = (impactedObj: ImpactedObject) => {
				if (!allAffected.some(o => o.systemName === impactedObj.ileObject.systemName && o.type === impactedObj.ileObject.type)) {
					allAffected.push(impactedObj.ileObject);
				}

				impactedObj.children.forEach(child => addImpact(child));
			}

			impacts.forEach(impact => addImpact(impact));

			partialBuild = allAffected;
		}

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

		if (!this.noChildren) {
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

	public generateGenericRules(): string[] {
		let lines = [];

		for (const entry of Object.entries(this.settings.compiles)) {
			const [type, data] = entry;

			// commandSource means 'is this object built from CL commands in a file'
			if (data.commandSource) {
				const objects = this.targets.getResolvedObjects(data.becomes);

				for (const ileObject of objects) {
					if (ileObject.reference) continue;
					if (ileObject.source) {
						const sourcePath = path.join(this.cwd, ileObject.source.relativePath);
						const exists = existsSync(sourcePath); // Is this even needed? We already have relativePath??

						if (exists) {
							try {
								const content = readFileSync(sourcePath, { encoding: `utf-8` });
								const eol = content.indexOf(`\r\n`) >= 0 ? `\r\n` : `\n`;
								const commands = content.split(eol).filter(l => !l.startsWith(`/*`)); // Remove comments

								const customAttributes = this.settings.objectAttributes[`${ileObject.systemName}.${ileObject.type}`];

								lines.push(
									`$(PREPATH)/${ileObject.systemName}.${data.becomes}: ${asPosix(ileObject.source.relativePath)}`,
									...(commands.map(l => `\t-system -q "${toCl(l, customAttributes)}"`)),
									``,
								);

							} catch (e) {
								console.log(`Failed to parse '${ileObject.source.relativePath}'`);
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
						
						// This is used when your object really has source

						const possibleTarget: ILEObjectTarget = this.targets.getTarget(ileObject) || (ileObject as ILEObjectTarget);
						const customAttributes = this.getObjectAttributes(data, possibleTarget);
						
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

		const parentName = ileObject.source?.relativePath ? path.dirname(ileObject.source.relativePath) : undefined;
		const qsysTempName: string | undefined = (parentName && parentName.length > 10 ? parentName.substring(0, 10) : parentName);

		const resolve = (command: string) => {
			command = command.replace(new RegExp(`\\*CURLIB`, `g`), `$(BIN_LIB)`);
			command = command.replace(new RegExp(`\\$\\*`, `g`), ileObject.systemName);
			command = command.replace(new RegExp(`\\$<`, `g`), asPosix(ileObject.source.relativePath));
			command = command.replace(new RegExp(`\\$\\(SRCPF\\)`, `g`), qsysTempName);

			if (ileObject.deps && ileObject.deps.length > 0) {
				// This piece of code adds special variables that can be used for building dependencies
				const uniqueObjectTypes = ileObject.deps.map(d => d.type).filter((value, index, array) => array.indexOf(value) === index);

				for (const objType of uniqueObjectTypes) {
					const specificDeps = ileObject.deps.filter(d => d.type === objType);
					command = command.replace(new RegExp(`\\*${objType}S`, `g`), specificDeps.map(d => d.systemName).join(` `));
				}
			}

			return command;
		}

		// TODO: resolve the parameters from the Rules.mk
		const objectKey = `${ileObject.systemName}.${ileObject.type}`;
		
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

		const resolvedCommand = resolve(toCl(data.command, data.parameters));

		lines.push(
			`$(PREPATH)/${objectKey}: ${asPosix(ileObject.source.relativePath)}`,
			...(qsysTempName && data.member ?
				[
					// TODO: consider CCSID when creating the source file
					`\t-system -qi "CRTSRCPF FILE($(BIN_LIB)/${qsysTempName}) RCDLEN(112) CCSID(${sourceFileCcsid})"`,
					`\tsystem "CPYFRMSTMF FROMSTMF('${asPosix(ileObject.source.relativePath)}') TOMBR('$(PREPATH)/${qsysTempName}.FILE/${ileObject.systemName}.MBR') MBROPT(*REPLACE)"`
				] : []),
			...(data.preCommands ? data.preCommands.map(cmd => `\t${resolve(cmd)}`) : []),
			...(data.command ?
				[
					`\tliblist -c $(BIN_LIB);\\`,
					`\tliblist -a $(LIBL);\\`,
					`\tsystem "${resolvedCommand}" > .logs/${ileObject.systemName.toLowerCase()}.splf${resolvedCommand.includes(`*EVENTF`) ? ` || \\\n\t(system "CPYTOSTMF FROMMBR('$(PREPATH)/EVFEVENT.FILE/${ileObject.systemName}.MBR') TOSTMF('.evfevent/${ileObject.systemName.toLowerCase()}.evfevent') DBFCCSID(*FILE) STMFCCSID(1208) STMFOPT(*REPLACE)"; $(SHELL) -c 'exit 1')` : ``}`
				]
				: []
			),
			...(data.postCommands ? data.postCommands.map(cmd => `\t${resolve(cmd)}`) : []),
		);

		return lines;
	}
}
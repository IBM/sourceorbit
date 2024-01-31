import { existsSync, readFileSync } from 'fs';
import path from 'path';
import { ILEObject, ILEObjectTarget, ImpactedObject, ObjectType, Targets } from '../targets';
import { asPosix, getFiles, toCl } from '../utils';
import { warningOut } from '../cli';
import { name } from '../../webpack.config';

// Always try and store parmId as lowercase
type CommandParameters = {[parmId: string]: string};

interface FolderOptions {
	version?: "0.0.1",
	options?: {
		objLib?: string,
		tgtCcsid?: string
	}
}

interface CompileData {
	/** indicates what type of object will be built from this source */
	becomes: ObjectType;
	/** will copy the source to a temp member first */
	member?: boolean,
	/** `preCommands` do not respect the library list and is run before 'command' */
	preCommands?: string[]
	/** `command` does respect the library list */
	command?: string;
	
	parameters?: CommandParameters;
	/** Used if the commands are built up from source. Usually means `command` and `commands` is blank */
	commandSource?: boolean;
	/** `postCommands` do not respect the library list and is run after 'command' */
	postCommands?: string[];

	/** if the object can be built without source, flag this true so it builds generic rules */
	sourceOptional?: boolean;
	/** if the non-source object now requires source. Use make generic name like `qbndsrc/%.bnd` */
	targetSource?: string;
};

interface iProject {
	includePaths?: string[];
	compiles?: { [ext: string]: CompileData },
	binders?: string[];
	objectAttributes?: {
		[object: string]: CommandParameters
	}
}

export class MakeProject {
	private noChildren: boolean = false;
	private settings: iProject;
	private folderSettings: {[folder: string]: FolderOptions} = {};

	constructor(private cwd: string, private targets: Targets) {
		this.settings = MakeProject.getDefaultSettings();

		this.setupSettings();
	}

	public setNoChildrenInBuild(noChildren: boolean) {
		this.noChildren = noChildren;
	}

	public static getDefaultSettings(): iProject {
		return {
			binders: [],
			includePaths: [],
			objectAttributes: {},
			compiles: {
				"pgm": {
					becomes: `PGM`,
					command: `CRTPGM`,
					parameters: {
						pgm: `$(BIN_LIB)/$*`,
						entry: `$*`,
						modules: `*MODULES`,
						tgtrls: `*CURRENT`,
						tgtccsid: `*JOB`,
						bnddir: `$(BNDDIR)`,
						dftactgrp: `*NO`
					}
				},
				"pgm.rpgle": {
					becomes: `PGM`,
					command: `CRTBNDRPG`,
					parameters: {
						pgm: `$(BIN_LIB)/$*`,
						srcstmf: `'$<'`,
						option: `*EVENTF`,
						dbgview: `*SOURCE`,
						tgtrls: `*CURRENT`,
						tgtccsid: `*JOB`,
						bnddir: `$(BNDDIR)`,
						dftactgrp: `*NO`
					}
				},
				"pgm.sqlrpgle": {
					becomes: "PGM",
					command: `CRTSQLRPGI`,
					parameters: {
						obj: `$(BIN_LIB)/$*`,
						srcstmf: `'$<'`,
						commit: `*NONE`,
						dbgview: `*SOURCE`,
						option: `*EVENTF`,
						rpgppopt: `*LVL2`,
						compileopt: `TGTCCSID(*JOB) BNDDIR($(BNDDIR)) DFTACTGRP(*no)`
					}
				},
				"rpgle": {
					becomes: `MODULE`,
					command: `CRTRPGMOD`,
					parameters: {
						module: `$(BIN_LIB)/$*`,
						srcstmf: `'$<'`,
						option: `*EVENTF`,
						dbgview: `*SOURCE`,
						tgtrls: `*CURRENT`,
						tgtccsid: `*JOB`
					}
				},
				"sqlrpgle": {
					becomes: "MODULE",
					command: `CRTSQLRPGI`,
					parameters: {
						obj: `$(BIN_LIB)/$*`,
						srcstmf: `'$<'`,
						commit: `*NONE`,
						dbgview: `*SOURCE`,
						compileopt: `'TGTCCSID(*JOB)'`,
						rpgppopt: `*LVL2`,
						option: `*EVENTF`,
						objtype: `*MODULE`
					}
				},
				"pgm.clle": {
					becomes: `PGM`,
					command: `CRTBNDCL`,
					parameters: {
						pgm: `$(BIN_LIB)/$*`,
						srcstmf: `'$<'`,
						option: `*EVENTF`,
						dbgview: `*SOURCE`,
						tgtrls: `*CURRENT`,
						dftactgrp: `*NO`
					}
				},
				dspf: {
					becomes: "FILE",
					member: true,
					command: "CRTDSPF",
					parameters: {
						file: `$(BIN_LIB)/$*`,
						srcfile: `$(BIN_LIB)/$(SRCPF)`,
						srcmbr: `$*`,
						option: `*EVENTF`
					}
				},
				prtf: {
					becomes: "FILE",
					member: true,
					command: "CRTPRTF FILE($(BIN_LIB)/$*)",
					parameters: {
						file: `$(BIN_LIB)/$*`,
						srcfile: `$(BIN_LIB)/$(SRCPF)`,
						srcmbr: `$*`,
						option: `*EVENTF`
					}
				},
				cmd: {
					becomes: "CMD",
					member: true,
					command: "CRTCMD",
					parameters: {
						cmd: `$(BIN_LIB)/$*`,
						pgm: `$(BIN_LIB)/$*`,
						srcfile: `$(BIN_LIB)/$(SRCPF)`,
						option: `*EVENTF`
					}
				},
				sql: {
					becomes: `FILE`,
					command: `RUNSQLSTM`,
					parameters: {
						srcstmf: `'$<'`,
						commit: `*NONE`
					}
				},
				sqludf: {
					becomes: `SRVPGM`,
					command: `RUNSQLSTM`,
					parameters: {
						srcstmf: `'$<'`,
						commit: `*NONE`
					}
				},
				table: {
					becomes: `FILE`,
					command: `RUNSQLSTM`,
					parameters: {
						srcstmf: `'$<'`,
						commit: `*NONE`
					}
				},
				binder: binderSourceCompile,
				bnd: binderSourceCompile,
				srvpgm: {
					becomes: `SRVPGM`,
					preCommands: [
						`-system -q "CRTBNDDIR BNDDIR($(BIN_LIB)/$(APP_BNDDIR))"`,
						`-system -q "RMVBNDDIRE BNDDIR($(BIN_LIB)/$(APP_BNDDIR)) OBJ(($(BIN_LIB)/$*))"`,
						`-system "DLTOBJ OBJ($(BIN_LIB)/$*) OBJTYPE(*SRVPGM)"`
					],
					command: `CRTSRVPGM`,
					parameters: {
						srvpgm: `$(BIN_LIB)/$*`,
						module: `*MODULES`,
						srcstmf: `'$<'`,
						bnddir: `$(BNDDIR)`
					},
					postCommands: [
						`-system -q "ADDBNDDIRE BNDDIR($(BIN_LIB)/$(APP_BNDDIR)) OBJ((*LIBL/$* *SRVPGM *IMMED))"`
					]
				},
				bnddir: {
					sourceOptional: true,
					becomes: `BNDDIR`,
					preCommands: [
						`-system -q "CRTBNDDIR BNDDIR($(BIN_LIB)/$*)"`,
						`-system -q "ADDBNDDIRE BNDDIR($(BIN_LIB)/$*) OBJ($(patsubst %.SRVPGM,(*LIBL/% *SRVPGM *IMMED),$(notdir $^)))"`
					]
				},
				dtaara: {
					becomes: `DTAARA`,
					commandSource: true
				},
				mnucmd: {
					becomes: `MENU`,
					member: true,
					command: `CRTMNU`,
					parameters: {
						menu: `$(BIN_LIB)/$*`,
						type: `*DSPF`,
						dspf: `$(BIN_LIB)/$*`
					}
				},
				pf: {
					becomes: `FILE`,
					member: true,
					command: `CRTPF`,
					parameters: {
						file: `$(BIN_LIB)/$*`,
						srcfile: `$(BIN_LIB)/$(SRCPF)`,
						option: `*EVENTF`
					}
				},
				lf: {
					becomes: `FILE`,
					member: true,
					command: `CRTLF`,
					parameters: {
						file: `$(BIN_LIB)/$*`,
						srcfile: `$(BIN_LIB)/$(SRCPF)`,
						option: `*EVENTF`
					}
				}
			}
		};
	}

	private setupSettings() {
		// First, let's setup the project settings
		try {
			const content = readFileSync(path.join(this.cwd, `iproj.json`), { encoding: `utf-8` });
			const asJson: iProject = JSON.parse(content);

			this.applySettings(asJson);
			warningOut(`make: Loaded project settings.`);
		} catch (e) {
			warningOut(`make: Failed to read 'iproj.json'.`);
		}

		// Then fetch the directory specific settings
		const ibmiFiles = getFiles(this.cwd, `**/.ibmi.json`);

		for (const ibmiFile of ibmiFiles) {
			const relative = path.relative(this.cwd, ibmiFile);
			const folder = path.dirname(relative);

			try {
				this.folderSettings[folder] = JSON.parse(readFileSync(ibmiFile, { encoding: `utf-8` }));
			} catch (e) {
				warningOut(`make: Failed to read ${relative}.`);
			}
		}

		// getFiles is case insensitive
		const rulesFiles = getFiles(this.cwd, `**/rules.mk`);
		this.settings.objectAttributes = {};

		for (const rulesFile of rulesFiles) {
			const relative = path.relative(this.cwd, rulesFile);
			const folder = path.dirname(relative);

			try {
				const content = readFileSync(rulesFile, { encoding: `utf-8` });
				const lines = content.split(`\n`);

				for (const line of lines) {
					const nameSplit = line.indexOf(`:`);
					if (nameSplit < 0) continue;

					const name = line.substring(0, nameSplit).trim().toUpperCase();
					const value = line.substring(nameSplit + 1).trim();

					const assignmentSplit = value.indexOf(`=`);
					if (assignmentSplit >= 0) {
						// If there is an assignment value, this means we're
						// setting a compile parameter on a specific object
						const key = value.substring(0, assignmentSplit).trim().toLowerCase();
						const val = value.substring(assignmentSplit + 1).trim();

						if (!this.settings.objectAttributes[name]) this.settings.objectAttributes[name] = {};

						this.settings.objectAttributes[name][key] = val;

					} else {
						// Otherwise, we're overriding the deps with hardcoded deps

						const nameParts = name.split(`.`);
						const targetName = nameParts[0];
						const targetType = nameParts[1] as ObjectType;

						if (targetName && targetType) {
							const currentTarget = this.targets.getTarget({systemName: targetName, type: targetType});

							if (currentTarget) {
								// We set this to empty since we're overriding them
								currentTarget.deps = [];

								const parts = value.split(` `).map(p => p.trim()).filter(p => p.length > 0);

								// We always skip the first because ibmi-bob wants the first entry to be the source name.
								for (let i = 1; i < parts.length; i++) {
									const part = parts[i];
									const partSplit = part.split(`.`);
									const objName = partSplit[0];
									const objType = partSplit[1] as ObjectType;

									if (objName && objType) {
										const obj = this.targets.searchForObject({systemName: objName, type: objType}, undefined);

										if (obj) {
											currentTarget.deps.push(obj);
										} else {
											warningOut(`make: Failed to find '${part}' in '${relative}'`);
										}
									}
								}
							}
					}

					}
				}

			} catch (e) {
				warningOut(`make: Failed to read ${relative}.`);
			}
		}
	}

	public getSettings() {
		return this.settings;
	}

	public applySettings(input: iProject) {
		if (input.includePaths && input.includePaths.length > 0) {
			this.settings.includePaths = input.includePaths;
		}

		if (input.binders && input.binders.length > 0) {
			this.settings.binders = input.binders;
		}

		if (input.compiles) {
			for (const [ext, data] of Object.entries(input.compiles)) {
				// We don't want to fully overwrite the default settings,
				// perhaps the user is only changing the `dir`?
				this.settings.compiles[ext] = {
					...(this.settings.compiles[ext] || {}),
					...data
				};
			}
		}
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
						`$(PREPATH)/${target.systemName}.${target.type}: ${target.deps.map(dep => `$(PREPATH)/${dep.systemName}.${dep.type}`).join(` `)}`
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
					if (ileObject.relativePath) {
						const sourcePath = path.join(this.cwd, ileObject.relativePath);
						const exists = existsSync(sourcePath);

						if (exists) {
							try {
								const content = readFileSync(sourcePath, { encoding: `utf-8` });
								const eol = content.indexOf(`\r\n`) >= 0 ? `\r\n` : `\n`;
								const commands = content.split(eol).filter(l => !l.startsWith(`/*`)); // Remove comments

								lines.push(
									`$(PREPATH)/${ileObject.systemName}.${data.becomes}: ${asPosix(ileObject.relativePath)}`,
									...(commands.map(l => `\t-system -q "${l}"`)),
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
						// This is used when your object really has source

						const possibleTarget: ILEObjectTarget = this.targets.getTarget(ileObject) || (ileObject as ILEObjectTarget);
						let customAttributes = this.settings.objectAttributes[`${ileObject.systemName}.${ileObject.type}`] || {};

						if (ileObject.relativePath) {
							// We need to take in the current folders .ibmi.json file for any specific values
							const folder = path.dirname(ileObject.relativePath);
							const folderSettings = this.folderSettings[folder];
							if (folderSettings) {
								// If there is a tgtccsid, we only want to apply it to commands
								// that allow tgtccsid as a valid parameter
								if (folderSettings.options?.tgtCcsid && data.parameters?.tgtccsid) {
									customAttributes.tgtccsid = folderSettings.options.tgtCcsid;
								}

								// Then if this setting has a objLib, we want to replace all instances
								// of $(BIN_LIB) with the correct value correct value.
								if (folderSettings.options?.objLib) {
									const objLib = folderSettings.options.objLib;
									const correctValue = objLib.startsWith(`&`) ? `$(${objLib})` : objLib;
									// Replace each occurance of $(BIN_LIB) with the correct value
									for (const key of Object.keys(customAttributes)) {
										customAttributes[key] = customAttributes[key].replace(new RegExp(`\\$\\(BIN_LIB\\)`, `g`), correctValue);
									}
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

		const parentName = ileObject.relativePath ? path.dirname(ileObject.relativePath) : undefined;
		const qsysTempName: string | undefined = (parentName && parentName.length > 10 ? parentName.substring(0, 10) : parentName);

		const resolve = (command: string) => {
			command = command.replace(new RegExp(`\\*CURLIB`, `g`), `$(BIN_LIB)`);
			command = command.replace(new RegExp(`\\$\\*`, `g`), ileObject.systemName);
			command = command.replace(new RegExp(`\\$<`, `g`), asPosix(ileObject.relativePath));
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

		const resolvedCommand = resolve(toCl(data.command, data.parameters));

		lines.push(
			`$(PREPATH)/${objectKey}: ${asPosix(ileObject.relativePath)}`,
			...(qsysTempName && data.member ?
				[
					`\t-system -qi "CRTSRCPF FILE($(BIN_LIB)/${qsysTempName}) RCDLEN(112)"`,
					`\tsystem "CPYFRMSTMF FROMSTMF('${asPosix(ileObject.relativePath)}') TOMBR('$(PREPATH)/${qsysTempName}.FILE/${ileObject.systemName}.MBR') MBROPT(*REPLACE)"`
				] : []),
			...(data.preCommands ? data.preCommands.map(cmd => `\t${resolve(cmd)}`) : []),
			...(data.command ?
				[
					`\tliblist -c $(BIN_LIB);\\`,
					`\tliblist -a $(LIBL);\\`,
					`\tsystem "${resolvedCommand}" > .logs/${ileObject.systemName.toLowerCase()}.splf` // TODO: write the spool file somewhere?
				]
				: []
			),
			...(data.postCommands ? data.postCommands.map(cmd => `\t${resolve(cmd)}`) : []),
			...(resolvedCommand.includes(`*EVENTF`) ? [`\tsystem "CPYTOSTMF FROMMBR('$(PREPATH)/EVFEVENT.FILE/${ileObject.systemName}.MBR') TOSTMF('.evfevent/${ileObject.systemName.toLowerCase()}.evfevent') DBFCCSID(*FILE) STMFCCSID(1208) STMFOPT(*REPLACE)"`] : []),
		);

		return lines;
	}
}

const binderSourceCompile: CompileData = {
	becomes: `SRVPGM`,
	preCommands: [
		`-system -q "CRTBNDDIR BNDDIR($(BIN_LIB)/$(APP_BNDDIR))"`,
		// `-system -q "RMVBNDDIRE BNDDIR($(BIN_LIB)/$(APP_BNDDIR)) OBJ(($(BIN_LIB)/$*))"`,
		// `-system "DLTOBJ OBJ($(BIN_LIB)/$*) OBJTYPE(*SRVPGM)"`
	],
	command: `CRTSRVPGM`,
	parameters: {
		srvpgm: `$(BIN_LIB)/$*`,
		module: `*MODULES`,
		srcstmf: `'$<'`,
		bnddir: `$(BNDDIR)`,
		replace: `*YES`
	},
	postCommands: [
		`-system -q "ADDBNDDIRE BNDDIR($(BIN_LIB)/$(APP_BNDDIR)) OBJ((*LIBL/$* *SRVPGM *IMMED))"`
	]
};
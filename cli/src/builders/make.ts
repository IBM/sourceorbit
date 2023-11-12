import { existsSync, readFileSync } from 'fs';
import path from 'path';
import { ILEObject, ILEObjectTarget, ObjectType, Targets } from '../targets';
import { asPosix } from '../utils';

interface CompileData {
	/** indicates what type of object will be built from this source */
	becomes: ObjectType;
	/** will copy the source to a temp member first */
	member?: boolean,
	/** `preCommands` do not respect the library list and is run before 'command' */
	preCommands?: string[]
	/** `command` does respect the library list */
	command?: string;
	/** Used if the commands are built up from source. Usually means `command` and `commands` is blank */
	commandSource?: boolean;
	/** `postCommands` do not respect the library list and is run after 'command' */
	postCommands?: string[]
	/** if the non-source object now requires source. Use make generic name like `qbndsrc/%.bnd` */
	targetSource?: string;
	/** if the object can be built without source, flag this true so it builds generic rules */
	sourceOptional?: boolean;
};

interface iProject {
	includePaths?: string[];
	compiles?: { [ext: string]: CompileData },
	binders?: string[];
}

export class MakeProject {
	private settings: iProject;

	constructor(private cwd: string, private targets: Targets) {
		this.settings = MakeProject.getDefaultSettings();

		this.setupSettings();
	}

	public static getDefaultSettings(): iProject {
		return {
			binders: [],
			includePaths: [],
			compiles: {
				"pgm": {
					becomes: `PGM`,
					command: `CRTPGM PGM($(BIN_LIB)/$*) ENTRY($*) MODULES(*MODULES) TGTRLS(*CURRENT) TGTCCSID(*JOB) BNDDIR($(BNDDIR)) DFTACTGRP(*no)` // TODO: fix this
				},
				"pgm.rpgle": {
					becomes: `PGM`,
					command: `CRTBNDRPG PGM($(BIN_LIB)/$*) SRCSTMF('$<') OPTION(*EVENTF) DBGVIEW(*SOURCE) TGTRLS(*CURRENT) TGTCCSID(*JOB) BNDDIR($(BNDDIR)) DFTACTGRP(*no)`
				},
				"pgm.sqlrpgle": {
					becomes: "PGM",
					preCommands: [
						`system -s "CHGATR OBJ('$<') ATR(*CCSID) VALUE(1252)"`
					],
					command: `CRTSQLRPGI OBJ($(BIN_LIB)/$*) SRCSTMF('$<') COMMIT(*NONE) DBGVIEW(*SOURCE) OPTION(*EVENTF) COMPILEOPT('BNDDIR($(BNDDIR)) DFTACTGRP(*no)')`
				},
				"rpgle": {
					becomes: `MODULE`,
					command: `CRTRPGMOD MODULE($(BIN_LIB)/$*) SRCSTMF('$<') OPTION(*EVENTF) DBGVIEW(*SOURCE) TGTRLS(*CURRENT) TGTCCSID(*JOB)`
				},
				"sqlrpgle": {
					becomes: "MODULE",
					preCommands: [
						`system -s "CHGATR OBJ('$<') ATR(*CCSID) VALUE(1252)"`
					],
					command: `CRTSQLRPGI OBJ($(BIN_LIB)/$*) SRCSTMF('$<') COMMIT(*NONE) DBGVIEW(*SOURCE) OPTION(*EVENTF) OBJTYPE(*MODULE)`
				},
				"pgm.clle": {
					becomes: `PGM`,
					command: `CRTBNDCL PGM($(BIN_LIB)/$*) SRCSTMF('$<') OPTION(*EVENTF) DBGVIEW(*SOURCE) TGTRLS(*CURRENT) DFTACTGRP(*no)`
				},
				dspf: {
					becomes: "FILE",
					member: true,
					command: "CRTDSPF FILE($(BIN_LIB)/$*) SRCFILE($(BIN_LIB)/$(SRCPF)) SRCMBR($*) OPTION(*EVENTF)"
				},
				prtf: {
					becomes: "FILE",
					member: true,
					command: "CRTPRTF FILE($(BIN_LIB)/$*) SRCFILE($(BIN_LIB)/$(SRCPF)) SRCMBR($*) OPTION(*EVENTF)"
				},
				cmd: {
					becomes: "CMD",
					member: true,
					command: "CRTCMD CMD($(BIN_LIB)/$*) PGM($(BIN_LIB)/$*) SRCFILE($(BIN_LIB)/$(SRCPF)) OPTION(*EVENTF)"
				},
				sql: {
					becomes: `FILE`,
					command: `RUNSQLSTM SRCSTMF('$<') COMMIT(*NONE)`
				},
				table: {
					becomes: `FILE`,
					command: `RUNSQLSTM SRCSTMF('$<') COMMIT(*NONE)`
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
					command: `CRTSRVPGM SRVPGM($(BIN_LIB)/$*) MODULE(*MODULES) SRCSTMF('$<') BNDDIR($(BNDDIR))`,
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
					command: `CRTMNU MENU($(BIN_LIB)/$*) TYPE(*DSPF) DSPF($(BIN_LIB)/$*)`
				},
				pf: {
					becomes: `FILE`,
					member: true,
					command: `CRTPF FILE($(BIN_LIB)/$*) SRCFILE($(BIN_LIB)/$(SRCPF)) OPTION(*EVENTF)`
				},
				lf: {
					becomes: `FILE`,
					member: true,
					command: `CRTLF FILE($(BIN_LIB)/$*) SRCFILE($(BIN_LIB)/$(SRCPF)) OPTION(*EVENTF)`
				}
			}
		};
	}

	private setupSettings() {
		try {
			const content = readFileSync(path.join(this.cwd, `iproj.json`), { encoding: `utf-8` });
			const asJson: iProject = JSON.parse(content);

			this.applySettings(asJson);
		} catch (e) {
			// console.log(`Failed to read 'iproj.json'.`);
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

	public generateTargets(specificObjects?: ILEObject[]): string[] {
		let lines = [];

		const all = specificObjects || [
			...(this.targets.binderRequired() ? [this.targets.getBinderTarget()] : []),
			...this.targets.getParentObjects(`PGM`),
			...this.targets.getParentObjects(`CMD`)
		];

		if (all.length > 0) {
			lines.push(
				`all: .logs .evfevent ${all.map(dep => `$(PREPATH)/${dep.name}.${dep.type}`).join(` `)}`,
				``
			)
		}

		for (const target of this.targets.getDeps()) {
			if (target && target.deps.length > 0) {
				lines.push(
					`$(PREPATH)/${target.name}.${target.type}: ${target.deps.map(dep => `$(PREPATH)/${dep.name}.${dep.type}`).join(` `)}`
				)
			}
		};

		lines.push(
			``,
			`.logs:`,
			`  mkdir .logs`,
			`.evfevent:`,
			`  mkdir .evfevent`,
			`library: $(PREPATH)`,
			`  mkdir -system -q "CRTLIB LIB($(BIN_LIB))"`
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
									`$(PREPATH)/${ileObject.name}.${data.becomes}: ${asPosix(ileObject.relativePath)}`,
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
				const objects = this.targets.getObjectsByExtension(type);

				if (objects.length > 0) {
					for (const ileObject of objects) {
						// This is used when your object really has source

						const possibleTarget: ILEObjectTarget = this.targets.getDep(ileObject) || (ileObject as ILEObjectTarget);
						
						lines.push(...MakeProject.generateSpecificTarget(data, possibleTarget));
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
								`\tsystem "${data.command}"` // TODO: write the spool file somewhere?
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

	static generateSpecificTarget(data: CompileData, ileObject: ILEObjectTarget) {
		let lines: string[] = [];

		const parentName = ileObject.relativePath ? path.dirname(ileObject.relativePath) : undefined;
		const qsysTempName: string | undefined = (parentName && parentName.length > 10 ? parentName.substring(0, 10) : parentName);

		const resolve = (command: string) => {
			command = command.replace(new RegExp(`\\*CURLIB`, `g`), `$(BIN_LIB)`);
			command = command.replace(new RegExp(`\\$\\*`, `g`), ileObject.name);
			command = command.replace(new RegExp(`\\$<`, `g`), asPosix(ileObject.relativePath));
			command = command.replace(new RegExp(`\\$\\(SRCPF\\)`, `g`), qsysTempName);

			if (ileObject.deps && ileObject.deps.length > 0) {
				// This piece of code adds special variables that can be used for building dependencies
				const uniqueObjectTypes = ileObject.deps.map(d => d.type).filter((value, index, array) => array.indexOf(value) === index);

				for (const objType of uniqueObjectTypes) {
					const specificDeps = ileObject.deps.filter(d => d.type === objType);
					command = command.replace(new RegExp(`\\*${objType}S`, `g`), specificDeps.map(d => d.name).join(` `));
				}
			}

			return command;
		}

		const resolvedCommand = resolve(data.command);

		lines.push(
			`$(PREPATH)/${ileObject.name}.${ileObject.type}: ${asPosix(ileObject.relativePath)}`,
			...(qsysTempName && data.member ?
				[
					`\t-system -qi "CRTSRCPF FILE($(BIN_LIB)/${qsysTempName}) RCDLEN(112)"`,
					`\tsystem "CPYFRMSTMF FROMSTMF('${asPosix(ileObject.relativePath)}') TOMBR('$(PREPATH)/${qsysTempName}.FILE/${ileObject.name}.MBR') MBROPT(*REPLACE)"`
				] : []),
			...(data.preCommands ? data.preCommands.map(cmd => `\t${resolve(cmd)}`) : []),
			...(data.command ?
				[
					`\tliblist -c $(BIN_LIB);\\`,
					`\tliblist -a $(LIBL);\\`,
					`\tsystem "${resolvedCommand}" > .logs/${ileObject.name.toLowerCase()}.splf` // TODO: write the spool file somewhere?
				]
				: []
			),
			...(data.postCommands ? data.postCommands.map(cmd => `\t${resolve(cmd)}`) : []),
			...(resolvedCommand.includes(`*EVENTF`) ? [`\tsystem "CPYTOSTMF FROMMBR('$(PREPATH)/EVFEVENT.FILE/${ileObject.name}.MBR') TOSTMF('.evfevent/${ileObject.name.toLowerCase()}.evfevent') DBFCCSID(*FILE) STMFCCSID(1208) STMFOPT(*REPLACE)"`] : []),
		);

		return lines;
	}
}

const binderSourceCompile: CompileData = {
	becomes: `SRVPGM`,
	preCommands: [
		`-system -q "CRTBNDDIR BNDDIR($(BIN_LIB)/$(APP_BNDDIR))"`,
		`-system -q "RMVBNDDIRE BNDDIR($(BIN_LIB)/$(APP_BNDDIR)) OBJ(($(BIN_LIB)/$*))"`,
		`-system "DLTOBJ OBJ($(BIN_LIB)/$*) OBJTYPE(*SRVPGM)"`
	],
	command: `CRTSRVPGM SRVPGM($(BIN_LIB)/$*) MODULE(*MODULES) SRCSTMF('$<') BNDDIR($(BNDDIR))`,
	postCommands: [
		`-system -q "ADDBNDDIRE BNDDIR($(BIN_LIB)/$(APP_BNDDIR)) OBJ((*LIBL/$* *SRVPGM *IMMED))"`
	]
};
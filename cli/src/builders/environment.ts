import { str } from "crc-32/crc32c";
import { ObjectType } from "../targets";
import path from "path";

// Always try and store parmId as lowercase
export type CommandParameters = { [parmId: string]: string };

export type CompileAttribute = { [ext: string]: CompileData };

export interface CompileData {
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

export interface Action {
  name: string;
  command: string;
  extensions?: string[];
  environment?: "ile";
}

export function getBranchLibraryName(currentBranch: string) {
  return `VS${(str(currentBranch, 0) >>> 0).toString(16).toUpperCase()}`;
}

export function getTrueBasename(name: string) {
  // Logic to handle second extension, caused by bob.
  const sourceObjectTypes = [`.PGM`, `.SRVPGM`, `.TEST`];
  const secondName = path.parse(name);
  if (secondName.ext && sourceObjectTypes.includes(secondName.ext.toUpperCase())) {
    name = secondName.name;
  }

  // Remove bob text convention
  if (name.includes(`-`)) {
    name = name.substring(0, name.indexOf(`-`));
  }

  return name;
}

export function getDefaultCompiles(): CompileAttribute {
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

  return {
    "pgm": {
      becomes: `PGM`,
      command: `CRTPGM`,
      parameters: {
        pgm: `$(BIN_LIB)/$*`,
        entmod: `$*`,
        module: `*MODULES`,
        tgtrls: `*CURRENT`,
        bnddir: `$(APP_BNDDIR)`,
        actgrp: `*NEW`
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
        bnddir: `$(APP_BNDDIR)`,
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
        compileopt: `TGTCCSID(*JOB) BNDDIR($(APP_BNDDIR)) DFTACTGRP(*no)`
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
      command: "CRTPRTF",
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
        bnddir: `$(APP_BNDDIR)`
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
  };
}
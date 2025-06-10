import { beforeAll, describe, expect, test } from 'vitest';

import { Targets } from '../src/targets'
import { MakeProject } from '../src/builders/make';
import { setupFixture } from './fixtures/projects';
import { ReadFileSystem } from '../src/readFileSystem';

describe(`pseudo tests`, () => {
  const project = setupFixture(`pseudo`);
  
  const fs = new ReadFileSystem();
  const targets = new Targets(project.cwd, fs);
  let make: MakeProject;

  beforeAll(async () => {
    project.setup();
    await targets.loadProject();

    expect(targets.getTargets().length).toBeGreaterThan(0);
    targets.resolveBinder();

    make = new MakeProject(project.cwd, targets, fs);
    await make.setupSettings();
  });

  test(`Test objects exists`, () => {
    expect(targets.searchForObject({ systemName: `MYTHING`, type: `DTAARA` })).toBeDefined();
    expect(targets.searchForObject({ systemName: `MSTDSP`, type: `FILE` })).toBeDefined();
  })

  test(`Program depends on DTAARA`, () => {
    const programTarget = targets.getTarget({ systemName: `TESTER`, type: `PGM` });

    expect(programTarget).toBeDefined();
    expect(programTarget.deps.length).toBe(1);
    expect(programTarget.deps[0].systemName).toBe(`MYTHING`);
  });

  test(`Ensure custom attributes are respected`, () => {
    const makefile = make.getMakefile();

    // Covers:
    // .ibmi.json -> tgtCcsid
    // Rules.mk rules
    const testerProgram = makefile.findIndex(l => l.startsWith(`$(PREPATH)/TESTER.PGM: qrpglesrc/tester.pgm.rpgle`));
    expect(testerProgram).toBeGreaterThan(-1);
    expect(makefile[testerProgram + 3]).toBe([
      `\tsystem "CRTBNDRPG PGM($(BIN_LIB)/TESTER) SRCSTMF('qrpglesrc/tester.pgm.rpgle') OPTION(*EVENTF) DBGVIEW(*SOURCE) TGTRLS(*CURRENT) TGTCCSID(273) BNDDIR(MYBND) DFTACTGRP(*NO) TEXT('My program')" > .logs/tester.splf || \\`,
      `\t(system "CPYTOSTMF FROMMBR('$(PREPATH)/EVFEVENT.FILE/TESTER.MBR') TOSTMF('.evfevent/tester.evfevent') DBFCCSID(*FILE) STMFCCSID(1208) STMFOPT(*REPLACE)"; $(SHELL) -c 'exit 1')`
    ].join('\n'));

    // Covers:
    // Rules.mk rules
    const theDtaara = makefile.findIndex(l => l.startsWith(`$(PREPATH)/MYTHING.DTAARA:`));
    expect(theDtaara).toBeGreaterThan(-1);
    expect(makefile[theDtaara + 1]).toBe(`\t-system -q "CRTDTAARA DTARA(MYTHING) TYPE(*CHAR) LEN(15) VALUE('HELLO') TEXT('Hello world')"`);
  });

  test(`Ensure TGTCCSID is updated in COMPILEOPT`, () => {
    const makefile = make.getMakefile();

    // Covers:
    // .ibmi.json -> tgtCcsid -> updating COMPILEOPT
    const testerProgram = makefile.findIndex(l => l.startsWith(`$(PREPATH)/OTHER.PGM: qrpglesrc/other.pgm.sqlrpgle`));
    expect(testerProgram).toBeGreaterThan(-1);
    expect(makefile[testerProgram + 3]).toBe([
      `\tsystem "CRTSQLRPGI OBJ($(BIN_LIB)/OTHER) SRCSTMF('qrpglesrc/other.pgm.sqlrpgle') COMMIT(*NONE) DBGVIEW(*SOURCE) OPTION(*EVENTF) RPGPPOPT(*LVL2) COMPILEOPT('TGTCCSID(273) BNDDIR($(BNDDIR)) DFTACTGRP(*no)')" > .logs/other.splf || \\`,
      `\t(system "CPYTOSTMF FROMMBR('$(PREPATH)/EVFEVENT.FILE/OTHER.MBR') TOSTMF('.evfevent/other.evfevent') DBFCCSID(*FILE) STMFCCSID(1208) STMFOPT(*REPLACE)"; $(SHELL) -c 'exit 1')`
    ].join('\n'));
  });

  test(`Ensure TGTCCSID is applied to CRTSRCPF CCSID`, () => {
    const makefile = make.getMakefile();

    // Covers:
    // .ibmi.json -> tgtCcsid -> CRTSRCPF CCSID
    const testerProgram = makefile.findIndex(l => l.startsWith(`$(PREPATH)/MSTDSP.FILE: qobjs/mstdsp.dspf`));
    expect(testerProgram).toBeGreaterThan(-1);
    expect(makefile[testerProgram + 1]).toBe(`\t-system -qi "CRTSRCPF FILE($(BIN_LIB)/QTMPSRC) RCDLEN(112) CCSID(37)"`);
  });
});


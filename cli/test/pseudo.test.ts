import { beforeAll, describe, expect, test } from 'vitest';

import { Targets } from '../src/targets'
import { MakeProject } from '../src/builders/make';
import { getFiles } from '../src/utils';
import { setupPseudo } from './fixtures/projects';
import { scanGlob } from '../src/extensions';

const cwd = setupPseudo();

const makeDefaults = MakeProject.getDefaultSettings();

let files = getFiles(cwd, scanGlob);

describe.skipIf(files.length === 0)(`psuedo tests`, () => {
  const targets = new Targets(cwd);
  let make: MakeProject;

  beforeAll(async () => {
    targets.loadObjectsFromPaths(files);
    const parsePromises = files.map(f => targets.parseFile(f));
    await Promise.all(parsePromises);

    expect(targets.getTargets().length).toBeGreaterThan(0);
    targets.resolveBinder();

    make = new MakeProject(cwd, targets);
  });

  test(`Test DTAARA exists`, () => {
    expect(targets.searchForObject({ systemName: `MYTHING`, type: `DTAARA` }, undefined)).toBeDefined();
  });

  test(`Program depends on DTAARA`, () => {
    const programTarget = targets.getTarget({ systemName: `TESTER`, type: `PGM` });

    expect(programTarget).toBeDefined();
    expect(programTarget.deps.length).toBe(1);
    expect(programTarget.deps[0].systemName).toBe(`MYTHING`);
  });

  test(`Ensure program doesn't include bad child dep`, () => {
    const programTarget = targets.getTarget({ systemName: `TESTER`, type: `PGM` });

    expect(programTarget).toBeDefined();
    expect(programTarget.deps.length).toBe(1);
    expect(programTarget.deps[0].systemName).toBe(`MYTHING`);
  });

  test(`Program compile CCSID is from config`, () => {
    const makefile = make.getMakefile();

    const testerProgram = makefile.findIndex(l => l.startsWith(`$(PREPATH)/TESTER.PGM: qrpglesrc/tester.pgm.rpgle`));
    expect(testerProgram).toBeGreaterThan(-1);
    expect(makefile[testerProgram + 3]).toBe(`\tsystem "CRTBNDRPG PGM($(BIN_LIB)/TESTER) SRCSTMF('qrpglesrc/tester.pgm.rpgle') OPTION(*EVENTF) DBGVIEW(*SOURCE) TGTRLS(*CURRENT) TGTCCSID(37) BNDDIR($(BNDDIR)) DFTACTGRP(*NO) TEXT('My program')" > .logs/tester.splf`);

    const theDtaara = makefile.findIndex(l => l.startsWith(`$(PREPATH)/MYTHING.DTAARA:`));
    expect(theDtaara).toBeGreaterThan(-1);
    expect(makefile[theDtaara + 1]).toBe(`\t-system -q "CRTDTAARA DTARA(MYTHING) TYPE(*CHAR) LEN(15) VALUE('HELLO') TEXT('Hello world')"`);
  });
});

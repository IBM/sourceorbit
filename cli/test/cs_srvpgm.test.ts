import { beforeAll, describe, expect, test } from 'vitest';

import { Targets } from '../src/targets'
import { MakeProject } from '../src/builders/make';
import { setupFixture } from './fixtures/projects';
import { ReadFileSystem } from '../src/readFileSystem';
import { BobProject } from '../src/builders/bob';

describe(`pseudo tests`, () => {
  const project = setupFixture(`cs_srvpgm`);
  
  const fs = new ReadFileSystem();
  const targets = new Targets(project.cwd, fs);
  let make: MakeProject;

  beforeAll(async () => {
    project.setup();
    await targets.loadProject();

    expect(targets.getTargets().length).toBeGreaterThan(0);
    targets.resolveBinder();

    make = new MakeProject(project.cwd, targets);
  });

  test(`That test files are understood`, () => {
    expect(targets).toBeDefined();

    const testModule = targets.getTarget({systemName: `EMPTEST`, type: `MODULE`});
    expect(testModule).toBeDefined();

    expect(testModule.deps.length).toBe(3);
    expect(testModule.deps.find(f => f.systemName === `EMPLOYEE`)).toBeDefined();
    expect(testModule.deps.find(f => f.systemName === `DEPARTMENT`)).toBeDefined();
    expect(testModule.deps.find(f => f.systemName === `EMPDET`)).toBeDefined();
  });

  test('Deps are picked up for the module', () => {
    const empdet = targets.getTarget({systemName: `EMPDET`, type: `MODULE`});
    expect(empdet).toBeDefined();

    expect(empdet.deps.length).toBe(2);
    expect(empdet.deps.find(f => f.systemName === `EMPLOYEE`)).toBeDefined();
    expect(empdet.deps.find(f => f.systemName === `DEPARTMENT`)).toBeDefined();

    const employees = targets.getTarget({systemName: `EMPLOYEES`, type: `PGM`});
    expect(employees).toBeDefined();

    expect(employees.deps.length).toBe(4);
    expect(employees.deps.find(f => f.systemName === `EMPLOYEES` && f.type === `MODULE`)).toBeDefined();
    expect(employees.deps.find(f => f.systemName === `EMPDET` && f.type === `MODULE`)).toBeDefined();
    expect(employees.deps.find(f => f.systemName === `EMPS` && f.type === `FILE`)).toBeDefined();
    expect(employees.deps.find(f => f.systemName === `EMPLOYEE` && f.type === `FILE`)).toBeDefined();
  });

  test('ibmi-bob rules', () => {
    const bobProject = new BobProject(targets);

    const files = bobProject.createRules();

    expect(files[`Rules.mk`]).toBeDefined();
    expect(files[`Rules.mk`]).toBe(`SUBDIRS = qddssrc qrpglesrc qtestsrc`);

    expect(files[`qtestsrc/Rules.mk`]).toBe(`EMPTEST.MODULE: emptest.test.sqlrpgle qrpgleref/empdet.rpgleinc EMPLOYEE.FILE DEPARTMENT.FILE EMPDET.MODULE`)

    console.log(files[`qrpglesrc/Rules.mk`]);
    expect(files[`qrpglesrc/Rules.mk`]).toContain(`EMPLOYEES.MODULE: employees.pgm.sqlrpgle qrpgleref/constants.rpgleinc qrpgleref/empdet.rpgleinc`);
    expect(files[`qrpglesrc/Rules.mk`]).toContain(`EMPLOYEES.PGM: EMPLOYEE.FILE EMPS.FILE EMPDET.MODULE EMPLOYEES.MODULE`);
  });

  test('makefile', () => {
    const makefile = new MakeProject(targets.getCwd(), targets);

    const contents = makefile.getMakefile().join(`\n`);

    expect(contents).toContain(`$(PREPATH)/EMPLOYEES.PGM:`);
    expect(contents).toContain(`system "CRTPGM PGM($(BIN_LIB)/EMPLOYEES) ENTMOD(EMPLOYEES) MODULE(EMPDET EMPLOYEES) TGTRLS(*CURRENT) BNDDIR($(BNDDIR)) ACTGRP(*NEW)" > .logs/employees.splf`);
  });
});

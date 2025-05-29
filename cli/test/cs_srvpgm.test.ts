import { beforeAll, describe, expect, test } from 'vitest';

import { Targets } from '../src/targets'
import { MakeProject } from '../src/builders/make';
import { setupFixture } from './fixtures/projects';
import { ReadFileSystem } from '../src/readFileSystem';
import { BobProject } from '../src/builders/bob';
import path from 'path';

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
    expect(targets.binderRequired()).toBeFalsy();

    const testModule = targets.getTarget({systemName: `TEMPTEST`, type: `MODULE`});
    expect(testModule).toBeDefined();

    expect(testModule.deps.length).toBe(3);
    expect(testModule.deps.find(f => f.systemName === `EMPLOYEE`)).toBeDefined();
    expect(testModule.deps.find(f => f.systemName === `DEPARTMENT`)).toBeDefined();
    expect(testModule.deps.find(f => f.systemName === `EMPDET` && f.type === `MODULE`)).toBeDefined();
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

    expect(files[path.join(`qtestsrc`, `Rules.mk`)]).toBe(`TEMPTEST.MODULE: emptest.test.sqlrpgle qrpgleref/empdet.rpgleinc EMPLOYEE.FILE DEPARTMENT.FILE EMPDET.MODULE`)
    
    expect(files[path.join(`qrpglesrc`, `Rules.mk`)]).toContain(`EMPLOYEES.MODULE: employees.pgm.sqlrpgle qrpgleref/constants.rpgleinc qrpgleref/empdet.rpgleinc`);
    expect(files[path.join(`qrpglesrc`, `Rules.mk`)]).toContain(`EMPLOYEES.PGM: EMPLOYEE.FILE EMPS.FILE EMPDET.MODULE EMPLOYEES.MODULE`);
    expect(files[path.join(`qrpglesrc`, `Rules.mk`)]).not.toContain(`EMPDET.SRVPGM`); // Ensure no service program is created
  });

  test('makefile', () => {
    const makefile = new MakeProject(targets.getCwd(), targets);

    const contents = makefile.getMakefile().join(`\n`);

    expect(contents).toContain(`$(PREPATH)/EMPLOYEES.PGM:`);
    expect(contents).toContain(`system "CRTPGM PGM($(BIN_LIB)/EMPLOYEES) ENTMOD(EMPLOYEES) MODULE(EMPDET EMPLOYEES) TGTRLS(*CURRENT) BNDDIR($(BNDDIR)) ACTGRP(*NEW)" > .logs/employees.splf`);

    expect(contents).not.toContain(`EMPDET.SRVPGM`); // Ensure no service program is created
    expect(contents).toContain(`EMPDET.MODULE`);
  });
});

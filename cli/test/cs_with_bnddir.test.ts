import { beforeAll, describe, expect, test } from 'vitest';

import { Targets } from '../src/targets'
import { MakeProject } from '../src/builders/make';
import { setupFixture } from './fixtures/projects';
import { ReadFileSystem } from '../src/readFileSystem';
import { BobProject } from '../src/builders/bob';

describe(`pseudo tests`, () => {
  const project = setupFixture(`cs_with_bnddir`);
  
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
    expect(targets.binderRequired()).toBeTruthy();

    const testModule = targets.getTarget({systemName: `EMPDETT`, type: `MODULE`});
    expect(testModule).toBeDefined();

    expect(testModule.deps.length).toBe(3);
    expect(testModule.deps.find(f => f.systemName === `EMPLOYEE`)).toBeDefined();
    expect(testModule.deps.find(f => f.systemName === `DEPARTMENT`)).toBeDefined();
    expect(testModule.deps.find(f => f.systemName === `EMPDET` && f.type === `SRVPGM`)).toBeDefined();
  });

  test('Deps are picked up for the module', () => {
    const empdet = targets.getTarget({systemName: `EMPDET`, type: `MODULE`});
    expect(empdet).toBeDefined();

    expect(empdet.deps.length).toBe(2);
    expect(empdet.deps.find(f => f.systemName === `EMPLOYEE`)).toBeDefined();
    expect(empdet.deps.find(f => f.systemName === `DEPARTMENT`)).toBeDefined();

    const employees = targets.getTarget({systemName: `EMPLOYEES`, type: `PGM`});
    expect(employees).toBeDefined();

    expect(employees.deps.length).toBe(3);
    expect(employees.deps.find(f => f.systemName === `EMPDET` && f.type === `SRVPGM`)).toBeDefined();
    expect(employees.deps.find(f => f.systemName === `EMPS` && f.type === `FILE`)).toBeDefined();
    expect(employees.deps.find(f => f.systemName === `EMPLOYEE` && f.type === `FILE`)).toBeDefined();
  });

  test('ibmi-bob rules', () => {
    const bobProject = new BobProject(targets);

    const files = bobProject.createRules();
    console.log(files[`qrpglesrc/Rules.mk`]);

    expect(files[`Rules.mk`]).toBeDefined();
    expect(files[`Rules.mk`]).toBe(`SUBDIRS = qddssrc qrpglesrc qsqlsrc qtestsrc`);

    expect(files[`qtestsrc/Rules.mk`]).toBe(`EMPDETT.MODULE: empdett.test.sqlrpgle qrpgleref/empdet.rpgleinc EMPLOYEE.FILE DEPARTMENT.FILE APP.BNDDIR`)

    expect(files[`qrpglesrc/Rules.mk`]).toContain(`EMPDET.MODULE: empdet.sqlrpgle qrpgleref/empdet.rpgleinc EMPLOYEE.FILE DEPARTMENT.FILE`);
    expect(files[`qrpglesrc/Rules.mk`]).toContain(`EMPDET.SRVPGM: empdet.bnd EMPDET.MODULE`); 
    expect(files[`qrpglesrc/Rules.mk`]).toContain(`EMPLOYEES.PGM: employees.pgm.sqlrpgle qrpgleref/constants.rpgleinc qrpgleref/empdet.rpgleinc EMPLOYEE.FILE EMPS.FILE APP.BNDDIR`);
    expect(files[`qrpglesrc/Rules.mk`]).toContain(`APP.BNDDIR: app.bnddir EMPDET.SRVPGM`);
  });
});

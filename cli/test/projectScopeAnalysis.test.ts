import { assert, beforeAll, describe, expect, test } from 'vitest';

import { Targets } from '../src/targets'
import path from 'path';
import { MakeProject } from '../src/builders/make';
import { getFiles } from '../src/utils';
import { setupCompanySystem, setupSqlReferencesSystem } from './fixtures/projects';
import { scanGlob } from '../src/extensions';
import { writeFileSync } from 'fs';

const cwd = setupCompanySystem();

const makeDefaults = MakeProject.getDefaultSettings();

let files = getFiles(cwd, scanGlob);

describe.skipIf(files.length === 0)(`sql_reference tests (internal scope analysis)`, () => {
  const targets = new Targets(cwd);
  
  beforeAll(async () => {
    targets.setAnalysisForScopes(true);
    targets.loadObjectsFromPaths(files);
    const parsePromises = files.map(f => targets.parseFile(f));
    await Promise.all(parsePromises);

    expect(targets.getTargets().length).toBeGreaterThan(0);
    targets.resolveBinder();
  });

  test(`Check employees (with internal scope analisys)`, async () => {
    const myPgm = targets.getTarget({systemName: `EMPLOYEES`, type: `PGM`});
    expect(myPgm.relativePath).toBe(path.join(`qrpglesrc`, `employees.pgm.sqlrpgle`));
    expect(myPgm.deps.length).toBe(3);

    const empTable = myPgm.deps[0];
    expect(empTable.systemName).toBe(`EMPLOYEE`);
    expect(empTable.type).toBe(`FILE`);
    expect(empTable.relativePath).toBe(path.join(`qddssrc`, `employee.table`));

    const empDisplay = myPgm.deps[1];
    expect(empDisplay.systemName).toBe(`EMPS`);
    expect(empDisplay.type).toBe(`FILE`);
    expect(empDisplay.relativePath).toBe(path.join(`qddssrc`, `emps.dspf`));
  });

  test(`Check depts (with internal scope analisys)`, async () => {
    const myPgm = targets.getTarget({systemName: `DEPTS`, type: `PGM`});
    expect(myPgm.relativePath).toBe(path.join(`qrpglesrc`, `depts.pgm.sqlrpgle`));
    expect(myPgm.text).toBe(`This is the text for this program`);

    expect(myPgm.deps.length).toBe(5);

    const empPgm = myPgm.deps[0];
    expect(empPgm.systemName).toBe(`EMPLOYEES`);
    expect(empPgm.type).toBe(`PGM`);
    expect(empPgm.relativePath).toBe(path.join(`qrpglesrc`, `employees.pgm.sqlrpgle`));

    const deptTable = myPgm.deps[1];
    expect(deptTable.systemName).toBe(`DEPARTMENT`);
    expect(deptTable.type).toBe(`FILE`);
    expect(deptTable.relativePath).toBe(path.join(`qddssrc`, `department.table`));

    const deptFile = myPgm.deps[2];
    expect(deptFile.systemName).toBe(`DEPTS`);
    expect(deptFile.type).toBe(`FILE`);
    expect(deptFile.relativePath).toBe(path.join(`qddssrc`, `depts.dspf`));

    // second time it appear
    const deptTable2 = myPgm.deps[3];
    expect(deptTable2.systemName).toBe(`DEPARTMENT`);
    expect(deptTable2.type).toBe(`FILE`);
    expect(deptTable2.relativePath).toBe(path.join(`qddssrc`, `department.table`));

    const utilsSrvPgm = myPgm.deps[4];
    expect(utilsSrvPgm.systemName).toBe(`UTILS`);
    expect(utilsSrvPgm.type).toBe(`SRVPGM`);
    expect(utilsSrvPgm.relativePath).toBe(path.join(`qsrvsrc`, `utils.bnd`));
  });


  test(`Double resolve binder test`, async () => {
    targets.resolveBinder();
    let deptsPgm = targets.getTarget({systemName: `DEPTS`, type: `PGM`});

    expect(deptsPgm.deps.length).toBe(5);

    targets.resolveBinder();
    deptsPgm = targets.getTarget({systemName: `DEPTS`, type: `PGM`});

    expect(deptsPgm.deps.length).toBe(5);
  });

});
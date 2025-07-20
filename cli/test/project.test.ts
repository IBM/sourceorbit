import { assert, beforeAll, describe, expect, test } from 'vitest';

import { Targets } from '../src/targets'
import path from 'path';
import { MakeProject } from '../src/builders/make';
import { setupFixture } from './fixtures/projects';
import { writeFileSync } from 'fs';
import { getDefaultCompiles } from '../src/builders/environment';
import { ReadFileSystem } from '../src/readFileSystem';

const compileDefaults = getDefaultCompiles();

describe(`company_system tests`, () => {
  const project = setupFixture(`company_system`);

  const fs = new ReadFileSystem();
  const targets: Targets = new Targets(project.cwd, fs);
  
  beforeAll(async () => {
    project.setup();
    await targets.loadProject();

    expect(targets.getTargets().length).toBeGreaterThan(0);
    targets.resolveBinder();
  });

  test(`Check objects are generated`, async () => {
    expect(targets.getResolvedObjects().length).toBe(14);
    expect(targets.getTargets().length).toBe(15);
    expect(targets.getTargetsOfType(`FILE`).length).toBe(4);
    expect(targets.getTargetsOfType(`PGM`).length).toBe(4);
    expect(targets.getTargetsOfType(`MODULE`).length).toBe(2);
    expect(targets.getTargetsOfType(`SRVPGM`).length).toBe(4);
  });

  test(`Check mypgm`, async () => {
    const myPgm = targets.getTarget({systemName: `MYPGM`, type: `PGM`});
    expect(myPgm.relativePath).toBe(path.join(`qrpglesrc`, `mypgm.pgm.rpgle`));
    expect(myPgm.deps.length).toBe(0);
  });

  test(`Check employees`, async () => {
    const myPgm = targets.getTarget({systemName: `EMPLOYEES`, type: `PGM`});
    expect(myPgm.relativePath).toBe(path.join(`qrpglesrc`, `employees.pgm.sqlrpgle`));

    expect(myPgm.deps.length).toBe(2);

    const empTable = myPgm.deps[0];
    expect(empTable.systemName).toBe(`EMPLOYEE`);
    expect(empTable.type).toBe(`FILE`);
    expect(empTable.relativePath).toBe(path.join(`qddssrc`, `employee.table`));

    const empDisplay = myPgm.deps[1];
    expect(empDisplay.systemName).toBe(`EMPS`);
    expect(empDisplay.type).toBe(`FILE`);
    expect(empDisplay.relativePath).toBe(path.join(`qddssrc`, `emps.dspf`));
  });

  test(`Check depts`, async () => {
    const myPgm = targets.getTarget({systemName: `DEPTS`, type: `PGM`});
    expect(myPgm.relativePath).toBe(path.join(`qrpglesrc`, `depts.pgm.sqlrpgle`));
    expect(myPgm.text).toBe(`This is the text for this program`);

    expect(myPgm.deps.length).toBe(4);

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

    const utilsSrvPgm = myPgm.deps[3];
    expect(utilsSrvPgm.systemName).toBe(`UTILS`);
    expect(utilsSrvPgm.type).toBe(`SRVPGM`);
    expect(utilsSrvPgm.relativePath).toBe(path.join(`qsrvsrc`, `utils.bnd`));

    expect(myPgm.headers).toBeDefined();
    expect(myPgm.headers.length).toBe(2);
  });

  test(`Check utils`, async () => {
    const myPgm = targets.getTarget({systemName: `UTILS`, type: `SRVPGM`});
    expect(myPgm.relativePath).toBe(path.join(`qsrvsrc`, `utils.bnd`));

    expect(myPgm.deps.length).toBe(1);

    const empPgm = myPgm.deps[0];
    expect(empPgm.systemName).toBe(`UTILS`);
    expect(empPgm.type).toBe(`MODULE`);
    expect(empPgm.relativePath).toBe(path.join(`qrpglesrc`, `utils.sqlrpgle`));
  });

  test(`Check getDouble`, async () => {
    const theObj = targets.getTarget({systemName: `GETDOUBLE`, type: `SRVPGM`});
    expect(theObj.relativePath).toBe(path.join(`qsqlsrc`, `getDouble.sql`));

    expect(theObj.deps.length).toBe(1);

    const dep = theObj.deps[0];
    expect(dep.systemName).toBe(`BANKING`);
    expect(dep.type).toBe(`SRVPGM`);
    expect(dep.relativePath).toBe(path.join(`qsrvsrc`, `banking.bnd`));

    const logs = targets.logger.getLogsFor(theObj.relativePath);
    expect(logs.length).toBe(1);
    expect(logs[0].message).toBe(`Extension should be based on type. Suggested name is 'getdouble.sqludf'`);
    expect(logs[0].type).toBe(`warning`);
  });

  test(`Check binding directory`, async () => {
    const myBinder = targets.getTarget({systemName: `$(APP_BNDDIR)`, type: `BNDDIR`});
    expect(myBinder.relativePath).toBeUndefined();

    expect(myBinder.deps.length).toBe(2);

    const bankingSrvpgm = myBinder.deps.find(d => d.systemName === `BANKING`);
    expect(bankingSrvpgm.systemName).toBe(`BANKING`);
    expect(bankingSrvpgm.type).toBe(`SRVPGM`);
    expect(bankingSrvpgm.relativePath).toBe(path.join(`qsrvsrc`, `banking.bnd`));

    const utilsSrvpgm = myBinder.deps.find(d => d.systemName === `UTILS`);
    expect(utilsSrvpgm.systemName).toBe(`UTILS`);
    expect(utilsSrvpgm.type).toBe(`SRVPGM`);
    expect(utilsSrvpgm.relativePath).toBe(path.join(`qsrvsrc`, `utils.bnd`));
  });

  test(`Check employee table`, async () => {
    const empTable = targets.getTarget({systemName: `EMPLOYEE`, type: `FILE`});
    expect(empTable.relativePath).toBe(path.join(`qddssrc`, `employee.table`));
    expect(empTable.text).toBe(`Employee File`);
    
  });

  test(`Testing removing and adding an object`, async () => {
    // First, let's delete the object internally
    let deptsPgm = targets.getTarget({systemName: `DEPTS`, type: `PGM`});
    let deptsFile = targets.getTarget({systemName: `DEPTS`, type: `FILE`});

    const deptsFilePath = path.join(project.cwd, deptsFile.relativePath);
    const deptsPgmPath = path.join(project.cwd, deptsPgm.relativePath);

    targets.logger.flush(deptsFile.relativePath);

    // We removed the DEPTS display file, used by DEPTS program
    const impacted = targets.removeObjectByPath(path.join(project.cwd, deptsFile.relativePath));
    expect(impacted.length).toBe(1);
    expect(impacted[0].systemName).toBe(`DEPTS`);
    expect(impacted[0].type).toBe(`PGM`);
    
    const logs = targets.logger.getLogsFor(deptsPgm.relativePath);
    expect(logs.length).toBe(3);
    expect(logs[0].message).toBe(`Include at line 13 found, to path 'qrpgleref/constants.rpgleinc'`);
    expect(logs[1].message).toBe(`Include at line 14 found, to path 'qrpgleref/utils.rpgleinc'`);
    expect(logs[2].message).toBe(`This object depended on DEPTS.FILE before it was deleted.`);

    expect(targets.getTarget({systemName: `DEPTS`, type: `FILE`})).toBeUndefined();

    targets.resolveBinder();

    // Now let's add it back

    await targets.parseFile(deptsFilePath);

    deptsFile = targets.getTarget({systemName: `DEPTS`, type: `FILE`});
    expect(deptsFile).toBeDefined();

    // Just because we re-handle the path, doesn't mean it's picked up again in other places where it was before
    expect(deptsPgm.deps.find(d => d.systemName === `DEPTS` && d.type === `FILE`)).toBeUndefined();

    await targets.parseFile(deptsPgmPath);
    // We have to fetch the dep again because the old reference is lost since we parsed again
    deptsPgm = targets.getTarget({systemName: `DEPTS`, type: `PGM`});
    expect(deptsPgm.deps.find(d => d.systemName === `DEPTS` && d.type === `FILE`)).toBeDefined();
  });

  test(`Double resolve binder test`, async () => {
    targets.resolveBinder();
    let deptsPgm = targets.getTarget({systemName: `DEPTS`, type: `PGM`});

    expect(deptsPgm.deps.length).toBe(4);

    targets.resolveBinder();
    deptsPgm = targets.getTarget({systemName: `DEPTS`, type: `PGM`});

    expect(deptsPgm.deps.length).toBe(4);
  });

  test(`Check mypgm RPGLE target`, async () => {
    const myPgm = targets.getTarget({systemName: `MYPGM`, type: `PGM`});
    const lines = MakeProject.generateSpecificTarget(compileDefaults[`pgm.rpgle`], myPgm);

    expect(lines.join()).toBe([
      '$(PREPATH)/MYPGM.PGM: qrpglesrc/mypgm.pgm.rpgle',
      '\tliblist -c $(BIN_LIB);\\',
      '\tliblist -a $(LIBL);\\',
      [
        `\tsystem "CRTBNDRPG PGM($(BIN_LIB)/MYPGM) SRCSTMF('qrpglesrc/mypgm.pgm.rpgle') OPTION(*EVENTF) DBGVIEW(*SOURCE) TGTRLS(*CURRENT) TGTCCSID(*JOB) BNDDIR($(APP_BNDDIR)) DFTACTGRP(*NO)" > .logs/mypgm.splf || \\`,
        `\t(system "CPYTOSTMF FROMMBR(\'$(PREPATH)/EVFEVENT.FILE/MYPGM.MBR\') TOSTMF(\'.evfevent/mypgm.evfevent\') DBFCCSID(*FILE) STMFCCSID(1208) STMFOPT(*REPLACE)"; $(SHELL) -c 'exit 1')`,
      ].join('\n')
    ].join());
  });

  test(`Check depts SQLRPGLE target (with CHGATR)`, async () => {
    const myPgm = targets.getTarget({systemName: `DEPTS`, type: `PGM`});
    const lines = MakeProject.generateSpecificTarget(compileDefaults[`pgm.sqlrpgle`], myPgm);

    expect(lines.join()).toBe([
      '$(PREPATH)/DEPTS.PGM: qrpglesrc/depts.pgm.sqlrpgle',
      '\tliblist -c $(BIN_LIB);\\',
      '\tliblist -a $(LIBL);\\',
      [
        `\tsystem "CRTSQLRPGI OBJ($(BIN_LIB)/DEPTS) SRCSTMF('qrpglesrc/depts.pgm.sqlrpgle') COMMIT(*NONE) DBGVIEW(*SOURCE) OPTION(*EVENTF) RPGPPOPT(*LVL2) COMPILEOPT('TGTCCSID(*JOB) BNDDIR($(APP_BNDDIR)) DFTACTGRP(*no)')" > .logs/depts.splf || \\`,
        `\t(system "CPYTOSTMF FROMMBR('$(PREPATH)/EVFEVENT.FILE/DEPTS.MBR') TOSTMF('.evfevent/depts.evfevent') DBFCCSID(*FILE) STMFCCSID(1208) STMFOPT(*REPLACE)"; $(SHELL) -c 'exit 1')`,
      ].join('\n')
    ].join());
  });

  test(`Check depts DSPF target (member)`, async () => {
    const myPgm = targets.getTarget({systemName: `DEPTS`, type: `FILE`});
    const lines = MakeProject.generateSpecificTarget(compileDefaults[`dspf`], myPgm);

    expect(lines.join()).toBe([
      '$(PREPATH)/DEPTS.FILE: qddssrc/depts.dspf',
      '\t-system -qi "CRTSRCPF FILE($(BIN_LIB)/QTMPSRC) RCDLEN(112) CCSID(*JOB)"',
      `\tsystem "CPYFRMSTMF FROMSTMF('qddssrc/depts.dspf') TOMBR('$(PREPATH)/QTMPSRC.FILE/DEPTS.MBR') MBROPT(*REPLACE)"`,
      '\tliblist -c $(BIN_LIB);\\',
      '\tliblist -a $(LIBL);\\',
      [
        `\tsystem "CRTDSPF FILE($(BIN_LIB)/DEPTS) SRCFILE($(BIN_LIB)/QTMPSRC) SRCMBR(DEPTS) OPTION(*EVENTF)" > .logs/depts.splf || \\`,
        `\t(system "CPYTOSTMF FROMMBR(\'$(PREPATH)/EVFEVENT.FILE/DEPTS.MBR\') TOSTMF(\'.evfevent/depts.evfevent\') DBFCCSID(*FILE) STMFCCSID(1208) STMFOPT(*REPLACE)"; $(SHELL) -c 'exit 1')`,
      ].join('\n')
    ].join());
  });

  test(`Check utils SRVPGM target (from binder source, *MODULES variable)`, async () => {
    const myPgm = targets.getTarget({systemName: `UTILS`, type: `SRVPGM`});
    const lines = MakeProject.generateSpecificTarget(compileDefaults[`bnd`], myPgm);

    expect(lines.join()).toBe([
      '$(PREPATH)/UTILS.SRVPGM: qsrvsrc/utils.bnd',
      '\t-system -q "CRTBNDDIR BNDDIR($(BIN_LIB)/$(APP_BNDDIR))"',
      '\tliblist -c $(BIN_LIB);\\',
      '\tliblist -a $(LIBL);\\',
      `\tsystem "CRTSRVPGM SRVPGM($(BIN_LIB)/UTILS) MODULE(UTILS) SRCSTMF('qsrvsrc/utils.bnd') BNDDIR($(BNDDIR)) REPLACE(*YES)" > .logs/utils.splf`,
      '\t-system -q "ADDBNDDIRE BNDDIR($(BIN_LIB)/$(APP_BNDDIR)) OBJ((*LIBL/UTILS *SRVPGM *IMMED))"',
    ].join());
  });

  test(`Check banking SRVPGM target (no binder source)`, async () => {
    const myPgm = targets.getTarget({systemName: `BANKING`, type: `SRVPGM`});
    const lines = MakeProject.generateSpecificTarget(compileDefaults[`srvpgm`], myPgm);

    expect(lines.join()).toBe([
      '$(PREPATH)/BANKING.SRVPGM: qsrvsrc/banking.bnd',
      '\t-system -q "CRTBNDDIR BNDDIR($(BIN_LIB)/$(APP_BNDDIR))"',
      '\t-system -q "RMVBNDDIRE BNDDIR($(BIN_LIB)/$(APP_BNDDIR)) OBJ(($(BIN_LIB)/BANKING))"',
      '\t-system "DLTOBJ OBJ($(BIN_LIB)/BANKING) OBJTYPE(*SRVPGM)"',
      '\tliblist -c $(BIN_LIB);\\',
      '\tliblist -a $(LIBL);\\',
      '\tsystem "CRTSRVPGM SRVPGM($(BIN_LIB)/BANKING) MODULE(BANKING) SRCSTMF(\'qsrvsrc/banking.bnd\') BNDDIR($(APP_BNDDIR))" > .logs/banking.splf',
      '\t-system -q "ADDBNDDIRE BNDDIR($(BIN_LIB)/$(APP_BNDDIR)) OBJ((*LIBL/BANKING *SRVPGM *IMMED))"',
    ].join());
  });

  test(`Checking makefile rule generation`, async () => {
    const makeProject = new MakeProject(project.cwd, targets, fs);
    await makeProject.setupSettings();

    const headerContent = makeProject.generateGenericRules();

    expect(headerContent.find(l => l === `$(PREPATH)/DEPTS.PGM: qrpglesrc/depts.pgm.sqlrpgle`)).toBeDefined();
    expect(headerContent.find(l => l === `$(PREPATH)/BANKING.MODULE: qrpglesrc/banking.sqlrpgle`)).toBeDefined();
    expect(headerContent.find(l => l === `$(PREPATH)/BANKING.SRVPGM: qsrvsrc/banking.bnd`)).toBeDefined();
    expect(headerContent.find(l => l === `$(PREPATH)/DEPARTMENT.FILE: qddssrc/department.table`)).toBeDefined();
  });

  test(`Makefile targets for all`, async () => {
    const makeProject = new MakeProject(project.cwd, targets, fs);
    await makeProject.setupSettings();

    const header = makeProject.generateHeader();
    expect(header).toContain(`APP_BNDDIR=$(APP_BNDDIR)`);

    // Generate targets on it's own will have BNDDIR, PGM, etc
    const targetContent = makeProject.generateTargets();

    const allTarget = targetContent.find(l => l.startsWith(`all:`));
    expect(allTarget).toBeDefined();

    expect(allTarget).toContain(`all: .logs .evfevent library`);
    // The order cannot be guaranteed, so we just check for the presence of the targets
    expect(allTarget).toContain(`$(PREPATH)/$(APP_BNDDIR).BNDDIR`);
    expect(allTarget).toContain(`$(PREPATH)/MYPGM.PGM`);
    expect(allTarget).toContain(`$(PREPATH)/DEPTS.PGM`);
    expect(allTarget).toContain(`$(PREPATH)/EMPLOYEES.PGM`);
  });

  test(`Makefile targets for partial build (DEPTS display file)`, async () => {
    const makeProject = new MakeProject(project.cwd, targets, fs);
    await makeProject.setupSettings();

    const deptsFile = targets.getTarget({systemName: `DEPTS`, type: `FILE`});

    // Generate targets on it's own will have BNDDIR, PGM, etc
    const headerContent = makeProject.generateTargets([deptsFile]);

    const allTarget = headerContent.find(l => l.startsWith(`all:`));
    expect(allTarget).toBeDefined();

    expect(allTarget).toBe(`all: .logs .evfevent library $(PREPATH)/DEPTS.FILE $(PREPATH)/DEPTS.PGM`);
  });

  test(`Makefile targets for partial build (EMPLOYEE table)`, async () => {
    const makeProject = new MakeProject(project.cwd, targets, fs);
    await makeProject.setupSettings();

    const deptsFile = targets.getTarget({systemName: `EMPLOYEE`, type: `FILE`});

    // Generate targets on it's own will have BNDDIR, PGM, etc
    const headerContent = makeProject.generateTargets([deptsFile]);

    const allTarget = headerContent.find(l => l.startsWith(`all:`));
    expect(allTarget).toBeDefined();

    const allTargets = allTarget.substring(5).split(` `);
    expect(allTargets.length).toBe(8);
    expect(allTargets[0]).toBe(`.logs`);
    expect(allTargets[1]).toBe(`.evfevent`);
    expect(allTargets[2]).toBe(`library`);
    expect(allTargets).toContain(`$(PREPATH)/EMPLOYEE.FILE`);
    expect(allTargets).toContain(`$(PREPATH)/EMPLOYEES.PGM`);
    expect(allTargets).toContain(`$(PREPATH)/DEPTS.PGM`);
    expect(allTargets).toContain(`$(PREPATH)/SHOWEMPS.PGM`);
    expect(allTargets).toContain(`$(PREPATH)/GETTOTSAL.SRVPGM`);
    
    const deptsTargetDeps = headerContent.find(l => l.startsWith(`$(PREPATH)/DEPTS.PGM:`));
    expect(deptsTargetDeps).toBeDefined();

    expect(deptsTargetDeps).toContain(`$(PREPATH)/DEPARTMENT.FILE`);
  });

  test(`Makefile targets for partial build (EMPLOYEE table) without children`, async () => {
    const makeProject = new MakeProject(project.cwd, targets, fs);
    await makeProject.setupSettings();

    makeProject.setNoChildrenInBuild(true);

    const deptsFile = targets.getTarget({systemName: `EMPLOYEE`, type: `FILE`});

    // Generate targets on it's own will have BNDDIR, PGM, etc
    const headerContent = makeProject.generateTargets([deptsFile]);

    const allTarget = headerContent.find(l => l.startsWith(`all: `));
    expect(allTarget).toBeDefined();

    const allTargets = allTarget.substring(5).split(` `);
    expect(allTargets.length).toBe(8);
    expect(allTargets[0]).toBe(`.logs`);
    expect(allTargets[1]).toBe(`.evfevent`);
    expect(allTargets[2]).toBe(`library`);
    expect(allTargets).toContain(`$(PREPATH)/EMPLOYEE.FILE`);
    expect(allTargets).toContain(`$(PREPATH)/EMPLOYEES.PGM`);
    expect(allTargets).toContain(`$(PREPATH)/DEPTS.PGM`);
    expect(allTargets).toContain(`$(PREPATH)/SHOWEMPS.PGM`);
    expect(allTargets).toContain(`$(PREPATH)/GETTOTSAL.SRVPGM`);
    
    const deptsTargetDeps = headerContent.find(l => l.startsWith(`$(PREPATH)/DEPTS.PGM:`));
    expect(deptsTargetDeps).toBeUndefined();
  });

  test(`Impact of EMPLOYEES`, () => {
    const empPgm = targets.getTarget({systemName: `EMPLOYEES`, type: `PGM`});
    expect(empPgm.relativePath).toBe(path.join(`qrpglesrc`, `employees.pgm.sqlrpgle`));

    const impactTree = targets.getImpactFor(empPgm);
    expect(impactTree.ileObject.systemName).toBe(`EMPLOYEES`);
    expect(impactTree.children.length).toBe(2);

    // Because DEPTS calls the EMPLOYEES program, so if EMPLOYEES changes, DEPTS needs a rebuild
    expect(impactTree.children.some(o => o.ileObject.systemName)).toBeTruthy();

    // Because SHOWEMPS calls the EMPLOYEES program, so if EMPLOYEES changes, SHOWEMPS needs a rebuild
    expect(impactTree.children.some(o => o.ileObject.systemName)).toBeTruthy();
  });

  test(`Impact of UTILS`, () => {
    const utilsModule = targets.getTarget({systemName: `UTILS`, type: `MODULE`});
    expect(utilsModule.relativePath).toBe(path.join(`qrpglesrc`, `utils.sqlrpgle`));

    const impactTree = targets.getImpactFor(utilsModule);
    expect(impactTree.ileObject.systemName).toBe(`UTILS`);
    expect(impactTree.children.length).toBe(1);

    // Because DEPTS calls the EMPLOYEES program, so if EMPLOYEES changes, DEPTS needs a rebuild
    const utilsSrvPgm = impactTree.children[0];
    expect(utilsSrvPgm.ileObject.systemName).toBe(`UTILS`);
    expect(utilsSrvPgm.ileObject.systemName).toBe(`UTILS`);
    expect(utilsSrvPgm.children.length).toBe(2);

    const srvChildren = utilsSrvPgm.children;
    expect(srvChildren[0].ileObject.systemName).toBe(`DEPTS`);
    expect(srvChildren[0].ileObject.type).toBe(`PGM`);

    expect(srvChildren[1].ileObject.systemName).toBe(`$(APP_BNDDIR)`);
    expect(srvChildren[1].ileObject.type).toBe(`BNDDIR`);
  });

  test(`Resolve SQL object by path`, () => {
    // We have a test for this as SQL objects are created a little different
    // from regular objects.

    const filePath = path.join(targets.getCwd(), `qddssrc`, `employee.table`);
    const resolvedObject = targets.getResolvedObject(filePath);

    expect(resolvedObject).toBeDefined();
    expect(resolvedObject.systemName).toBe(`EMPLOYEE`);
    expect(resolvedObject.type).toBe(`FILE`);
  })

  test(`Test function references`, () => {
    // We have a test for this as SQL objects are created a little different
    // from regular objects.

    const resolvedObject = targets.getTarget({systemName: `GETTOTSAL`, type: `SRVPGM`});
    expect(resolvedObject.relativePath).toBe(path.join(`qsqlsrc`, `getTotalSalary.sqludf`));

    expect(resolvedObject).toBeDefined();
    expect(resolvedObject.systemName).toBe(`GETTOTSAL`);
    expect(resolvedObject.longName).toBe(`getTotalSalary`);
    expect(resolvedObject.type).toBe(`SRVPGM`);

    expect(resolvedObject.deps.length).toBe(1);
    expect(resolvedObject.deps[0].systemName).toBe(`EMPLOYEE`);

    const logs = targets.logger.getLogsFor(resolvedObject.relativePath);
    expect(logs.length).toBe(0);
    // expect(logs[0].message).toBe(`Extension should be based on type. Suggested name is 'getTotalSalary.sqludf'`);
    // expect(logs[0].type).toBe(`warning`);

    const functionMake = MakeProject.generateSpecificTarget(compileDefaults[`sqludf`], resolvedObject);
    expect(functionMake.length).toBe(4);
    expect(functionMake[0]).toBe(`$(PREPATH)/GETTOTSAL.SRVPGM: qsqlsrc/getTotalSalary.sqludf`);
    expect(functionMake[3]).toBe(`\tsystem "RUNSQLSTM SRCSTMF('qsqlsrc/getTotalSalary.sqludf') COMMIT(*NONE)" > .logs/gettotsal.splf`);
  })

  test(`Generate makefile`, async () => {
    const makeProj = new MakeProject(project.cwd, targets, fs);
    await makeProj.setupSettings();

    writeFileSync(path.join(project.cwd, `makefile`), makeProj.getMakefile().join(`\n`));
  });
});
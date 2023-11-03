import { assert, beforeAll, describe, expect, test } from 'vitest';

import glob from "glob";
import { Targets } from '../src/targets'
import path from 'path';
import { allExtensions } from '../src/extensions';
import { MakeProject } from '../src/builders/make';

const cwd = path.join(__dirname, `..`, `..`, `ibmi-company_system-rmake`);
const scanGlob = `**/*.{${allExtensions.join(`,`)},${allExtensions.map(e => e.toUpperCase()).join(`,`)}}`;

const makeDefaults = MakeProject.getDefaultSettings();

let files = getFiles(cwd, scanGlob);

describe.skipIf(files.length === 0)(`ibmi-company_system tests`, () => {
  const targets = new Targets(cwd);
  
  beforeAll(async () => {
    const parsePromises = files.map(f => targets.handlePath(f));
    await Promise.all(parsePromises);

    expect(targets.getDeps().length).toBeGreaterThan(0);
    targets.resolveBinder();
  });

  test(`Check objects are generated`, async () => {
    expect(targets.getResolvedObjects().length).toBe(11);
    expect(targets.getDeps().length).toBe(12);
    expect(targets.getParentObjects(`FILE`).length).toBe(4);
    expect(targets.getParentObjects(`PGM`).length).toBe(3);
    expect(targets.getParentObjects(`MODULE`).length).toBe(2);
    expect(targets.getParentObjects(`SRVPGM`).length).toBe(2);
  });

  test(`Check mypgm`, async () => {
    const myPgm = targets.getDep({name: `MYPGM`, type: `PGM`});
    expect(myPgm.relativePath).toBe(`qrpglesrc/mypgm.pgm.rpgle`);
    expect(myPgm.deps.length).toBe(0);
  });

  test(`Check employees`, async () => {
    const myPgm = targets.getDep({name: `EMPLOYEES`, type: `PGM`});
    expect(myPgm.relativePath).toBe(`qrpglesrc/employees.pgm.sqlrpgle`);

    expect(myPgm.deps.length).toBe(2);

    const empTable = myPgm.deps[0];
    expect(empTable.name).toBe(`EMPLOYEE`);
    expect(empTable.type).toBe(`FILE`);
    expect(empTable.relativePath).toBe(`qddssrc/employee.table`);

    const empDisplay = myPgm.deps[1];
    expect(empDisplay.name).toBe(`EMPS`);
    expect(empDisplay.type).toBe(`FILE`);
    expect(empDisplay.relativePath).toBe(`qddssrc/emps.dspf`);
  });

  test(`Check depts`, async () => {
    const myPgm = targets.getDep({name: `DEPTS`, type: `PGM`});
    expect(myPgm.relativePath).toBe(`qrpglesrc/depts.pgm.sqlrpgle`);
    expect(myPgm.text).toBe(`This is the text for this program`);

    expect(myPgm.deps.length).toBe(4);

    const empPgm = myPgm.deps[0];
    expect(empPgm.name).toBe(`EMPLOYEES`);
    expect(empPgm.type).toBe(`PGM`);
    expect(empPgm.relativePath).toBe(`qrpglesrc/employees.pgm.sqlrpgle`);

    const deptTable = myPgm.deps[1];
    expect(deptTable.name).toBe(`DEPARTMENT`);
    expect(deptTable.type).toBe(`FILE`);
    expect(deptTable.relativePath).toBe(`qddssrc/department.table`);

    const deptFile = myPgm.deps[2];
    expect(deptFile.name).toBe(`DEPTS`);
    expect(deptFile.type).toBe(`FILE`);
    expect(deptFile.relativePath).toBe(`qddssrc/depts.dspf`);

    const utilsSrvPgm = myPgm.deps[3];
    expect(utilsSrvPgm.name).toBe(`UTILS`);
    expect(utilsSrvPgm.type).toBe(`SRVPGM`);
    expect(utilsSrvPgm.relativePath).toBe(`qsrvsrc/utils.bnd`);
  });

  test(`Check utils`, async () => {
    const myPgm = targets.getDep({name: `UTILS`, type: `SRVPGM`});
    expect(myPgm.relativePath).toBe(`qsrvsrc/utils.bnd`);

    expect(myPgm.deps.length).toBe(1);

    const empPgm = myPgm.deps[0];
    expect(empPgm.name).toBe(`UTILS`);
    expect(empPgm.type).toBe(`MODULE`);
    expect(empPgm.relativePath).toBe(`qrpglesrc/utils.sqlrpgle`);
  });

  test(`Check binding directory`, async () => {
    const myBinder = targets.getDep({name: `$(APP_BNDDIR)`, type: `BNDDIR`});
    expect(myBinder.relativePath).toBeUndefined();

    expect(myBinder.deps.length).toBe(2);

    console.log(myBinder.deps);

    const bankingSrvpgm = myBinder.deps[0];
    expect(bankingSrvpgm.name).toBe(`BANKING`);
    expect(bankingSrvpgm.type).toBe(`SRVPGM`);
    expect(bankingSrvpgm.relativePath).toBe(`qsrvsrc/banking.bnd`);

    const utilsSrvpgm = myBinder.deps[1];
    expect(utilsSrvpgm.name).toBe(`UTILS`);
    expect(utilsSrvpgm.type).toBe(`SRVPGM`);
    expect(utilsSrvpgm.relativePath).toBe(`qsrvsrc/utils.bnd`);
  });

  test(`Check employee table`, async () => {
    const empTable = targets.getDep({name: `EMPLOYEE`, type: `FILE`});
    expect(empTable.relativePath).toBe(`qddssrc/employee.table`);
    expect(empTable.text).toBe(`Employee File`);
    
  });

  test(`Testing removing and adding an object`, async () => {
    // First, let's delete the object internally
    let deptsPgm = targets.getDep({name: `DEPTS`, type: `PGM`});
    let deptsFile = targets.getDep({name: `DEPTS`, type: `FILE`});

    const deptsFilePath = path.join(cwd, deptsFile.relativePath);
    const deptsPgmPath = path.join(cwd, deptsPgm.relativePath);

    targets.logger.flush(deptsFile.relativePath);

    // We removed the DEPTS display file, used by DEPTS program
    const impacted = targets.removeObjectByPath(path.join(cwd, deptsFile.relativePath));
    expect(impacted.length).toBe(1);
    expect(impacted[0].name).toBe(`DEPTS`);
    expect(impacted[0].type).toBe(`PGM`);
    
    const logs = targets.logger.getLogsFor(deptsPgm.relativePath);
    expect(logs.length).toBe(1);
    expect(logs[0].message).toBe(`This object depended on DEPTS.FILE before it was deleted.`);

    expect(targets.getDep({name: `DEPTS`, type: `FILE`})).toBeUndefined();

    targets.resolveBinder();

    // Now let's add it back

    await targets.handlePath(deptsFilePath);

    deptsFile = targets.getDep({name: `DEPTS`, type: `FILE`});
    expect(deptsFile).toBeDefined();

    // Just because we re-handle the path, doesn't mean it's picked up again in other places where it was before
    expect(deptsPgm.deps.find(d => d.name === `DEPTS` && d.type === `FILE`)).toBeUndefined();

    await targets.handlePath(deptsPgmPath);
    // We have to fetch the dep again because the old reference is lost since we parsed again
    deptsPgm = targets.getDep({name: `DEPTS`, type: `PGM`});
    expect(deptsPgm.deps.find(d => d.name === `DEPTS` && d.type === `FILE`)).toBeDefined();
  });

  test(`Double resolve binder test`, async () => {
    targets.resolveBinder();
    let deptsPgm = targets.getDep({name: `DEPTS`, type: `PGM`});

    expect(deptsPgm.deps.length).toBe(4);

    targets.resolveBinder();
    deptsPgm = targets.getDep({name: `DEPTS`, type: `PGM`});

    expect(deptsPgm.deps.length).toBe(4);
  });

  test(`Check mypgm RPGLE target`, async () => {
    const myPgm = targets.getDep({name: `MYPGM`, type: `PGM`});
    const lines = MakeProject.generateSpecificTarget(makeDefaults.compiles[`pgm.rpgle`], myPgm);

    expect(lines.join()).toBe([
      '$(PREPATH)/MYPGM.PGM: qrpglesrc/mypgm.pgm.rpgle',
      '\tliblist -c $(BIN_LIB);\\',
      `\tsystem "CRTBNDRPG PGM($(BIN_LIB)/MYPGM) SRCSTMF('qrpglesrc/mypgm.pgm.rpgle') OPTION(*EVENTF) DBGVIEW(*SOURCE) TGTRLS(*CURRENT) TGTCCSID(*JOB) BNDDIR($(BNDDIR)) DFTACTGRP(*no)" > .logs/mypgm.splf`,
      `\tsystem "CPYTOSTMF FROMMBR(\'$(PREPATH)/EVFEVENT.FILE/MYPGM.MBR\') TOSTMF(\'.evfevent/mypgm.evfevent\') DBFCCSID(*FILE) STMFCCSID(1208) STMFOPT(*REPLACE)"`
    ].join());
  });

  test(`Check depts SQLRPGLE target (with CHGATR)`, async () => {
    const myPgm = targets.getDep({name: `DEPTS`, type: `PGM`});
    const lines = MakeProject.generateSpecificTarget(makeDefaults.compiles[`pgm.sqlrpgle`], myPgm);

    expect(lines.join()).toBe([
      '$(PREPATH)/DEPTS.PGM: qrpglesrc/depts.pgm.sqlrpgle',
      `\tsystem -s "CHGATR OBJ('qrpglesrc/depts.pgm.sqlrpgle') ATR(*CCSID) VALUE(1252)"`,
      '\tliblist -c $(BIN_LIB);\\',
      `\tsystem "CRTSQLRPGI OBJ($(BIN_LIB)/DEPTS) SRCSTMF('qrpglesrc/depts.pgm.sqlrpgle') COMMIT(*NONE) DBGVIEW(*SOURCE) OPTION(*EVENTF) COMPILEOPT('BNDDIR($(BNDDIR)) DFTACTGRP(*no)')" > .logs/depts.splf`,
      `\tsystem "CPYTOSTMF FROMMBR(\'$(PREPATH)/EVFEVENT.FILE/DEPTS.MBR\') TOSTMF(\'.evfevent/depts.evfevent\') DBFCCSID(*FILE) STMFCCSID(1208) STMFOPT(*REPLACE)"`
    ].join());
  });

  test(`Check depts DSPF target (member)`, async () => {
    const myPgm = targets.getDep({name: `DEPTS`, type: `FILE`});
    const lines = MakeProject.generateSpecificTarget(makeDefaults.compiles[`dspf`], myPgm);

    expect(lines.join()).toBe([
      '$(PREPATH)/DEPTS.FILE: qddssrc/depts.dspf',
      '\t-system -qi "CRTSRCPF FILE($(BIN_LIB)/qddssrc) RCDLEN(112)"',
      `\tsystem "CPYFRMSTMF FROMSTMF('qddssrc/depts.dspf') TOMBR('$(PREPATH)/qddssrc.FILE/DEPTS.MBR') MBROPT(*REPLACE)"`,
      '\tliblist -c $(BIN_LIB);\\',
      '\tsystem "CRTDSPF FILE($(BIN_LIB)/DEPTS) SRCFILE($(BIN_LIB)/qddssrc) SRCMBR(DEPTS) OPTION(*EVENTF)" > .logs/depts.splf',
      `\tsystem "CPYTOSTMF FROMMBR(\'$(PREPATH)/EVFEVENT.FILE/DEPTS.MBR\') TOSTMF(\'.evfevent/depts.evfevent\') DBFCCSID(*FILE) STMFCCSID(1208) STMFOPT(*REPLACE)"`
    ].join());
  });

  test(`Check utils SRVPGM target (from binder source, *MODULES variable)`, async () => {
    const myPgm = targets.getDep({name: `UTILS`, type: `SRVPGM`});
    const lines = MakeProject.generateSpecificTarget(makeDefaults.compiles[`bnd`], myPgm);

    expect(lines.join()).toBe([
      '$(PREPATH)/UTILS.SRVPGM: qsrvsrc/utils.bnd',
      '\t-system -q "CRTBNDDIR BNDDIR($(BIN_LIB)/$(APP_BNDDIR))"',
      '\t-system -q "RMVBNDDIRE BNDDIR($(BIN_LIB)/$(APP_BNDDIR)) OBJ(($(BIN_LIB)/UTILS))"',
      '\t-system "DLTOBJ OBJ($(BIN_LIB)/UTILS) OBJTYPE(*SRVPGM)"',
      '\tliblist -c $(BIN_LIB);\\',
      `\tsystem "CRTSRVPGM SRVPGM($(BIN_LIB)/UTILS) MODULE(UTILS) SRCSTMF('qsrvsrc/utils.bnd') BNDDIR($(BNDDIR))" > .logs/utils.splf`,
      '\t-system -q "ADDBNDDIRE BNDDIR($(BIN_LIB)/$(APP_BNDDIR)) OBJ((*LIBL/UTILS *SRVPGM *IMMED))"',
      `\tsystem "CPYTOSTMF FROMMBR(\'$(PREPATH)/EVFEVENT.FILE/UTILS.MBR\') TOSTMF(\'.evfevent/utils.evfevent\') DBFCCSID(*FILE) STMFCCSID(1208) STMFOPT(*REPLACE)"`
    ].join());
  });

  test(`Check banking SRVPGM target (no binder source)`, async () => {
    const myPgm = targets.getDep({name: `BANKING`, type: `SRVPGM`});
    const lines = MakeProject.generateSpecificTarget(makeDefaults.compiles[`srvpgm`], myPgm);

    expect(lines.join()).toBe([
      '$(PREPATH)/BANKING.SRVPGM: qsrvsrc/banking.bnd',
      '\t-system -q "CRTBNDDIR BNDDIR($(BIN_LIB)/$(APP_BNDDIR))"',
      '\t-system -q "RMVBNDDIRE BNDDIR($(BIN_LIB)/$(APP_BNDDIR)) OBJ(($(BIN_LIB)/BANKING))"',
      '\t-system "DLTOBJ OBJ($(BIN_LIB)/BANKING) OBJTYPE(*SRVPGM)"',
      '\tliblist -c $(BIN_LIB);\\',
      '\tsystem "CRTSRVPGM SRVPGM($(BIN_LIB)/BANKING) MODULE(*SRVPGM) EXPORT(*ALL) BNDDIR($(BNDDIR))" > .logs/banking.splf',
      '\t-system -q "ADDBNDDIRE BNDDIR($(BIN_LIB)/$(APP_BNDDIR)) OBJ((*LIBL/BANKING *SRVPGM *IMMED))"',
      `\tsystem "CPYTOSTMF FROMMBR(\'$(PREPATH)/EVFEVENT.FILE/BANKING.MBR\') TOSTMF(\'.evfevent/banking.evfevent\') DBFCCSID(*FILE) STMFCCSID(1208) STMFOPT(*REPLACE)"`
    ].join());
  });

  test(`Impact of EMPLOYEES`, () => {
    const empPgm = targets.getDep({name: `EMPLOYEES`, type: `PGM`});
    expect(empPgm.relativePath).toBe(`qrpglesrc/employees.pgm.sqlrpgle`);

    const impactTree = targets.getImpactFor(empPgm);
    expect(impactTree.ileObject.name).toBe(`EMPLOYEES`);
    expect(impactTree.children.length).toBe(1);

    // Because DEPTS calls the EMPLOYEES program, so if EMPLOYEES changes, DEPTS needs a rebuild
    expect(impactTree.children[0].ileObject.name).toBe(`DEPTS`);
  });

  test(`Impact of UTILS`, () => {
    const utilsModule = targets.getDep({name: `UTILS`, type: `MODULE`});
    expect(utilsModule.relativePath).toBe(`qrpglesrc/utils.sqlrpgle`);

    const impactTree = targets.getImpactFor(utilsModule);
    expect(impactTree.ileObject.name).toBe(`UTILS`);
    expect(impactTree.children.length).toBe(1);

    // Because DEPTS calls the EMPLOYEES program, so if EMPLOYEES changes, DEPTS needs a rebuild
    const utilsSrvPgm = impactTree.children[0];
    expect(utilsSrvPgm.ileObject.name).toBe(`UTILS`);
    expect(utilsSrvPgm.ileObject.name).toBe(`UTILS`);
    expect(utilsSrvPgm.children.length).toBe(2);

    const srvChildren = utilsSrvPgm.children;
    expect(srvChildren[0].ileObject.name).toBe(`DEPTS`);
    expect(srvChildren[0].ileObject.type).toBe(`PGM`);

    expect(srvChildren[1].ileObject.name).toBe(`$(APP_BNDDIR)`);
    expect(srvChildren[1].ileObject.type).toBe(`BNDDIR`);
  });

  test(`Resolve SQL object by path`, () => {
    // We have a test for this as SQL objects are created a little different
    // from regular objects.

    const filePath = path.join(targets.getCwd(), `qddssrc`, `employee.table`);
    const resolvedObject = targets.getResolvedObject(filePath);

    expect(resolvedObject).toBeDefined();
    expect(resolvedObject.name).toBe(`EMPLOYEE`);
    expect(resolvedObject.type).toBe(`FILE`);
  })
});

function getFiles(cwd: string, globPath: string): string[] {
	return glob.sync(globPath, {
		cwd,
		absolute: true,
		nocase: true,
	});
}
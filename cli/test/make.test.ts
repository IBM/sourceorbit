import { assert, expect, test } from 'vitest'
import { baseTargets, cwd, multiModuleObjects } from './fixtures/targets';
import { MakeProject } from '../src/builders/make';
import { ReadFileSystem } from '../src/readFileSystem';

test('generateTargets (pre-resolve)', async () => {
  const targets = await baseTargets(true);
  const project = new MakeProject(cwd, targets, new ReadFileSystem());
  await project.setupSettings();

  const targetContent = project.generateTargets();

  expect(targetContent.length).toBe(12);
  expect(targetContent).toEqual(
    [
      'all: .logs .evfevent library $(PREPATH)/PROGRAMA.PGM $(PREPATH)/PROGRAMB.PGM $(PREPATH)/PROGRAMA.CMD $(PREPATH)/UNUSED.CMD',
      '',
      '$(PREPATH)/PROGRAMA.PGM: $(PREPATH)/FILEA.FILE $(PREPATH)/PROGRAMB.PGM',
      '$(PREPATH)/MODULEA.MODULE: $(PREPATH)/FILEA.FILE $(PREPATH)/FILEB.FILE',
      '$(PREPATH)/MODULEB.MODULE: $(PREPATH)/FILEB.FILE',
      ``,
      `.logs:`,
			`\tmkdir .logs`,
			`.evfevent:`,
			`\tmkdir .evfevent`,
			`library:`,
			`\t-system -q "CRTLIB LIB($(BIN_LIB))"`,
    ]
  );
});

test('generateTargets (post-resolve)', async () => {
  const targets = await baseTargets(true);

  targets.resolveBinder();

  const project = new MakeProject(cwd, targets, new ReadFileSystem());

  const targetContent = project.generateTargets();

  expect(targetContent).toEqual(
    [
      'all: .logs .evfevent library $(PREPATH)/$(APP_BNDDIR).BNDDIR $(PREPATH)/PROGRAMA.PGM $(PREPATH)/PROGRAMB.PGM $(PREPATH)/PROGRAMA.CMD',
      '',
      '$(PREPATH)/PROGRAMA.PGM: $(PREPATH)/FILEA.FILE $(PREPATH)/PROGRAMB.PGM',
      '$(PREPATH)/PROGRAMB.PGM: $(PREPATH)/SRVPGMA.SRVPGM',
      '$(PREPATH)/MODULEA.MODULE: $(PREPATH)/FILEA.FILE $(PREPATH)/FILEB.FILE',
      '$(PREPATH)/MODULEB.MODULE: $(PREPATH)/FILEB.FILE',
      `$(PREPATH)/SRVPGMA.SRVPGM: $(PREPATH)/MODULEB.MODULE`,
      `$(PREPATH)/ORDENTSRV.SRVPGM: $(PREPATH)/ORDENTMOD.MODULE`,
      '$(PREPATH)/PROGRAMA.CMD: $(PREPATH)/PROGRAMA.PGM',
      '$(PREPATH)/$(APP_BNDDIR).BNDDIR: $(PREPATH)/SRVPGMA.SRVPGM $(PREPATH)/ORDENTSRV.SRVPGM',
      ``,
      `.logs:`,
			`\tmkdir .logs`,
			`.evfevent:`,
			`\tmkdir .evfevent`,
			`library:`,
			`\t-system -q "CRTLIB LIB($(BIN_LIB))"`,
    ]
  );

  // The steps API provides a way to get the steps to build a specific target.

  const programB = targets.getTarget({systemName: `PROGRAMB`, type: `PGM`});
  const steps = project.getSteps(programB);

  expect(steps.length).toBe(6);

  expect(steps[0].command).toBe(`CPYFRMSTMF FROMSTMF('qddssrc/fileB.pf') TOMBR('/QSYS.LIB/&CURLIB.LIB/QTMPSRC.FILE/FILEB.MBR') MBROPT(*REPLACE)`);
  expect(steps[1].command).toBe(`CRTPF FILE(*CURLIB/FILEB) SRCFILE(*CURLIB/QTMPSRC) OPTION(*EVENTF) SRCMBR(FILEB)`);
  expect(steps[2].command).toBe(`CRTSQLRPGI OBJ(*CURLIB/MODULEB) SRCSTMF('qrpglesrc/moduleB.sqlrpgle') COMMIT(*NONE) DBGVIEW(*SOURCE) COMPILEOPT('TGTCCSID(*JOB)') RPGPPOPT(*LVL2) OPTION(*EVENTF) OBJTYPE(*MODULE)`);
  expect(steps[3].command).toBe(`CRTSRVPGM SRVPGM(*CURLIB/SRVPGMA) MODULE(MODULEB) SRCSTMF('qsrvsrc/srvpgmA.bnd') BNDDIR(APP) REPLACE(*YES)`);
  expect(steps[4].command).toBe(`ADDBNDDIRE BNDDIR(*CURLIB/APP) OBJ((*LIBL/SRVPGMA *SRVPGM *IMMED))`);
  expect(steps[5].command).toBe(`CRTSQLRPGI OBJ(*CURLIB/PROGRAMB) SRCSTMF('qrpglesrc/programB.pgm.sqlrpgle') COMMIT(*NONE) DBGVIEW(*SOURCE) OPTION(*EVENTF) RPGPPOPT(*LVL2) COMPILEOPT('TGTCCSID(*JOB) BNDDIR(APP) DFTACTGRP(*no)')`);
});

test('generateHeader (binder changes)', async () => {
  const targets = await baseTargets(true);

  const project = new MakeProject(cwd, targets, new ReadFileSystem());
  await project.setupSettings();

  const headerContentA = project.generateHeader();
  let bndDirIndex = headerContentA.findIndex(h => h.startsWith(`BNDDIR=`));

  expect(bndDirIndex).toBeGreaterThanOrEqual(0);
  expect(headerContentA[bndDirIndex]).toBe(`BNDDIR=*NONE`);

  targets.resolveBinder();

  const headerContentB = project.generateHeader();

  expect(headerContentB[bndDirIndex]).toBe(`BNDDIR=($(BIN_LIB)/$(APP_BNDDIR))`);
});

test('applySettings (binder)', async () => {
  const targets = await baseTargets(true);

  const project = new MakeProject(cwd, targets, new ReadFileSystem());
  await project.setupSettings();

  project.getSettings().applySettings({
    binders: [`TESTING`]
  });

  const headerContentA = project.generateHeader();
  let bndDirIndex = headerContentA.findIndex(h => h.startsWith(`BNDDIR=`));

  expect(bndDirIndex).toBeGreaterThanOrEqual(0);
  expect(headerContentA[bndDirIndex]).toBe(`BNDDIR=(TESTING)`);

  targets.resolveBinder();

  const headerContentB = project.generateHeader();

  expect(headerContentB[bndDirIndex]).toBe(`BNDDIR=($(BIN_LIB)/$(APP_BNDDIR)) (TESTING)`);
});

test(`Multi-module program and service program`, async () => {
  const targets = await multiModuleObjects();

  const project = new MakeProject(cwd, targets, new ReadFileSystem());
  await project.setupSettings();
  
  const settings = project.getSettings();

  settings.applySettings({
    binders: [`ILEASTIC`, `NOXDB`]
  });

  const headerContent = project.generateTargets();

  expect(headerContent.join()).toBe([
    'all: .logs .evfevent library $(PREPATH)/$(APP_BNDDIR).BNDDIR $(PREPATH)/MYWEBAPP.PGM',
    '',
    '$(PREPATH)/MYWEBAPP.PGM: $(PREPATH)/HANDLERA.MODULE $(PREPATH)/HANDLERB.MODULE $(PREPATH)/UTILS.SRVPGM $(PREPATH)/MYWEBAPP.MODULE',
    '$(PREPATH)/HANDLERB.MODULE: $(PREPATH)/UTILS.SRVPGM',
    '$(PREPATH)/UTILS.SRVPGM: $(PREPATH)/JWTHANDLER.MODULE $(PREPATH)/VALIDATE.MODULE',
    '$(PREPATH)/$(APP_BNDDIR).BNDDIR: $(PREPATH)/UTILS.SRVPGM',
    ``,
    `.logs:`,
    `\tmkdir .logs`,
    `.evfevent:`,
    `\tmkdir .evfevent`,
    `library:`,
    `\t-system -q "CRTLIB LIB($(BIN_LIB))"`,
  ].join());

  const webappPgm = targets.getTarget({systemName: `MYWEBAPP`, type: `PGM`});
  const webPgmTarget = MakeProject.generateSpecificTarget(settings.compiles[`pgm`], webappPgm);
  expect(webPgmTarget.join()).toBe([
    '$(PREPATH)/MYWEBAPP.PGM: ',
    '\tliblist -c $(BIN_LIB);\\',
    '\tliblist -a $(LIBL);\\',
    '\tsystem "CRTPGM PGM($(BIN_LIB)/MYWEBAPP) ENTMOD(MYWEBAPP) MODULE(HANDLERA HANDLERB MYWEBAPP) TGTRLS(*CURRENT) BNDDIR($(APP_BNDDIR)) ACTGRP(*NEW)" > .logs/mywebapp.splf'
  ].join());

  const webappMod = targets.getTarget({systemName: `MYWEBAPP`, type: `MODULE`});
  const webModTarget = MakeProject.generateSpecificTarget(settings.compiles[`rpgle`], webappMod);
  expect(webModTarget.join()).toBe([
    '$(PREPATH)/MYWEBAPP.MODULE: qrpglesrc/mywebapp.pgm.rpgle',
    '\tliblist -c $(BIN_LIB);\\',
    '\tliblist -a $(LIBL);\\',
    [
      `\tsystem "CRTRPGMOD MODULE($(BIN_LIB)/MYWEBAPP) SRCSTMF('qrpglesrc/mywebapp.pgm.rpgle') OPTION(*EVENTF) DBGVIEW(*SOURCE) TGTRLS(*CURRENT) TGTCCSID(*JOB)" > .logs/mywebapp.splf || \\`,
      `\t(system "CPYTOSTMF FROMMBR('$(PREPATH)/EVFEVENT.FILE/MYWEBAPP.MBR') TOSTMF('.evfevent/mywebapp.evfevent') DBFCCSID(*FILE) STMFCCSID(1208) STMFOPT(*REPLACE)"; $(SHELL) -c 'exit 1')`,
    ].join('\n')
  ].join());

  const utilsSrvpgm = targets.getTarget({systemName: `UTILS`, type: `SRVPGM`});
  const utilsTarget = MakeProject.generateSpecificTarget(settings.compiles[`srvpgm`], utilsSrvpgm);

  expect(utilsTarget.join()).toBe([
    '$(PREPATH)/UTILS.SRVPGM: qsrvsrc/utils.binder',
    '\t-system -q "CRTBNDDIR BNDDIR($(BIN_LIB)/$(APP_BNDDIR))"',
    '\t-system -q "RMVBNDDIRE BNDDIR($(BIN_LIB)/$(APP_BNDDIR)) OBJ(($(BIN_LIB)/UTILS))"',
    '\t-system "DLTOBJ OBJ($(BIN_LIB)/UTILS) OBJTYPE(*SRVPGM)"',
    '\tliblist -c $(BIN_LIB);\\',
    '\tliblist -a $(LIBL);\\',
    `\tsystem "CRTSRVPGM SRVPGM($(BIN_LIB)/UTILS) MODULE(JWTHANDLER VALIDATE) SRCSTMF('qsrvsrc/utils.binder') BNDDIR($(APP_BNDDIR))" > .logs/utils.splf`,
    '\t-system -q "ADDBNDDIRE BNDDIR($(BIN_LIB)/$(APP_BNDDIR)) OBJ((*LIBL/UTILS *SRVPGM *IMMED))"'
  ].join());
});

test('generateTargets (post-resolve)', async () => {
  const targets = await baseTargets(true);

  targets.resolveBinder();

  const project = new MakeProject(cwd, targets, new ReadFileSystem());

  const srvpgma = targets.getTarget({systemName: `SRVPGMA`, type: `SRVPGM`});
  const srvpgmaRequirements = targets.getRequiredChildren([srvpgma]);
  expect(srvpgmaRequirements.length).toBe(3);
  expect(srvpgmaRequirements.map(r => r.systemName)).toEqual([
    `FILEB`,
    `MODULEB`,
    `SRVPGMA`
  ]);

  project.setPartialOptions({withChildren: true});

  const targetContent = project.generateTargets([srvpgma]);

  console.log(targetContent.join('\n'));

  expect(targetContent).toEqual(
    [
      'all: .logs .evfevent library $(PREPATH)/SRVPGMA.SRVPGM',
      '',
      '$(PREPATH)/MODULEB.MODULE: $(PREPATH)/FILEB.FILE',
      `$(PREPATH)/SRVPGMA.SRVPGM: $(PREPATH)/MODULEB.MODULE`,
      ``,
      `.logs:`,
			`\tmkdir .logs`,
			`.evfevent:`,
			`\tmkdir .evfevent`,
			`library:`,
			`\t-system -q "CRTLIB LIB($(BIN_LIB))"`,
    ]
  );

  const rules = project.generateGenericRules([srvpgma]);

  console.log(rules.join('\n'));
  expect(rules).toContain(`$(PREPATH)/MODULEB.MODULE: qrpglesrc/moduleB.sqlrpgle`);
  expect(rules).toContain(`$(PREPATH)/SRVPGMA.SRVPGM: qsrvsrc/srvpgmA.bnd`);
  expect(rules).toContain(`$(PREPATH)/FILEB.FILE: qddssrc/fileB.pf`);
  expect(rules.length).toBe(39);
});
import { assert, expect, test } from 'vitest'
import { baseTargets, cwd, multiModuleObjects } from './fixtures/targets';
import { MakeProject } from '../src/builders/make';

test('generateTargets (pre-resolve)', () => {
  const targets = baseTargets(true);
  const project = new MakeProject(cwd, targets);

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

test('generateTargets (post-resolve)', () => {
  const targets = baseTargets(true);

  targets.resolveBinder();

  const project = new MakeProject(cwd, targets);

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
});

test('generateHeader (binder changes)', () => {
  const targets = baseTargets(true);

  const project = new MakeProject(cwd, targets);

  const headerContentA = project.generateHeader();
  let bndDirIndex = headerContentA.findIndex(h => h.startsWith(`BNDDIR=`));

  expect(bndDirIndex).toBeGreaterThanOrEqual(0);
  expect(headerContentA[bndDirIndex]).toBe(`BNDDIR=*NONE`);

  targets.resolveBinder();

  const headerContentB = project.generateHeader();

  expect(headerContentB[bndDirIndex]).toBe(`BNDDIR=($(BIN_LIB)/$(APP_BNDDIR))`);
});

test('applySettings (binder)', () => {
  const targets = baseTargets(true);

  const project = new MakeProject(cwd, targets);

  project.applySettings({
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

test(`Multi-module program and service program`, () => {
  const targets = multiModuleObjects();

  const project = new MakeProject(cwd, targets);
  const settings = project.getSettings();

  project.applySettings({
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
    '\tsystem "CRTPGM PGM($(BIN_LIB)/MYWEBAPP) ENTRY(MYWEBAPP) MODULES(HANDLERA HANDLERB MYWEBAPP) TGTRLS(*CURRENT) TGTCCSID(*JOB) BNDDIR($(BNDDIR)) DFTACTGRP(*NO)" > .logs/mywebapp.splf'
  ].join());

  const webappMod = targets.getTarget({systemName: `MYWEBAPP`, type: `MODULE`});
  const webModTarget = MakeProject.generateSpecificTarget(settings.compiles[`rpgle`], webappMod);
  expect(webModTarget.join()).toBe([
    '$(PREPATH)/MYWEBAPP.MODULE: qrpglesrc/mywebapp.pgm.rpgle',
    '\tliblist -c $(BIN_LIB);\\',
    '\tliblist -a $(LIBL);\\',
    `\tsystem "CRTRPGMOD MODULE($(BIN_LIB)/MYWEBAPP) SRCSTMF('qrpglesrc/mywebapp.pgm.rpgle') OPTION(*EVENTF) DBGVIEW(*SOURCE) TGTRLS(*CURRENT) TGTCCSID(*JOB)" > .logs/mywebapp.splf`,
    `\tsystem "CPYTOSTMF FROMMBR('$(PREPATH)/EVFEVENT.FILE/MYWEBAPP.MBR') TOSTMF('.evfevent/mywebapp.evfevent') DBFCCSID(*FILE) STMFCCSID(1208) STMFOPT(*REPLACE)"`
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
    `\tsystem "CRTSRVPGM SRVPGM($(BIN_LIB)/UTILS) MODULE(JWTHANDLER VALIDATE) SRCSTMF('qsrvsrc/utils.binder') BNDDIR($(BNDDIR))" > .logs/utils.splf`,
    '\t-system -q "ADDBNDDIRE BNDDIR($(BIN_LIB)/$(APP_BNDDIR)) OBJ((*LIBL/UTILS *SRVPGM *IMMED))"'
  ].join());
})
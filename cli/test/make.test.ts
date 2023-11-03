import { assert, expect, test } from 'vitest'
import { Targets } from '../src/targets'
import path from 'path';
import { baseTargets, cwd } from './fixture';
import { MakeProject } from '../src/builders/make';

test('generateTargets (pre-resolve)', () => {
  const targets = baseTargets(true);
  const project = new MakeProject(cwd, targets);

  const targetContent = project.generateTargets();

  expect(targetContent.length).toBe(5);
  expect(targetContent).toEqual(
    [
      'all: $(PREPATH)/PROGRAMA.PGM $(PREPATH)/PROGRAMB.PGM $(PREPATH)/PROGRAMA.CMD $(PREPATH)/UNUSED.CMD',
      '',
      '$(PREPATH)/PROGRAMA.PGM: $(PREPATH)/FILEA.FILE $(PREPATH)/PROGRAMB.PGM',
      '$(PREPATH)/MODULEA.MODULE: $(PREPATH)/FILEA.FILE $(PREPATH)/FILEB.FILE',
      '$(PREPATH)/MODULEB.MODULE: $(PREPATH)/FILEB.FILE',
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
      'all: $(PREPATH)/$(APP_BNDDIR).BNDDIR $(PREPATH)/PROGRAMA.PGM $(PREPATH)/PROGRAMB.PGM $(PREPATH)/PROGRAMA.CMD',
      '',
      '$(PREPATH)/PROGRAMA.PGM: $(PREPATH)/FILEA.FILE $(PREPATH)/PROGRAMB.PGM',
      '$(PREPATH)/PROGRAMB.PGM: $(PREPATH)/SRVPGMA.SRVPGM',
      '$(PREPATH)/MODULEA.MODULE: $(PREPATH)/FILEA.FILE $(PREPATH)/FILEB.FILE',
      '$(PREPATH)/MODULEB.MODULE: $(PREPATH)/FILEB.FILE',
      `$(PREPATH)/SRVPGMA.SRVPGM: $(PREPATH)/MODULEB.MODULE`,
      `$(PREPATH)/ORDENTSRV.SRVPGM: $(PREPATH)/ORDENTMOD.MODULE`,
      '$(PREPATH)/PROGRAMA.CMD: $(PREPATH)/PROGRAMA.PGM',
      '$(PREPATH)/$(APP_BNDDIR).BNDDIR: $(PREPATH)/SRVPGMA.SRVPGM $(PREPATH)/ORDENTSRV.SRVPGM',
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
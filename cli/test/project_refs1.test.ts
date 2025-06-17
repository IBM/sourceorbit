import { beforeAll, describe, expect, test } from 'vitest';

import { Targets } from '../src/targets'
import { setupFixture } from './fixtures/projects';
import { ReadFileSystem } from '../src/readFileSystem';
import path from 'path';
import { readFile } from 'fs/promises';

describe(`company_system tests`, () => {
  const project = setupFixture(`company_system`);

  const fs = new ReadFileSystem();
  const targets: Targets = new Targets(project.cwd, fs, true);
  
  beforeAll(async () => {
    project.setup();
    await targets.loadProject();

    expect(targets.getTargets().length).toBeGreaterThan(0);
    targets.resolveBinder();
  });

  test('Expect symbols', async () => {
    const myPgm = targets.getTarget({systemName: `MYPGM`, type: `PGM`});
    expect(myPgm).toBeDefined();

    expect(myPgm.source).toBeDefined();
    expect(myPgm.source.symbols.length).toBe(28);

    const printf = myPgm.source.symbols.find(s => s.name === `printf`);
    expect(printf).toBeDefined();

    expect(printf.name).toBe(`printf`);
    expect(printf.relativePath).toBe(myPgm.source.relativePath);
    expect(printf.external).toBe(`printf`);
    expect(printf.children.length).toBe(1);

    expect(printf.references[printf.relativePath].length).toBe(2);

    // Let's check the accuracy of the references
    const filePath = path.join(project.cwd, printf.relativePath);
    const contents = await readFile(filePath, {encoding: `utf-8`});
    
    for (const reference of printf.references[printf.relativePath]) {
      const start = reference.start;
      const end = reference.end;
      const text = contents.substring(start, end);

      // toLowerCase because RPGLE is not case sensitive
      expect(text.toLowerCase()).toBe(`printf`);
    }

    const f1 = myPgm.source.symbols.find(s => s.name === `F01`);
    expect(f1).toBeDefined();

    expect(f1.relativePath).not.toBe(myPgm.source.relativePath);
    expect(f1.references[f1.relativePath].length).toBe(1);
    expect(f1.references[myPgm.source.relativePath].length).toBe(1);
  });

  test('Export lookup', async () => {
    const depts = targets.getTarget({systemName: `DEPTS`, type: `PGM`});
    expect(depts).toBeDefined();
    expect(depts.source).toBeDefined();
    expect(depts.source.symbols.length).toBeGreaterThan(0);

    const ClearSubfileProc = depts.source.symbols.find(s => s.name === `ClearSubfile`);
    expect(ClearSubfileProc).toBeDefined();
    expect(ClearSubfileProc.external).toBeUndefined();
    expect(ClearSubfileProc.children.length).toBe(0)

    const ToLowerProc = depts.source.symbols.find(s => s.name === `ToLower`);
    expect(ToLowerProc).toBeDefined();
    expect(ToLowerProc.external).toBe(`ToLower`);
    expect(ToLowerProc.children.length).toBe(1);

    const exportLookup = targets.resolveExport(ToLowerProc.external);
    expect(exportLookup).toBeDefined();

    expect(exportLookup.systemName).toBe(`UTILS`);
    expect(exportLookup.source.extension).toBe(`bnd`);

    const ut = targets.getTarget(exportLookup);
    expect(ut).toBeDefined();
    expect(ut.deps.length).toBe(1);

    const utilsMod = ut.deps[0];
    expect(utilsMod.source).toBeDefined();
    expect(utilsMod.source.extension).toBe(`sqlrpgle`);
    expect(utilsMod.source.symbols.length).toBe(1);

    const ToLower = utilsMod.source.symbols[0];
    expect(ToLower.name).toBe(`ToLower`);
    expect(Object.keys(ToLower.references).length).toBe(1);
    expect(ToLower.references[ToLower.relativePath].length).toBe(2);
  });
});
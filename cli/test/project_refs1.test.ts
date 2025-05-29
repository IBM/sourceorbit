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
});
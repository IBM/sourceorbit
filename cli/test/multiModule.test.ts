import { beforeAll, describe, expect, test } from 'vitest';

import { Targets } from '../src/targets'
import path from 'path';
import { MakeProject } from '../src/builders/make';
import { setupMultiModule } from './fixtures/projects';
import { writeFileSync } from 'fs';
import { ReadFileSystem } from '../src/readFileSystem';

const cwd = setupMultiModule();

describe(`multi_module tests`, () => {
  const fs = new ReadFileSystem();
  const targets = new Targets(cwd, fs);
  
  beforeAll(async () => {
    await targets.loadProject();

    targets.resolveBinder();
  });

  test(`Check objects are generated`, async () => {
    expect(targets.getResolvedObjects().length).toBe(4);
    expect(targets.getTargets().length).toBe(4);
    expect(targets.getTargetsOfType(`FILE`).length).toBe(0);
    expect(targets.getTargetsOfType(`PGM`).length).toBe(1);
    expect(targets.getTargetsOfType(`MODULE`).length).toBe(3);
    expect(targets.getTargetsOfType(`SRVPGM`).length).toBe(0);
  });

  test(`Check program`, async () => {
    const gitBrg = targets.getTarget({systemName: `GITBRG`, type: `PGM`});
    expect(gitBrg).toBeDefined();

    const deps = gitBrg.deps;
    expect(deps.length).toBe(2);
    
    expect(deps.find(d => d.systemName === `GITBRG` && d.type === `MODULE`)).toBeDefined();
    expect(deps.find(d => d.systemName === `UTILS` && d.type === `MODULE`)).toBeDefined();
    expect(deps.find(d => d.systemName === `OBJECTS` && d.type === `MODULE`)).toBeUndefined();
  });

  test(`Check solo module`, async () => {
    const objectsMod = targets.getTarget({systemName: `OBJECTS`, type: `MODULE`});
    expect(objectsMod).toBeDefined();

    expect(objectsMod.deps.length).toBe(0);
  });

  test(`Generate makefile`, () => {
    const makeProj = new MakeProject(cwd, targets);

    writeFileSync(path.join(cwd, `makefile`), makeProj.getMakefile().join(`\n`));
  });
});
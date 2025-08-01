import { beforeAll, describe, expect, test } from 'vitest';

import { Targets } from '../src/targets'
import { setupFixture } from './fixtures/projects';
import { ReadFileSystem } from '../src/readFileSystem';

describe(`CL with DCLF`, () => {
  const project = setupFixture(`cldclf`);
  
  const fs = new ReadFileSystem();
  const targets = new Targets(project.cwd, fs);

  beforeAll(async () => {
    project.setup();
    await targets.loadProject();

    expect(targets.getTargets().length).toBeGreaterThan(0);
    targets.resolveBinder();
  });

  test(`Objects are loaded`, () => {
    expect(targets).toBeDefined();
    expect(targets.binderRequired()).toBeFalsy();

    const targetObjects = targets.getTargets();

    expect(targetObjects.length).toBe(2);

    expect(targetObjects.some(t => t.systemName === `APGM` && t.type === `PGM` && t.extension === `clle`)).toBeTruthy();
    expect(targetObjects.some(t => t.systemName === `DEPARTMENT` && t.type === `FILE` && t.extension === `table`)).toBeTruthy();
  });

  test(`CL has valid dependency`, () => {
    const apgm = targets.getTarget({systemName: `APGM`, type: `PGM`});
    expect(apgm).toBeDefined();

    const logs = targets.logger.getLogsFor(apgm.relativePath);
    expect(logs.length).toBe(0);

    expect(apgm.deps.length).toBe(1);
    expect(apgm.deps[0].systemName).toBe(`DEPARTMENT`);
    expect(apgm.deps[0].type).toBe(`FILE`);
  });
});
import { beforeAll, describe, expect, test } from 'vitest';

import { Targets } from '../src/targets'
import { setupFixture } from './fixtures/projects';
import { ReadFileSystem } from '../src/readFileSystem';

const cwd = setupFixture(`override_objref`);

// This issue was occuring when you had two files with the same name, but different extensions.

describe(`assume programs tests`, () => {
 const project = setupFixture(`assume_pgms_copy`);

  const fs = new ReadFileSystem();
  const targets = new Targets(project.cwd, fs);
  targets.setAssumePrograms(true);

  beforeAll(async () => {
    project.setup();
    await targets.loadProject();

    expect(targets.getTargets().length).toBeGreaterThan(0);
    targets.resolveBinder();
  });

  test(`Ensure copybooks are not objects`, async () => {
    const resolved = targets.getResolvedObjects();
    const targetObjects = targets.getTargets();
    expect(resolved.length).toBe(1);
    expect(targetObjects.length).toBe(1);
  })
});
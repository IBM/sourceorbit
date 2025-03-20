import { beforeAll, describe, expect, test } from 'vitest';

import { Targets } from '../src/targets'
import { setupFixture } from './fixtures/projects';
import { ReadFileSystem } from '../src/readFileSystem';

const fs = new ReadFileSystem();

describe(`long name test`, () => {
  const project = setupFixture(`bob_long_names`);
  
  const targets = new Targets(project.cwd, fs);
  targets.setSuggestions({ renames: true, includes: true })

  beforeAll(async () => {
    project.setup();
    await targets.loadProject();

    expect(targets.getTargets().length).toBeGreaterThan(0);
    targets.resolveBinder();
  });

  test(`Ensure objects are defined`, async () => {
    expect(targets.getTargets().length).toBe(1);
    expect(targets.getResolvedObjects(`FILE`).length).toBe(1);
    expect(targets.binderRequired()).toBe(false);

    const dspf = targets.searchForObject({ systemName: `ART301D`, type: `FILE` });
    expect(dspf).toBeDefined();
  });
});
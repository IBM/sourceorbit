import { beforeAll, describe, expect, test } from 'vitest';

import { Targets } from '../src/targets'
import { setupFixture } from './fixtures/projects';
import { ReadFileSystem } from '../src/readFileSystem';

// This issue was occuring when you had two files with the same name, but different extensions.

describe(`sql long name lookup`, () => {
  const project = setupFixture(`sql_long_names`);

  const fs = new ReadFileSystem();
  const targets = new Targets(project.cwd, fs);
  targets.setSuggestions({ renames: true, includes: true })

  beforeAll(async () => {
    project.setup();
    await targets.loadProject();

    expect(targets.getTargets().length).toBeGreaterThan(0);
    targets.resolveBinder();
  });

  test(`Ensure objects are defined`, async () => {
    expect(targets.getTargets().length).toBe(2);
    expect(targets.getResolvedObjects(`FILE`).length).toBe(1);
    expect(targets.getResolvedObjects(`MODULE`).length).toBe(1);
    expect(targets.binderRequired()).toBe(false);

    const trans = targets.searchForObject({ systemName: `TRANS`, type: `FILE` });
    expect(trans).toBeDefined();

    const transaction = targets.searchForObject({ systemName: `TRANSACTION`, type: `FILE` });
    expect(transaction).toBeDefined();

    expect(trans).toMatchObject(transaction);

    const moduleLogs = targets.logger.getLogsFor(trans.relativePath);
    expect(moduleLogs).toBeUndefined();
  });

  test(`Ensure deps are correct`, async () => {
    const trans = targets.getTarget({ systemName: `DB`, type: `MODULE` });
    expect(trans).toBeDefined();

    expect(trans.deps.length).toBe(1);
    expect(trans.deps[0].systemName).toBe(`TRANS`);
    expect(trans.deps[0].longName).toBe(`TRANSACTION`);
  });

});
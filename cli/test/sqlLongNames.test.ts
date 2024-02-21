import { assert, beforeAll, describe, expect, test } from 'vitest';

import { Targets } from '../src/targets'
import path from 'path';
import { MakeProject } from '../src/builders/make';
import { getFiles } from '../src/utils';
import { setupFixture } from './fixtures/projects';
import { scanGlob } from '../src/extensions';

const cwd = setupFixture(`sql_long_names`);

// This issue was occuring when you had two files with the same name, but different extensions.

let files = getFiles(cwd, scanGlob);

describe.skipIf(files.length === 0)(`sql long name lookup`, () => {
  const targets = new Targets(cwd);
  targets.setSuggestions({ renames: true, includes: true })

  beforeAll(async () => {
    targets.loadObjectsFromPaths(files);
    const parsePromises = files.map(f => targets.parseFile(f));
    await Promise.all(parsePromises);

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

});
import { assert, beforeAll, describe, expect, test } from 'vitest';

import { Targets } from '../src/targets'
import path from 'path';
import { MakeProject } from '../src/builders/make';
import { getFiles } from '../src/utils';
import { setupFixture } from './fixtures/projects';
import { scanGlob } from '../src/extensions';
import { writeFileSync } from 'fs';
import { BobProject } from '../src';

const cwd = setupFixture(`bob_long_names`);

let files = getFiles(cwd, scanGlob);

describe.skipIf(files.length === 0)(`long name test`, () => {
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
    expect(targets.getTargets().length).toBe(1);
    expect(targets.getResolvedObjects(`FILE`).length).toBe(1);
    expect(targets.binderRequired()).toBe(false);

    const dspf = targets.searchForObject({ systemName: `ART301D`, type: `FILE` });
    expect(dspf).toBeDefined();
  });
});
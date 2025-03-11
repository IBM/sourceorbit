import { assert, beforeAll, describe, expect, test } from 'vitest';

import { Targets } from '../src/targets'
import path from 'path';
import { MakeProject } from '../src/builders/make';
import { getFiles } from '../src/utils';
import { setupFixture } from './fixtures/projects';
import { referencesFileName, scanGlob } from '../src/extensions';
import { writeFileSync } from 'fs';
import { BobProject } from '../src/builders/bob';

const cwd = setupFixture(`override_objref`);

// This issue was occuring when you had two files with the same name, but different extensions.

let files = getFiles(cwd, scanGlob);

describe(`ensure that objrefs can be overridden`, () => {
  const targets = new Targets(cwd);
  targets.setSuggestions({renames: true, includes: true})

  test(`Ensure objects are defined`, async () => {
    await targets.handleRefsFile(path.join(cwd, referencesFileName));
    expect(targets.getResolvedObjects().length).toBe(1);
    expect(targets.getTargets().length).toBe(0);

    targets.loadObjectsFromPaths(files);
    const parsePromises = files.map(f => targets.parseFile(f));
    await Promise.all(parsePromises);

    expect(targets.getTargets().length).toBeGreaterThan(0);
    targets.resolveBinder();

    expect(targets.getTargets().length).toBe(4);

    expect(targets.getResolvedObjects().length).toBe(4);
    expect(targets.getResolvedObjects(`FILE`).length).toBe(4);

    expect(targets.binderRequired()).toBe(false);

    const pro250d = targets.searchForObject({systemName: `PRO250D`, type: `FILE`});
    expect(pro250d).toBeDefined();
    expect(pro250d.reference).toBeUndefined();

    const provider = targets.searchForObject({systemName: `PROVIDER`, type: `FILE`});
    expect(provider).toBeDefined();
    expect(provider.reference).toBeUndefined();

    const provide1 = targets.searchForObject({systemName: `PROVIDE1`, type: `FILE`});
    expect(provide1).toBeDefined();
    expect(provide1.reference).toBeUndefined();

    const samref = targets.searchForObject({systemName: `SAMREF`, type: `FILE`});
    expect(samref).toBeDefined();
    expect(samref.reference).toBeUndefined();
  });
});
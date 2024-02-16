import { assert, beforeAll, describe, expect, test } from 'vitest';

import { Targets } from '../src/targets'
import path from 'path';
import { MakeProject } from '../src/builders/make';
import { getFiles } from '../src/utils';
import { setupFixture } from './fixtures/projects';
import { referencesFileName, scanGlob } from '../src/extensions';
import { writeFileSync } from 'fs';

const cwd = setupFixture(`dds_refs_with_refs`);

// This issue was occuring when you had two files with the same name, but different extensions.

let files = getFiles(cwd, scanGlob);

describe.skipIf(files.length === 0)(`dds_refs tests with reference file`, () => {
  const targets = new Targets(cwd);
  targets.setSuggestions({renames: true, includes: true})
  
  beforeAll(async () => {
    targets.handleRefsFile(path.join(cwd, referencesFileName));
    targets.loadObjectsFromPaths(files);
    const parsePromises = files.map(f => targets.parseFile(f));
    await Promise.all(parsePromises);

    expect(targets.getTargets().length).toBeGreaterThan(0);
    targets.resolveBinder();
  });

  test(`Ensure objects are defined`, async () => {
    expect(targets.getTargets().length).toBe(3);
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
    expect(samref.reference).toBeTruthy();
  });

  test(`test PROD250D deps (REF & 32REFFLD)`, async () => {
    const pro250d = targets.getTarget({systemName: `PRO250D`, type: `FILE`});

    expect(pro250d).toBeDefined();
    const deps = pro250d.deps;
    expect(deps.length).toBe(1);
    expect(deps[0].systemName).toBe(`PROVIDER`);

    const logs = targets.logger.getLogsFor(pro250d.relativePath);
    expect(logs.length).toBe(1);
    expect(logs[0]).toMatchObject({
      message: `no object found for reference 'COUNTRY'`,
      type: `warning`,
      line: 32
    });
  });

  test(`test PROVIDER deps (REF)`, async () => {
    const provider = targets.getTarget({systemName: `PROVIDER`, type: `FILE`});

    expect(provider).toBeDefined();
    const deps = provider.deps;
    expect(deps.length).toBe(1);

    const logs = targets.logger.getLogsFor(provider.relativePath);
    expect(logs).toBeUndefined();
  });

  test(`test PROVIDE1 deps (REF)`, async () => {
    const providerLf = targets.getTarget({systemName: `PROVIDE1`, type: `FILE`});

    expect(providerLf).toBeDefined();
    const deps = providerLf.deps;
    expect(deps.length).toBe(1);

    const logs = targets.logger.getLogsFor(providerLf.relativePath);
    expect(logs).toBeUndefined();
  });
});
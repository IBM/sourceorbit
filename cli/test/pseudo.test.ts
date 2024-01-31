import { beforeAll, describe, expect, test } from 'vitest';

import { Targets } from '../src/targets'
import { MakeProject } from '../src/builders/make';
import { getFiles } from '../src/utils';
import { setupPseudo } from './fixtures/projects';
import { scanGlob } from '../src/extensions';

const cwd = setupPseudo();

const makeDefaults = MakeProject.getDefaultSettings();

let files = getFiles(cwd, scanGlob);

describe.skipIf(files.length === 0)(`psuedo tests`, () => {
  const targets = new Targets(cwd);
  beforeAll(async () => {
    targets.loadObjectsFromPaths(files);
    const parsePromises = files.map(f => targets.parseFile(f));
    await Promise.all(parsePromises);

    expect(targets.getTargets().length).toBeGreaterThan(0);
    targets.resolveBinder();
  });

  test(`Test DTAARA exists`, () => {
    expect(targets.searchForObject({ systemName: `MYTHING`, type: `DTAARA` }, undefined)).toBeDefined();
  });

  test(`Program depends on DTAARA`, () => {
    const programTarget = targets.getTarget({ systemName: `TESTER`, type: `PGM` });

    expect(programTarget).toBeDefined();
    expect(programTarget.deps.length).toBe(1);
    expect(programTarget.deps[0].systemName).toBe(`MYTHING`);
  });
});

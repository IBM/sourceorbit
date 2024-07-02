import { beforeAll, describe, expect, test } from 'vitest';

import { Targets } from '../src/targets'
import path from 'path';
import { getFiles } from '../src/utils';
import { setupFixture, setupSqlReferencesSystem } from './fixtures/projects';
import { scanGlob } from '../src/extensions';

const cwd = setupFixture(`sql_ref_with`)

let files = getFiles(cwd, scanGlob);

async function setupScopeAnalysis(targets: Targets) {
  targets.loadObjectsFromPaths(files);
  const parsePromises = files.map(f => targets.parseFile(f));
  await Promise.all(parsePromises);

  expect(targets.getTargets().length).toBeGreaterThan(0);
  targets.resolveBinder();
}

describe.skipIf(files.length === 0)(`sql_references_with tests`, () => {
  const targets = new Targets(cwd);

  beforeAll(async () => {
    await setupScopeAnalysis(targets);
  });

  test(`SQL with clause`, async () => {
    const myPgm = targets.getTarget({ systemName: `SQLWITHPGM`, type: `PGM` });
    expect(myPgm.relativePath).toBe(path.join(`sqlwithpgm.pgm.sqlrpgle`));
    
    const moduleLogs = targets.logger.getLogsFor(myPgm.relativePath);
    expect(moduleLogs.length).toBe(1);
    expect(moduleLogs[0].message).toBe(`No object found for reference 'TABLE1'`);
  });
});
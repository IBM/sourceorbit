import { beforeAll, describe, expect, test } from 'vitest';

import { Targets } from '../src/targets'
import path from 'path';
import { setupFixture } from './fixtures/projects';
import { ReadFileSystem } from '../src/readFileSystem';

async function setupScopeAnalysis(targets: Targets) {
  await targets.loadProject();

  expect(targets.getTargets().length).toBeGreaterThan(0);
  targets.resolveBinder();
}

describe(`sql_references_with tests`, () => {
  const project = setupFixture(`sql_ref_with`)

  const fs = new ReadFileSystem();
  const targets = new Targets(project.cwd, fs);

  beforeAll(async () => {
    project.setup();
    await setupScopeAnalysis(targets);
  });

  test(`SQL with clause`, async () => {
    const myPgm = targets.getTarget({ systemName: `SQLWITHPGM`, type: `PGM` });
    expect(myPgm.source.relativePath).toBe(path.join(`sqlwithpgm.pgm.sqlrpgle`));
    
    const moduleLogs = targets.logger.getLogsFor(myPgm.source.relativePath);
    expect(moduleLogs.length).toBe(1);
    expect(moduleLogs[0].message).toBe(`No object found for reference 'TABLE1'`);
  });
});
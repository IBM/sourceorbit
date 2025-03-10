import { beforeAll, describe, expect, test } from 'vitest';

import { Targets } from '../src/targets'
import path from 'path';
import { setupSqlReferencesSystem } from './fixtures/projects';
import { ReadFileSystem } from '../src/readFileSystem';

const cwd = setupSqlReferencesSystem();

async function setupScopeAnalysis(targets: Targets) {
  await targets.loadProject();

  expect(targets.getTargets().length).toBeGreaterThan(0);
  targets.resolveBinder();
}

describe(`sql_references tests (internal scope analysis)`, () => {
  const fs = new ReadFileSystem();
  const targets = new Targets(cwd, fs);

  beforeAll(async () => {
    await setupScopeAnalysis(targets);
  });

  test(`Check stock (with internal scope analysis)`, async () => {
    const myPgm = targets.getTarget({ systemName: `SQLREFPGM`, type: `PGM` });
    expect(myPgm.relativePath).toBe(path.join(`qrpglesrc`, `sqlrefpgm.pgm.sqlrpgle`));
    expect(myPgm.deps.length).toBe(1);

    const empTable = myPgm.deps[0];
    expect(empTable.systemName).toBe(`STOCK`);
    expect(empTable.type).toBe(`FILE`);
    expect(empTable.relativePath).toBe(path.join(`qddssrc`, `stock.table`));
  });
});
import { beforeAll, describe, expect, test } from 'vitest';

import { Targets } from '../src/targets'
import path from 'path';
import { getFiles } from '../src/utils';
import { setupSqlReferencesSystem } from './fixtures/projects';
import { scanGlob } from '../src/extensions';

const cwd = setupSqlReferencesSystem();

let files = getFiles(cwd, scanGlob);

async function setupScopeAnalysis(targets: Targets) {
  targets.loadObjectsFromPaths(files);
  const parsePromises = files.map(f => targets.parseFile(f));
  await Promise.all(parsePromises);

  expect(targets.getTargets().length).toBeGreaterThan(0);
  targets.resolveBinder();
}

describe.skipIf(files.length === 0)(`sql_references tests (internal scope analysis)`, () => {
  const targets = new Targets(cwd);

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
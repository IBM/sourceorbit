import { beforeAll, describe, expect, test } from 'vitest';

import { Targets } from '../src/targets'
import path from 'path';
import { getFiles } from '../src/utils';
import { setupFixture } from './fixtures/projects';
import { scanGlob } from '../src/extensions';

const cwd = setupFixture(`mixedCaseExport`);

let files = getFiles(cwd, scanGlob);

async function setupScopeAnalysis(targets: Targets) {
  targets.loadObjectsFromPaths(files);
  const parsePromises = files.map(f => targets.parseFile(f));
  await Promise.all(parsePromises);

  expect(targets.getTargets().length).toBeGreaterThan(0);
  targets.resolveBinder();
}

describe.skipIf(files.length === 0)(`pr with mixed case exports exports `, () => {
  const targets = new Targets(cwd);

  beforeAll(async () => {
    await setupScopeAnalysis(targets);
  });

  test(`Correct check exports no matter the casing`, async () => {
    const allLogs = targets.logger.getAllLogs();

    const [srvPgmObj] = targets.getResolvedObjects(`SRVPGM`);
    expect(srvPgmObj).toBeDefined();
    expect(srvPgmObj.systemName).toBe(`MODEXCEPT`);
    expect(srvPgmObj.type).toBe(`SRVPGM`);

    const srvPgmTarget = targets.getTarget({ systemName: `MODEXCEPT`, type: `SRVPGM` });
    expect(srvPgmTarget).toBeDefined();

    expect(srvPgmTarget.deps.length).toBe(1);

    expect(srvPgmTarget.exports.length).toBe(3);
    expect(srvPgmTarget.exports).toStrictEqual(srvPgmTarget.deps[0].exports);

    expect(allLogs[srvPgmObj.relativePath].length).toBe(0);
  });
});
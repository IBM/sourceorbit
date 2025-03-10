import { beforeAll, describe, expect, test } from 'vitest';

import { Targets } from '../src/targets'
import { setupFixture } from './fixtures/projects';
import { ReadFileSystem } from '../src/readFileSystem';

const cwd = setupFixture(`mixedCaseExport`);

async function setupScopeAnalysis(targets: Targets) {
  await targets.loadProject();

  expect(targets.getTargets().length).toBeGreaterThan(0);
  targets.resolveBinder();
}

describe(`pr with mixed case exports exports `, () => {
  const fs = new ReadFileSystem();
  const targets = new Targets(cwd, fs);

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
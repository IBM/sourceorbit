import { assert, beforeAll, describe, expect, test } from 'vitest';

import { Targets } from '../src/targets'
import path from 'path';
import { MakeProject } from '../src/builders/make';
import { getFiles } from '../src/utils';
import { setupFixture } from './fixtures/projects';
import { scanGlob } from '../src/extensions';
import { writeFileSync } from 'fs';

const cwd = setupFixture(`include_mismatch_fix`);

// This issue was occuring when you had two files with the same name, but different extensions.

let files = getFiles(cwd, scanGlob);

describe.skipIf(files.length === 0)(`include_mismatch_fix tests`, () => {
  const targets = new Targets(cwd);
  targets.setSuggestions({renames: true, includes: true})
  
  beforeAll(async () => {
    targets.loadObjectsFromPaths(files);
    const parsePromises = files.map(f => targets.parseFile(f));
    await Promise.all(parsePromises);

    expect(targets.getTargets().length).toBeGreaterThan(0);
    targets.resolveBinder();
  });

  test(`Ensure rename is against correct file`, async () => {
    const articlePf = targets.getTarget({systemName: `ARTICLE`, type: `FILE`});
    expect(articlePf).toBeDefined();

    const articlePfLogs = targets.logger.getLogsFor(articlePf.relativePath);
    expect(articlePfLogs.length).toBe(1);
    expect(articlePfLogs[0].message).toBe(`no object found for reference 'SAMREF'`);
    expect(articlePfLogs[0].type).toBe(`warning`);

    const articleIncludeLogs = targets.logger.getLogsFor(`QPROTOSRC/ARTICLE.RPGLE`);
    expect(articleIncludeLogs.length).toBe(1);
    expect(articleIncludeLogs[0].message).toBe(`Rename suggestion`);
    expect(articleIncludeLogs[0].type).toBe(`rename`);
    expect(articleIncludeLogs[0].change).toMatchObject({
      rename: {
        path: path.join(cwd, `QPROTOSRC`, `ARTICLE.RPGLE`),
        newName: 'ARTICLE.rpgleinc'
      }
    })
  });
});
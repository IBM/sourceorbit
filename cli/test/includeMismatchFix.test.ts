import { beforeAll, describe, expect, test } from 'vitest';

import { Targets } from '../src/targets'
import path from 'path';
import { setupFixture } from './fixtures/projects';
import { ReadFileSystem } from '../src/readFileSystem';

// This issue was occuring when you had two files with the same name, but different extensions.

describe(`include_mismatch_fix tests`, () => {
  const project = setupFixture(`include_mismatch_fix`);

  const fs = new ReadFileSystem();
  const targets = new Targets(project.cwd, fs);
  targets.setSuggestions({renames: true, includes: true})
  
  beforeAll(async () => {
    project.setup();
    await targets.loadProject();

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

    const articleIncludeLogs = targets.logger.getLogsFor(path.join(`QPROTOSRC`, `ARTICLE.RPGLE`));
    expect(articleIncludeLogs.length).toBe(1);
    expect(articleIncludeLogs[0].message).toBe(`Rename suggestion`);
    expect(articleIncludeLogs[0].type).toBe(`rename`);
    expect(articleIncludeLogs[0].change).toMatchObject({
      rename: {
        path: path.join(project.cwd, `QPROTOSRC`, `ARTICLE.RPGLE`),
        newName: 'ARTICLE.rpgleinc'
      }
    })
  });
});
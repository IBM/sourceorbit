import { beforeAll, describe, expect, test } from 'vitest';

import { Targets } from '../src/targets'
import { MakeProject } from '../src/builders/make';
import { setupFixture } from './fixtures/projects';
import { BobProject } from '../src/builders/bob';
import { ReadFileSystem } from '../src/readFileSystem';

// This issue was occuring when you had two files with the same name, but different extensions.

describe(`dds_refs tests with reference file`, () => {
  const project = setupFixture(`dds_deps_with_refs`);

  const fs = new ReadFileSystem();
  const targets = new Targets(project.cwd, fs);
  targets.setSuggestions({renames: true, includes: true})
  
  beforeAll(async () => {
    await targets.loadProject({withRef: `.objrefs`});

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


  test(`make doesn't include refs that do not exist or are referenced objects`, async () => {
    const makeProject = new MakeProject(project.cwd, targets, fs);
    await makeProject.setupSettings();

    const targetContent = makeProject.generateTargets();

    expect(targetContent).toContain(`$(PREPATH)/PROVIDE1.FILE: $(PREPATH)/PROVIDER.FILE`);
    expect(targetContent).toContain(`$(PREPATH)/PRO250D.FILE: $(PREPATH)/PROVIDER.FILE`);
    expect(targetContent).not.toContain(`$(PREPATH)/PROVIDER.FILE: $(PREPATH)/SAMREF.FILE`);
  });

  test(`bob doesn't include refs that do not exist or are referenced objects`, () => {
    const bobProject = new BobProject(targets);

    const files = bobProject.createRules();

    expect(files[`Rules.mk`]).toBeDefined();

    const baseRules = files[`Rules.mk`].split('\n').map(l => l.trim());

    expect(baseRules).toContain(`PRO250D.FILE: PRO250D.DSPF PROVIDER.FILE`);
    expect(baseRules).toContain(`PROVIDER.FILE: PROVIDER.PF`);
    expect(baseRules).toContain(`PROVIDE1.FILE: PROVIDE1.LF PROVIDER.FILE`);
  });
});
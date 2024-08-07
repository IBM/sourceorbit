import { assert, beforeAll, describe, expect, test } from 'vitest';

import { Targets } from '../src/targets'
import path from 'path';
import { MakeProject } from '../src/builders/make';
import { getFiles } from '../src/utils';
import { setupFixture, setupMultiModule } from './fixtures/projects';
import { scanGlob } from '../src/extensions';
import { writeFileSync } from 'fs';

const cwd = setupFixture(`multi_module_two`);

let files = getFiles(cwd, scanGlob);

describe.skipIf(files.length === 0)(`multi_module_two tests`, () => {
  const targets = new Targets(cwd);
  
  beforeAll(async () => {
    targets.loadObjectsFromPaths(files);
    for (const f of files) {
      await targets.parseFile(f);
    }

    expect(targets.getTargets().length).toBeGreaterThan(0);
    targets.resolveBinder();
  });

  test(`Check objects are generated`, async () => {
    expect(targets.getResolvedObjects().length).toBe(5);
    expect(targets.getTargets().length).toBe(5);
    expect(targets.getTargetsOfType(`FILE`).length).toBe(1);
    expect(targets.getTargetsOfType(`PGM`).length).toBe(1);
    expect(targets.getTargetsOfType(`MODULE`).length).toBe(3);
    expect(targets.getTargetsOfType(`SRVPGM`).length).toBe(0);
  });

  test(`Check program`, async () => {
    const runnerPgm = targets.getTarget({systemName: `RUNNER`, type: `PGM`});
    expect(runnerPgm).toBeDefined();

    const deps = runnerPgm.deps;
    expect(deps.length).toBe(3);

    expect(deps.some(d => d.systemName === `DB`)).toBeTruthy();
    expect(deps.some(d => d.systemName === `RUNNER`)).toBeTruthy();
    expect(deps.some(d => d.systemName === `DATA`)).toBeTruthy();
  });

  test(`Check data module`, async () => {
    const dataModule = targets.getTarget({systemName: `DB`, type: `MODULE`});
    expect(dataModule).toBeDefined();

    const deps = dataModule.deps;
    expect(deps.length).toBe(2);

    expect(deps.some(d => d.systemName === `CUSTOMER`)).toBeTruthy();
    expect(deps.some(d => d.systemName === `DATA`)).toBeTruthy();
  });

  test(`Check makefile result`, async () => {
    const project = new MakeProject(cwd, targets);

    const targetContent = project.getMakefile();

    const runnerTarget = targetContent.find(t => t.startsWith(`$(PREPATH)/RUNNER.PGM: `) && t.length > 50);
    expect(runnerTarget).toBeDefined();
    const runnerDepsString = runnerTarget.substring(runnerTarget.indexOf(`: `) + 2).split(` `);
    expect(runnerDepsString.length).toBe(3);
    expect(runnerDepsString).toContain(`$(PREPATH)/DB.MODULE`);
    expect(runnerDepsString).toContain(`$(PREPATH)/RUNNER.MODULE`);
    expect(runnerDepsString).toContain(`$(PREPATH)/DATA.MODULE`);

    expect(targetContent).toContain(`\tsystem "CRTPGM PGM($(BIN_LIB)/RUNNER) ENTMOD(RUNNER) MODULE(DB RUNNER DATA) TGTRLS(*CURRENT) BNDDIR($(BNDDIR)) ACTGRP(*NEW)" > .logs/runner.splf`);
    expect(targetContent).toContain(`$(PREPATH)/RUNNER.MODULE: rpgle/runner.pgm.rpgle`);
  });
});
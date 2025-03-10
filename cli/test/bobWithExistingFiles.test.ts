import { beforeAll, describe, expect, test } from 'vitest';

import { Targets } from '../src/targets'
import path from 'path';
import { setupFixture } from './fixtures/projects';
import { BobProject } from '../src';
import { ReadFileSystem } from '../src/readFileSystem';

const cwd = setupFixture(`pseudo`);

// This issue was occuring when you had two files with the same name, but different extensions.


describe(`bob Rules.mk tests`, () => {
  const fs = new ReadFileSystem();
  const targets = new Targets(cwd, fs);
  targets.setSuggestions({ renames: true, includes: true })

  beforeAll(async () => {
    await targets.loadProject();

    expect(targets.getTargets().length).toBeGreaterThan(0);
    targets.resolveBinder();
  });

  test(`bob to not overwrite any pre-existing rules for targets`, () => {
    const project = new BobProject(targets);

    const files = project.createRules();

    const baseRules = files[`Rules.mk`].split(/\r?\n/);

    expect(baseRules).toBeDefined();
    expect(baseRules[0]).toContain(`SUBDIRS =`);
    expect(baseRules[0]).toContain(`qobjs`);
    expect(baseRules[0]).toContain(`qrpglesrc`);

    const qobjRules = files[path.join(`qobjs`, `Rules.mk`)].split(/\r?\n/);

    expect(qobjRules).toBeDefined();
    expect(qobjRules).toContain(`MYTHING.DTAARA:text=Hello world`);
    expect(qobjRules).toContain(`MSTDSP.FILE: mstdsp.dspf`);

    const qrpglesrcRules = files[path.join(`qrpglesrc`, `Rules.mk`)].split(/\r?\n/);

    expect(qrpglesrcRules).toBeDefined();
    expect(qrpglesrcRules).toContain(`TESTER.PGM: tester.pgm.rpgle MYTHING.DTAARA`);
    expect(qrpglesrcRules).toContain(`TESTER.PGM: text = My program`);
    expect(qrpglesrcRules).toContain(`# Other assignment`);
    expect(qrpglesrcRules).toContain(`TESTER.PGM: bnddir:=MYBND`);
    expect(qrpglesrcRules).toContain(`OTHER.PGM: other.pgm.sqlrpgle MSTDSP.FILE`);
  });
});
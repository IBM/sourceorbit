import { assert, beforeAll, describe, expect, test } from 'vitest';

import { Targets } from '../src/targets'
import path from 'path';
import { MakeProject } from '../src/builders/make';
import { getFiles } from '../src/utils';
import { setupFixture } from './fixtures/projects';
import { scanGlob } from '../src/extensions';
import { writeFileSync } from 'fs';
import { BobProject } from '../src';

const cwd = setupFixture(`pseudo`);

// This issue was occuring when you had two files with the same name, but different extensions.

let files = getFiles(cwd, scanGlob);

describe.skipIf(files.length === 0)(`bob Rules.mk tests`, () => {
  const targets = new Targets(cwd);
  targets.setSuggestions({ renames: true, includes: true })

  beforeAll(async () => {
    targets.loadObjectsFromPaths(files);
    const parsePromises = files.map(f => targets.parseFile(f));
    await Promise.all(parsePromises);

    expect(targets.getTargets().length).toBeGreaterThan(0);
    targets.resolveBinder();
  });

  test(`bob to not overwrite any pre-existing rules for targets`, () => {
    const project = new BobProject(targets);

    const files = project.createRules();

    const baseRules = files[`Rules.mk`].split(`\n`);

    expect(baseRules).toBeDefined();
    expect(baseRules[0]).toContain(`SUBDIRS =`);
    expect(baseRules[0]).toContain(`qobjs`);
    expect(baseRules[0]).toContain(`qrpglesrc`);

    const qobjRules = files[`qobjs/Rules.mk`].split(`\n`);

    expect(qobjRules).toBeDefined();
    expect(qobjRules).toContain(`MYTHING.DTAARA:text=Hello world`);
    expect(qobjRules).toContain(`MSTDSP.FILE: mstdsp.dspf`);

    const qrpglesrcRules = files[`qrpglesrc/Rules.mk`].split(`\n`);

    expect(qrpglesrcRules).toBeDefined();
    expect(qrpglesrcRules).toContain(`TESTER.PGM: tester.pgm.rpgle MYTHING.DTAARA`);
    expect(qrpglesrcRules).toContain(`TESTER.PGM: text = My program`);
    expect(qrpglesrcRules).toContain(`# Other assignment`);
    expect(qrpglesrcRules).toContain(`TESTER.PGM: bnddir:=MYBND`);
    expect(qrpglesrcRules).toContain(`OTHER.PGM: other.pgm.sqlrpgle MSTDSP.FILE`);
  });
});
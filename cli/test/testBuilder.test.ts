import { beforeAll, describe, expect, test } from 'vitest';

import { Targets } from '../src/targets'
import { MakeProject } from '../src/builders/make';
import { getFiles } from '../src/utils';
import { setupPseudo, setupTestBuilderSuite } from './fixtures/projects';
import { scanGlob } from '../src/extensions';
import { TestBuilder } from '../src/tester';

const cwd = setupTestBuilderSuite();

let files = getFiles(cwd, scanGlob);

describe.skipIf(files.length === 0)(`TestBuilder tests`, () => {
  const targets = new Targets(cwd);
  beforeAll(async () => {
    targets.loadObjectsFromPaths(files);
    const parsePromises = files.map(f => targets.parseFile(f));
    await Promise.all(parsePromises);

    expect(targets.getTargets().length).toBeGreaterThan(0);
    targets.resolveBinder();
  });

  test(`Check base object count`, () => {
    const modules = targets.getResolvedObjects(`MODULE`);
    expect(modules.length).toBe(5);
    expect(targets.getResolvedObjects(`SRVPGM`).length).toBe(2);

    expect(modules.filter(m => m.testModule === true).length).toBe(3);
  });

  test(`Check deps`, () => {
    const bankingSrvPgm = targets.searchForObject({systemName: `BANKING`, type: `SRVPGM`}, undefined);
    expect(bankingSrvPgm).toBeDefined();

    const bankingTarget = targets.getTarget(bankingSrvPgm);
    expect(bankingTarget).toBeDefined();
    expect(bankingTarget?.deps.length).toBe(1);
    expect(bankingTarget?.deps[0].systemName).toBe(`BANKING`);
    expect(bankingTarget?.deps[0].type).toBe(`MODULE`);

    const utilsSrvPgm = targets.searchForObject({systemName: `UTILS`, type: `SRVPGM`}, undefined);
    expect(utilsSrvPgm).toBeDefined();

    const utilsTarget = targets.getTarget(utilsSrvPgm);
    expect(utilsTarget).toBeDefined();
    expect(utilsTarget?.deps.length).toBe(1);
    expect(utilsTarget?.deps[0].systemName).toBe(`UTILS`);
    expect(utilsTarget?.deps[0].type).toBe(`MODULE`);
  });

  test(`Generate runner`, () => {
    const testModules = targets.getResolvedObjects(`MODULE`).filter(m => m.testModule === true);
    expect(testModules.length).toBe(3);

    const testBuilder = new TestBuilder(testModules, targets.logger);
    const {newObjects: {program, module}} = testBuilder.getRunnerStructure();

    expect(module.systemName).toBe(`RUNNER`);
    expect(module.type).toBe(`MODULE`);
    expect(module.relativePath).toBe(`.so/runner.rpgle`);
    expect(module.testModule).toBe(true);

    expect(program.systemName).toBe(`RUNNER`);
    expect(program.type).toBe(`PGM`);
    expect(program.deps.length).toBe(4);

    const deps = program.deps;

    const runner = deps.find(d => d.systemName === `RUNNER`);
    const tbt = deps.find(d => d.systemName === `TBT`);
    const tUtils = deps.find(d => d.systemName === `T_UTILS`);
    const tStupid = deps.find(d => d.systemName === `T_STUPID`);

    expect(runner).toMatchObject(
      {
        extension: 'rpgle',
        relativePath: '.so/runner.rpgle',
        systemName: 'RUNNER',
        testModule: true,
        type: 'MODULE',
      }
    );

    expect(tbt).toMatchObject(
      {
        extension: 'rpgle',
        relativePath: 'qrpglesrc/bankingTest.test.rpgle',
        systemName: 'TBT',
        testModule: true,
        type: 'MODULE',
      }
    );

    expect(tUtils).toMatchObject(
      {
        extension: 'sqlrpgle',
        relativePath: 'qrpglesrc/utils.test.sqlrpgle',
        systemName: 'T_UTILS',
        testModule: true,
        type: 'MODULE',
      }
    );

    expect(tStupid).toMatchObject(
      {
        extension: 'rpgle',
        relativePath: 'qrpglesrc/stupid.test.rpgle',
        systemName: 'T_STUPID',
        testModule: true,
        type: 'MODULE',
      }
    );

    const logger = targets.logger;

    const tStupidLogs = logger.getLogsFor(tStupid.relativePath);
    expect(tStupidLogs.length).toBe(1);
    expect(tStupidLogs[0]).toMatchObject(
      { type: 'warning', message: 'No exports found in module.' }
    );

    const tUtilsLogs = logger.getLogsFor(tUtils.relativePath);
    expect(tUtilsLogs.length).toBe(1);
    expect(tUtilsLogs[0]).toMatchObject(
      { type: 'warning', message: 'No assert import found in module.' }
    );

    const tbtLogs = logger.getLogsFor(tbt.relativePath);
    expect(tbtLogs.length).toBe(0);

  });
});

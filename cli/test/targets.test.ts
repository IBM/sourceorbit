import {  expect, test } from 'vitest'
import { baseTargets } from './fixture';

test('resolveObject', () => {
  baseTargets();
});

test('createOrApend', () => {
  const targets = baseTargets(true);

  const deps = targets.getDeps();

  const programA = deps.find(d => d.name === `PROGRAMA`);
  expect(programA).toBeDefined();
  expect(programA.deps.length).toBe(2);
  expect(programA.deps[0].name).toBe(`FILEA`);
  expect(programA.deps[1].name).toBe(`PROGRAMB`);

  const programB = deps.find(d => d.name === `PROGRAMB`);
  expect(programB).toBeDefined();
  expect(programB.deps.length).toBe(0);

  const moduleA = deps.find(d => d.name === `MODULEA`);
  expect(moduleA).toBeDefined();
  expect(moduleA.deps.length).toBe(2);
  expect(moduleA.deps[0].name).toBe(`FILEA`);
  expect(moduleA.deps[1].name).toBe(`FILEB`);

  const moduleB = deps.find(d => d.name === `MODULEB`);
  expect(moduleB).toBeDefined();
  expect(moduleB.deps.length).toBe(1);
  expect(moduleB.deps[0].name).toBe(`FILEB`);
});

test('resolveBinder', () => {
  const targets = baseTargets(true);

  expect(targets.getDeps().length).toBe(10);
  expect(targets.binderRequired()).toBe(false);

  const unusedCmd = targets.getDep({name: `UNUSED`, type: `CMD`});
  const unusedSrvPgm = targets.getDep({name: `UNUSEDSRV`, type: `SRVPGM`});

  expect(unusedSrvPgm).toBeDefined();
  expect(unusedCmd).toBeDefined();

  targets.resolveBinder();

  expect(targets.getDep({name: `UNUSEDSRV`, type: `SRVPGM`})).toBeUndefined();
  expect(targets.getDep({name: `UNUSED`, type: `CMD`})).toBeUndefined();

  const unusedSrvLogs = targets.logger.getLogsFor(unusedSrvPgm.relativePath);
  expect(unusedSrvLogs.length).toBe(1);
  expect(unusedSrvLogs[0].message).toBe(`Removed as target because no modules were found with matching exports.`);
  expect(unusedSrvLogs[0].type).toBe(`info`);

  const unusedCmdLogs = targets.logger.getLogsFor(unusedCmd.relativePath);
  expect(unusedCmdLogs.length).toBe(1);
  expect(unusedCmdLogs[0].message).toBe(`Removed as target because no program was found with a matching name.`);
  expect(unusedCmdLogs[0].type).toBe(`info`);

  const deps = targets.getDeps();

  expect(deps.length).toBe(9);
  expect(targets.binderRequired()).toBe(true);

  const bnddir = deps.find(d => d.name === `$(APP_BNDDIR)` && d.type === `BNDDIR`);
  expect(bnddir).toBeDefined();
  expect(bnddir.extension).toBeUndefined();
  expect(bnddir.relativePath).toBeUndefined();
  expect(bnddir.deps.length).toBe(2);

  for (const srvPgmDep of bnddir.deps) {
    // Ensure that the deps of the bnddir exist
    const srvPgm = deps.find(d => d.name === srvPgmDep.name && d.type === srvPgmDep.type);

    expect(srvPgm).toBeDefined();
    expect(srvPgm.deps.length).toBe(1);

    expect(srvPgm.relativePath).toBeDefined();
    expect(srvPgm.extension).toBeDefined();
  }

  const programACmd = deps.find(d => d.name === `PROGRAMA` && d.type === `CMD`);
  expect(programACmd.deps.length).toBe(1);
  expect(programACmd.deps[0].name).toBe(`PROGRAMA`);
  expect(programACmd.deps[0].type).toBe(`PGM`);
});

test('getObjectsByExtension', () => {
  const targets = baseTargets(true);

  const rpglePrograms = targets.getObjectsByExtension(`pgm.rpgle`);
  expect(rpglePrograms.length).toBe(1);
  expect(rpglePrograms[0].relativePath).toBe(`qrpglesrc/programA.pgm.rpgle`);

  const rpgleModules = targets.getObjectsByExtension(`rpgle`);
  expect(rpgleModules.length).toBe(2);
  expect(rpgleModules[0].relativePath).toBe(`qrpglesrc/moduleA.rpgle`);
  expect(rpgleModules[1].relativePath).toBe(`qrpglesrc/ordentmod.rpgle`);
})
import { assert, expect, test } from 'vitest'
import { Targets } from '../../src/targets'
import path from 'path';
import { ReadFileSystem } from '../../src/readFileSystem';

export const cwd = path.join(`/`, `projects`);

export async function baseTargets(withDeps = false) {
  const fs = new ReadFileSystem();
  const targets = new Targets(cwd, fs);

  // Command object for PROGRAMA.PGM
  const programACommand = await targets.resolvePathToObject(path.join(cwd, `qcmdsrc`, `programA.cmd`));
  expect(programACommand.systemName).toBe(`PROGRAMA`);
  expect(programACommand.type).toBe(`CMD`);
  expect(programACommand.source.extension).toBe(`cmd`);
  expect(programACommand.source.relativePath).toBe(path.join(`qcmdsrc`, `programA.cmd`));

  // Command object that goes unused.
  const unusedCmd = await targets.resolvePathToObject(path.join(cwd, `qcmdsrc`, `unused.cmd`));
  expect(unusedCmd.systemName).toBe(`UNUSED`);
  expect(unusedCmd.type).toBe(`CMD`);
  expect(unusedCmd.source.extension).toBe(`cmd`);
  expect(unusedCmd.source.relativePath).toBe(path.join(`qcmdsrc`, `unused.cmd`));

  // Program object
  const programA = await targets.resolvePathToObject(path.join(cwd, `qrpglesrc`, `programA.pgm.rpgle`));
  expect(programA.systemName).toBe(`PROGRAMA`);
  expect(programA.type).toBe(`PGM`);
  expect(programA.source.extension).toBe(`rpgle`);
  expect(programA.source.relativePath).toBe(path.join(`qrpglesrc`, `programA.pgm.rpgle`));

  // Program object, imports TOLOWER
  const programB = await targets.resolvePathToObject(path.join(cwd, `qrpglesrc`, `programB.pgm.sqlrpgle`));
  expect(programB.systemName).toBe(`PROGRAMB`);
  expect(programB.type).toBe(`PGM`);
  expect(programB.source.extension).toBe(`sqlrpgle`);
  expect(programB.source.relativePath).toBe(path.join(`qrpglesrc`, `programB.pgm.sqlrpgle`));
  programB.imports = [`TOLOWER`];

  // Program object, imports TOLOWER
  const programC = await targets.resolvePathToObject(path.join(cwd, `qrpglesrc`, `programC.pgm.sqlrpgle`));
  expect(programC.systemName).toBe(`PROGRAMC`);
  expect(programC.type).toBe(`PGM`);
  expect(programC.source.extension).toBe(`sqlrpgle`);
  expect(programC.source.relativePath).toBe(path.join(`qrpglesrc`, `programC.pgm.sqlrpgle`));
  programC.imports = [`TOUPPER`];

  // Module MODULEA.MODULE, which is not used at all.
  const moduleA = await targets.resolvePathToObject(path.join(cwd, `qrpglesrc`, `moduleA.rpgle`));
  expect(moduleA.systemName).toBe(`MODULEA`);
  expect(moduleA.type).toBe(`MODULE`);
  expect(moduleA.source.extension).toBe(`rpgle`);
  expect(moduleA.source.relativePath).toBe(path.join(`qrpglesrc`, `moduleA.rpgle`));
  moduleA.exports = [`SUMNUMS`];
  
  // Module MODULEB.MODULE, which exports TOLOWER
  const moduleB = await targets.resolvePathToObject(path.join(cwd, `qrpglesrc`, `moduleB.sqlrpgle`));
  expect(moduleB.systemName).toBe(`MODULEB`);
  expect(moduleB.type).toBe(`MODULE`);
  expect(moduleB.source.extension).toBe(`sqlrpgle`);
  expect(moduleB.source.relativePath).toBe(path.join(`qrpglesrc`, `moduleB.sqlrpgle`));
  moduleB.exports = [`TOLOWER`];

  // SRVPGMA.SRVPGM, which imports TOLOWER from MODULEB.MODULE and therefore exports TOLOWER
  const srvpgmAModule = await targets.resolvePathToObject(path.join(cwd, `qsrvsrc`, `srvpgmA.bnd`));
  expect(srvpgmAModule.systemName).toBe(`SRVPGMA`);
  expect(srvpgmAModule.type).toBe(`SRVPGM`);
  expect(srvpgmAModule.source.extension).toBe(`bnd`);
  expect(srvpgmAModule.source.relativePath).toBe(path.join(`qsrvsrc`, `srvpgmA.bnd`));
  srvpgmAModule.imports = [`TOLOWER`];
  srvpgmAModule.exports = [`TOLOWER`];
  
  // FILEA.FILE
  const fileA = await targets.resolvePathToObject(path.join(cwd, `qddssrc`, `fileA.sql`));
  expect(fileA.systemName).toBe(`FILEA`);
  expect(fileA.type).toBe(`FILE`);
  expect(fileA.source.extension).toBe(`sql`);
  expect(fileA.source.relativePath).toBe(path.join(`qddssrc`, `fileA.sql`));
  
  // FILEB.FILE
  const fileB = await targets.resolvePathToObject(path.join(cwd, `qddssrc`, `fileB.pf`));
  expect(fileB.systemName).toBe(`FILEB`);
  expect(fileB.type).toBe(`FILE`);
  expect(fileB.source.extension).toBe(`pf`);
  expect(fileB.source.relativePath).toBe(path.join(`qddssrc`, `fileB.pf`));

  // ORDENTSRV.SRVPGM, which exports/imports FIXTOTALS
  const ORDENTSRV = await targets.resolvePathToObject(path.join(cwd, `qbndsrc`, `ordentsrv.binder`));
  ORDENTSRV.exports = [`FIXTOTALS`];
  ORDENTSRV.imports = [`FIXTOTALS`];
  expect(ORDENTSRV.systemName).toBe(`ORDENTSRV`);
  expect(ORDENTSRV.type).toBe(`SRVPGM`);
  expect(ORDENTSRV.source.extension).toBe(`binder`);
  expect(ORDENTSRV.source.relativePath).toBe(path.join(`qbndsrc`, `ordentsrv.binder`));

  // ORDENTMOD.MODULE which exports FIXTOTALS
  const ORDENTMOD = await targets.resolvePathToObject(path.join(cwd, `qrpglesrc`, `ordentmod.rpgle`));
  ORDENTMOD.exports = [`FIXTOTALS`];
  expect(ORDENTMOD.systemName).toBe(`ORDENTMOD`);
  expect(ORDENTMOD.type).toBe(`MODULE`);
  expect(ORDENTMOD.source.extension).toBe(`rpgle`);
  expect(ORDENTMOD.source.relativePath).toBe(path.join(`qrpglesrc`, `ordentmod.rpgle`));

  // UNUSEDSRV.SRVPGM, which exports BIGNOPE and is not used.
  const UNUSEDSRV = await targets.resolvePathToObject(path.join(cwd, `qbndsrc`, `unusedsrv.binder`));
  UNUSEDSRV.exports = [`BIGNOPE`];
  expect(UNUSEDSRV.systemName).toBe(`UNUSEDSRV`);
  expect(UNUSEDSRV.type).toBe(`SRVPGM`);
  expect(UNUSEDSRV.source.extension).toBe(`binder`);
  expect(UNUSEDSRV.source.relativePath).toBe(path.join(`qbndsrc`, `unusedsrv.binder`));

  if (withDeps) {
    targets.createOrAppend(programA, fileA);
    targets.createOrAppend(programA, programB);
    targets.createOrAppend(programB);
    targets.createOrAppend(moduleA, fileA);
    targets.createOrAppend(moduleA, fileB);
    targets.createOrAppend(moduleB, fileB);
    targets.createOrAppend(srvpgmAModule);
    targets.createOrAppend(ORDENTSRV);
    targets.createOrAppend(ORDENTMOD);
    targets.createOrAppend(UNUSEDSRV);
    targets.createOrAppend(programACommand);
    targets.createOrAppend(unusedCmd);
  }

  return targets;
}

export async function multiModuleObjects() {
  const fs = new ReadFileSystem();
  const targets = new Targets(cwd, fs);

  // Base program object MYWEBAPP.PGM
  const myWebApp = await targets.resolvePathToObject(path.join(cwd, `qrpglesrc`, `mywebapp.pgm.rpgle`));
  myWebApp.imports = [`ROUTEHANDLERA`, `ROUTEHANDLERB`, `JWT_MIDDLEWARE`, `IL_LISTEN`, `IL_RESPONSEWRITESTREAM`];

  // Module that is required by the MYWEBAPP.PGM
  const handlerAMod = await targets.resolvePathToObject(path.join(cwd, `qrpglesrc`, `handlerA.rpgle`));
  handlerAMod.exports = [`ROUTEHANDLERA`];
  handlerAMod.imports = [`JSON_SQLRESULTSET`, `IL_RESPONSEWRITESTREAM`];

  // Another module that is required by the MYWEBAPP.PGM
  const handlerBMod = await targets.resolvePathToObject(path.join(cwd, `qrpglesrc`, `handlerB.rpgle`));
  handlerBMod.exports = [`ROUTEHANDLERB`];
  handlerBMod.imports = [`API_VALIDATE`, `JSON_SQLRESULTSET`, `IL_RESPONSEWRITESTREAM`];

  // Another module that is part of the JWTHANDLER.SRVPGM object
  const jwtHandlerMod = await targets.resolvePathToObject(path.join(cwd, `qrpglesrc`, `jwtHandler.rpgle`));
  jwtHandlerMod.exports = [`JWT_MIDDLEWARE`];

  // Another module that is part of the JWTHANDLER.SRVPGM object
  const validateMod = await targets.resolvePathToObject(path.join(cwd, `qrpglesrc`, `validate.rpgle`));
  validateMod.exports = [`API_VALIDATE`];

  // Service program for JWTHANDLER, used by MYWEBAPP
  const jwtHandlerSrv = await targets.resolvePathToObject(path.join(cwd, `qsrvsrc`, `utils.binder`));
  jwtHandlerSrv.imports = [`JWT_MIDDLEWARE`, `API_VALIDATE`];
  jwtHandlerSrv.exports = [`JWT_MIDDLEWARE`, `API_VALIDATE`];

  targets.createOrAppend(myWebApp);
  targets.createOrAppend(handlerAMod);
  targets.createOrAppend(handlerBMod);
  targets.createOrAppend(jwtHandlerMod);
  targets.createOrAppend(validateMod);
  targets.createOrAppend(jwtHandlerSrv);

  targets.resolveBinder();

  return targets;
}
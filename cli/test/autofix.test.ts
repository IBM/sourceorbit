import { describe, expect, test } from "vitest";
import { setupIncludeFix, setupProjectFromQsys } from "./fixtures/projects";

import { Targets } from '../src/targets'
import { getFiles, renameFiles } from "../src/utils";
import { scanGlob } from "../src/extensions";

import * as path from "path";

test(`Auto rename RPGLE program and include and fix-include infos`, async () => {
  const cwd = setupProjectFromQsys();

  // First step is to rename the files

	let targets = new Targets(cwd);
	targets.setSuggestions({renames: true});

  const initialFiles = getFiles(cwd, scanGlob);
  targets.loadObjectsFromPaths(initialFiles);
	await Promise.allSettled(initialFiles.map(f => targets.parseFile(f)));

	targets.resolveBinder();

  // Ensure we can find by system name
  expect(targets.searchForObject({systemName: `DEPT`, type: `FILE`}, undefined)).toBeDefined();

  const bySystemNameA = targets.searchForAnyObject({name: `DEPT`, types: [`FILE`]});
  expect(bySystemNameA).toBeDefined();
  // And by long name
  const byLongNameA = targets.searchForAnyObject({name: `super_long_dept_name`, types: [`FILE`]});
  expect(byLongNameA).toBeDefined();

  // In theory they should be the same
  expect(bySystemNameA).toStrictEqual(byLongNameA);

  const oldPrograms = targets.getTargetsOfType(`PGM`);
  expect(oldPrograms.length).toBe(0); //Because the initial project extension is wrong

  let allLogs = targets.logger.getAllLogs();
  expect(Object.keys(allLogs).length).toBeGreaterThan(0);

  const errorTable = allLogs[path.join(`qprotosrc`, `errortable.rpgle`)];
  const payroll = allLogs[path.join(`qrpglesrc`, `payroll.rpgle`)];
  const empmst = allLogs[path.join(`qsqlsrc`, `empmst.sql`)];
  const emp = allLogs[path.join(`qsqlsrc`, `emp.sql`)];
  const dept = allLogs[path.join(`qsqlsrc`, `dept.sql`)];

  expect(emp.length).toBe(1);
  expect(emp[0].message).toBe(`SUPER_LONG_EMP_NAME (FILE) name is longer than 10 characters. Consider using 'FOR SYSTEM NAME' in the CREATE statement.`);
  expect(emp[0].type).toBe(`warning`);
  expect(emp[0].range.start).toBeDefined();
  expect(emp[0].range.end).toBeDefined();

  expect(dept.length).toBe(1);
  expect(dept[0]).toStrictEqual({
    message: "Rename suggestion",
    type: "rename",
    change: {
      rename: {
        path: path.join(cwd, `qsqlsrc`, `dept.sql`),
        newName: "super_long_dept_name.table",
      },
    },
  });

  expect(empmst.length).toBe(1);
  expect(empmst[0]).toStrictEqual({
    message: "Rename suggestion",
    type: "rename",
    change: {
      rename: {
        path: path.join(cwd, `qsqlsrc`, `empmst.sql`),
        newName: "empmst.table",
      },
    },
  });

  expect(errorTable.length).toBe(1);
  expect(errorTable[0]).toStrictEqual({
    message: "Rename suggestion",
    type: "rename",
    change: {
      rename: {
        path: path.join(cwd, `qprotosrc`, `errortable.rpgle`),
        newName: "errortable.rpgleinc",
      },
    },
  });

  expect(payroll.length).toBe(2);
  expect(payroll[0]).toStrictEqual({
    line: 35,
    // Note here that we are using .posix. RPGLE includes always use posix.
    message: `Include at line 35 found, to path '${path.posix.join(`qprotosrc`, `errortable.rpgle`)}'`,
    type: "info",
  });
  expect(payroll[1]).toStrictEqual({
    message: "Rename suggestion",
    type: "rename",
    change: {
      rename: {
        path: path.join(cwd, `qrpglesrc`, `payroll.rpgle`),
        newName: "payroll.pgm.rpgle",
      },
    },
  });

  // Trigger the rename
  renameFiles(targets.logger);

  // Next, scan the project again and check the logs
  targets = new Targets(cwd);
	targets.setSuggestions({renames: true});

  const newFiles = getFiles(cwd, scanGlob);
  targets.loadObjectsFromPaths(newFiles);
	await Promise.allSettled(newFiles.map(f => targets.parseFile(f)));

	targets.resolveBinder();

  const bySystemNameB = targets.searchForAnyObject({name: `DEPT`, types: [`FILE`]});
  expect(bySystemNameB).toBeDefined();
  // And by long name
  const byLongNameB = targets.searchForAnyObject({name: `super_long_dept_name`, types: [`FILE`]});
  expect(byLongNameB).toBeDefined();

  // In theory they should be the same
  expect(bySystemNameB).toStrictEqual(byLongNameB);

  const newPrograms = targets.getTargetsOfType(`PGM`);
  expect(newPrograms.length).toBe(1); //Because the extension was fixed

  allLogs = targets.logger.getAllLogs();
  expect(Object.keys(allLogs).length).toBeGreaterThan(0);

  const oldPayroll = allLogs[path.join(`qrpglesrc`, `payroll.rpgle`)];
  expect(oldPayroll).toBeUndefined();

  const newPayroll = allLogs[path.join(`qrpglesrc`, `payroll.pgm.rpgle`)];
  expect(newPayroll.length).toBe(1);
  expect(newPayroll[0]).toStrictEqual({
    line: 35,
    message: `Include at line 35 found, to path '${path.posix.join(`qprotosrc`, `errortable.rpgleinc`)}'`,
    type: "info",
  });

  // Small test to check we can resolve the files
  const ddsTarget = targets.getTarget({systemName: `MSTDSP`, type: `FILE`});
  expect(ddsTarget).toBeDefined();
  expect(ddsTarget?.relativePath).toBe(path.join(`qddssrc`, `mstdsp.dspf`));
  expect(ddsTarget.deps.length).toBe(3);
});


test(`Fix includes in same directory`, async () => {
  const cwd = setupIncludeFix();

  // First step is to rename the files

	let targets = new Targets(cwd);
	targets.setSuggestions({includes: true});

  const initialFiles = getFiles(cwd, scanGlob);
  targets.loadObjectsFromPaths(initialFiles);
	await Promise.allSettled(initialFiles.map(f => targets.parseFile(f)));

	targets.resolveBinder();

  const programs = targets.getTargetsOfType(`PGM`);
  expect(programs.length).toBe(1);

  let allLogs = targets.logger.getAllLogs();
  expect(Object.keys(allLogs).length).toBe(1)

  const payroll = allLogs[path.join(`QRPGLESRC`, `PAYROLL.pgm.rpgle`)];

  expect(payroll.length).toBe(5);

  expect(payroll[0]).toStrictEqual({
    message: "Will update to use unix style path.",
    type: "includeFix",
    line: 35,
    change: {
      lineContent: "/copy 'QRPGLESRC/ERRORTABLE.rpgleinc'",
    },
  });

  expect(payroll[1]).toStrictEqual({
    message: "No object found for reference 'MSTDSP'",
    type: "warning",
    line: 28,
  });

  expect(payroll[2]).toStrictEqual({
    message: "No object found for reference 'EMPMST'",
    type: "warning",
    line: 29,
  });

  expect(payroll[3]).toStrictEqual({
    message: "No object found for reference 'PRJMST'",
    type: "warning",
    line: 30,
  });

  expect(payroll[4]).toStrictEqual({
    message: "No object found for reference 'RSNMST'",
    type: "warning",
    line: 31,
  });
});
import { describe, expect, test } from "vitest";
import { getFiles, scanGlob } from "./fixtures/files";
import { setupIncludeFix, setupProjectFromQsys } from "./fixtures/projects";

import { Targets } from '../src/targets'
import { renameFiles } from "../src/cli";

test(`Auto rename RPGLE program and include and fix-include infos`, async () => {
  const cwd = setupProjectFromQsys();

  // First step is to rename the files

  let targets = new Targets(cwd);
	targets.setSuggestions({renames: true});

  const initialFiles = getFiles(cwd, scanGlob);
  await Promise.allSettled(initialFiles.map(f => targets.handlePath(f)));

  targets.resolveBinder();

  const oldPrograms = targets.getParentObjects(`PGM`);
  expect(oldPrograms.length).toBe(0); //Because the initial project extension is wrong

  let allLogs = targets.logger.getAllLogs();
  expect(Object.keys(allLogs).length).toBeGreaterThan(0);

  const errorTable = allLogs["qprotosrc/errortable.rpgle"];
  const payroll = allLogs["qrpglesrc/payroll.rpgle"];
  const empmst = allLogs["qsqlsrc/empmst.sql"];
  const emp = allLogs["qsqlsrc/emp.sql"];
  const dept = allLogs["qsqlsrc/super_long_dept_name.sql"];

  expect(emp.length).toBe(1);
  expect(emp[0]).toStrictEqual({
    message: "SUPER_LONG_EMP_NAME (FILE) name is longer than 10 characters. Consider using 'FOR SYSTEM NAME' in the CREATE statement.",
    type: "warning",
    range: {
      start: 94,
      end: 113,
    },
  });

  expect(dept.length).toBe(1);
  expect(dept[0]).toStrictEqual({
    message: "Rename suggestion",
    type: "rename",
    change: {
      rename: {
        path: Targets.getPosixyPath(targets.getCwd() + "/qsqlsrc/super_long_dept_name.sql"),
        newName: "dept.table",
      },
    },
  });

  expect(empmst.length).toBe(1);
  expect(empmst[0]).toStrictEqual({
    message: "Rename suggestion",
    type: "rename",
    change: {
      rename: {
        path: Targets.getPosixyPath(targets.getCwd() + "/qsqlsrc/empmst.sql"),
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
        path: Targets.getPosixyPath(targets.getCwd() + "/qprotosrc/errortable.rpgle"),
        newName: "errortable.rpgleinc",
      },
    },
  });

  expect(payroll.length).toBe(2);
  expect(payroll[0]).toStrictEqual({
    line: 35,
    message: "Include at line 35 found, to path 'qprotosrc/errortable.rpgle'",
    type: "info",
  });
  expect(payroll[1]).toStrictEqual({
    message: "Rename suggestion",
    type: "rename",
    change: {
      rename: {
        path: Targets.getPosixyPath(targets.getCwd() + "/qrpglesrc/payroll.rpgle"),
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
  await Promise.allSettled(newFiles.map(f => targets.handlePath(f)));

  targets.resolveBinder();

  const newPrograms = targets.getParentObjects(`PGM`);
  expect(newPrograms.length).toBe(1); //Because the extension was fixed

  allLogs = targets.logger.getAllLogs();
  expect(Object.keys(allLogs).length).toBeGreaterThan(0);

  const oldPayroll = allLogs["qrpglesrc/payroll.rpgle"];
  expect(oldPayroll).toBeUndefined();

  const newPayroll = allLogs["qrpglesrc/payroll.pgm.rpgle"];
  expect(newPayroll.length).toBe(1);
  expect(newPayroll[0]).toStrictEqual({
    line: 35,
    message: "Include at line 35 found, to path 'qprotosrc/errortable.rpgleinc'",
    type: "info",
  });
});


test(`Fix includes in same directory`, async () => {
  const cwd = setupIncludeFix();

  // First step is to rename the files

  let targets = new Targets(cwd);
	targets.setSuggestions({includes: true});

  const initialFiles = getFiles(cwd, scanGlob);
  await Promise.allSettled(initialFiles.map(f => targets.handlePath(f)));

  targets.resolveBinder();

  const programs = targets.getParentObjects(`PGM`);
  expect(programs.length).toBe(1);

  let allLogs = targets.logger.getAllLogs();
  expect(Object.keys(allLogs).length).toBe(1)

  const payroll = allLogs["QRPGLESRC/PAYROLL.pgm.rpgle"];

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
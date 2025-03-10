import { expect, test } from "vitest";
import { setupFixture } from "./fixtures/projects";

import { Targets } from '../src/targets'
import { renameFiles } from "../src/utils";
import { scanGlob } from "../src/extensions";

import * as path from "path";
import { ReadFileSystem } from "../src/readFileSystem";

test(`Auto rename RPGLE program and include and fix-include infos`, async () => {
  const project = setupFixture(`auto_rename1`);
  project.setup();

  const fs = new ReadFileSystem();

  // First step is to rename the files
	let targets = new Targets(project.cwd, fs);
	targets.setSuggestions({renames: true});

  await targets.loadProject();

	targets.resolveBinder();

  let allLogs = targets.logger.getAllLogs();
  expect(Object.keys(allLogs).length).toBeGreaterThan(0);

  const pgmSource = allLogs[path.join(`src`, `BBSADMMNUR.rpgle`)].filter(log => log.type === `rename`);
  const cbkSource = allLogs[path.join(`src`, `CBKOPTIMIZ.rpgle`)];

  expect(pgmSource.length).toBe(1);
  expect(cbkSource.length).toBe(1);

  expect(pgmSource[0].message).toBe(`Rename suggestion`);
  expect(pgmSource[0].type).toBe(`rename`);
  expect(pgmSource[0].change.rename.newName).toBe(`BBSADMMNUR.pgm.rpgle`);

  expect(cbkSource[0].message).toBe(`Rename suggestion`);
  expect(cbkSource[0].type).toBe(`rename`);
  expect(cbkSource[0].change.rename.newName).toBe(`CBKOPTIMIZ.rpgleinc`);

  // Trigger the rename
  renameFiles(targets.logger);

  // Next, scan the project again and check the logs
  targets = new Targets(project.cwd, fs);
  targets.setSuggestions({includes: true});

  const newFiles = await fs.getFiles(project.cwd, scanGlob);
  targets.loadObjectsFromPaths(newFiles);
  await Promise.allSettled(newFiles.map(f => targets.parseFile(f)));

  allLogs = targets.logger.getAllLogs();

  const newPgmSource = allLogs[path.join(`src`, `BBSADMMNUR.pgm.rpgle`)].filter(log => log.type === `includeFix`);

  expect(newPgmSource.length).toBe(1);

  expect(newPgmSource[0].message).toBe(`Will update to use unix style path.`);
  expect(newPgmSource[0].type).toBe(`includeFix`);
  expect(newPgmSource[0].change.lineContent).toBe(`      /copy 'src/CBKOPTIMIZ.rpgleinc'`);
  expect(newPgmSource[0].line).toBe(11);
});

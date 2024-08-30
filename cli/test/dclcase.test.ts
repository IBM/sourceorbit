import { describe, expect, test } from "vitest";
import { setupFixture, setupIncludeFix, setupProjectFromQsys } from "./fixtures/projects";

import { Targets } from '../src/targets'
import { getFiles, renameFiles } from "../src/utils";
import { scanGlob } from "../src/extensions";

import * as path from "path";

test(`Check that dclcase is being imported correctly`, async () => {
  const cwd = setupFixture(`dclcase`);

  // First step is to rename the files

	let targets = new Targets(cwd);
	targets.setSuggestions({renames: true});

  const initialFiles = getFiles(cwd, scanGlob);
  targets.loadObjectsFromPaths(initialFiles);
	await Promise.allSettled(initialFiles.map(f => targets.parseFile(f)));
  
  const pgmObject = targets.getTarget({systemName: `APIVAL01S`, type: `PGM`});

  expect(pgmObject).toBeDefined();
  expect(pgmObject.systemName).toBe(`APIVAL01S`);
  expect(pgmObject.type).toBe(`PGM`);
  expect(pgmObject.imports.length).toBe(1);
  expect(pgmObject.imports[0]).toBe(`APIVAL01S_iws_validate`);


	targets.resolveBinder();
});

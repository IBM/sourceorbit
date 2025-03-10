import { expect, test } from "vitest";
import { setupFixture } from "./fixtures/projects";

import { Targets } from '../src/targets'
import { ReadFileSystem } from "../src/readFileSystem";


test(`Check that dclcase is being imported correctly`, async () => {
  const cwd = setupFixture(`dclcase`);

  // First step is to rename the files

  const fs = new ReadFileSystem();
  const targets = new Targets(cwd, fs);
	targets.setSuggestions({renames: true});

  await targets.loadProject();
  
  const pgmObject = targets.getTarget({systemName: `APIVAL01S`, type: `PGM`});

  expect(pgmObject).toBeDefined();
  expect(pgmObject.systemName).toBe(`APIVAL01S`);
  expect(pgmObject.type).toBe(`PGM`);
  expect(pgmObject.imports.length).toBe(1);
  expect(pgmObject.imports[0]).toBe(`APIVAL01S_iws_validate`);


	targets.resolveBinder();
});

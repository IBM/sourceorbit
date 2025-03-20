import type { TestProject } from "vitest/node";
import { getAllFixtures, setupFixture } from "./fixtures/projects";

export async function setup(project: TestProject) {
  console.log(`Setting up project....`);

  console.table(getAllFixtures());

  const fixtures = getAllFixtures();
  for (const fixture of fixtures) {
    let proj = setupFixture(fixture);
    proj.copy();
  }
}
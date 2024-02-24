import { describe, it, expect } from "vitest";
import { CParser } from "../src/baseSource";
import { getSourcePath, resolveInclude } from "./fixtures/fixtures";

describe("include local tests", () => {
  const parser = new CParser();

  parser.setIncludeResolver(resolveInclude);

  it("should not crash", () => {
    const result = parser.expand(getSourcePath("simpleMain.c"));
    console.log(result);
  });
});
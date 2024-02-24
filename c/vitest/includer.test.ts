import { describe, it, expect } from "vitest";
import { CParser } from "../src/baseSource";
import { getSourcePath, resolveInclude } from "./fixtures/fixtures";

describe("include local tests", () => {
  const parser = new CParser();

  parser.setIncludeResolver(resolveInclude);

  it("should not crash", () => {
    parser.expand(getSourcePath("simpleMain.c"));
  });

  it("ifndef", () => {
    const result = parser.expand(getSourcePath("simpleMain2.c"));
    const preprocess = parser.preprocess(result);
    console.log(preprocess.map((t) => t.value).join(" "));
  });
});
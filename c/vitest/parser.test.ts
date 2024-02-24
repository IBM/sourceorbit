import { describe, it, expect } from "vitest";
import { CParser } from "../src/baseSource";
import { getSourcePath, resolveInclude } from "./fixtures/fixtures";

describe("include local tests", () => {
  const parser = new CParser();

  parser.setIncludeResolver(resolveInclude);

  it("should not crash", () => {
    parser.expand(getSourcePath("simpleMain.c"));
  });

  it("preprocess", () => {
    const result = parser.expand(getSourcePath("simpleMain2.c"));
    const preprocess = parser.preprocess(result);
    // console.log(preprocess.map((t) => t.value).join(" "));
  });

  it("simple main function", () => {
    const result = parser.expand(getSourcePath("simpleMain2.c"));
    const preprocess = parser.preprocess(result);
    const functions = parser.getMethods(preprocess);
    expect(functions.length).toBe(1);
  });

  it("complicated module with imports", () => {
    const result = parser.expand(getSourcePath("generic.c"));
    const preprocess = parser.preprocess(result);
    const functions = parser.getMethods(preprocess);
    console.log(functions);
    expect(functions.length).toBe(12);
  });
});
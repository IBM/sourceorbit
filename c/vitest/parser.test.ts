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
    expect(functions).toMatchObject([
      { name: '__memicmp', type: 'import' },
      { name: '__stricmp', type: 'import' },
      { name: '__strnicmp', type: 'import' },
      { name: '__strdup', type: 'import' },
      { name: 'ensureOpenXlate', type: 'export' },
      { name: 'c2s', type: 'export' },
      { name: 'strTrim', type: 'export' },
      { name: 'hex', type: 'export' },
      { name: 'findchr', type: 'export' },
      { name: 'xlateMem', type: 'export' },
      { name: 'xlatecpy', type: 'export' },
      { name: 'iconvWrite', type: 'import' }
    ]);
  });

  it("complicated module with imports and static and inline prototypes", () => {
    const result = parser.expand(getSourcePath("datainto.c"));
    const preprocess = parser.preprocess(result);
    const functions = parser.getMethods(preprocess);
    console.log(functions);
  });
});
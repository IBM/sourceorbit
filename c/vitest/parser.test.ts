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

  it("complicated module with imports, extern and exports", () => {
    const result = parser.expand(getSourcePath("generic.c"));
    const preprocess = parser.preprocess(result);
    const functions = parser.getMethods(preprocess);
    expect(functions).toMatchObject([
      { name: '__memicmp', type: 'import' },
      { name: '__stricmp', type: 'import' },
      { name: '__strnicmp', type: 'import' },
      { name: '__strdup', type: 'import' },
      { name: 'InputCcsid', type: 'extern' },
      { name: 'OutputCcsid', type: 'extern' },
      { name: 'xlateEto1208', type: 'extern' },
      { name: 'xlate1208toE', type: 'extern' },
      { name: 'ensureOpenXlate', type: 'export' },
      { name: 'c2s', type: 'export' },
      { name: 'strTrim', type: 'export' },
      { name: 'hex', type: 'export' },
      { name: 'findchr', type: 'export' },
      { name: 'xlateMem', type: 'export' },
      { name: 'xlatecpy', type: 'export' },
      { name: 'iconvWrite', type: 'export' },
      { name: 'iconvPutc', type: 'export' },
      { name: 'swapEndian', type: 'export' },
      { name: 'xlate', type: 'export' },
      { name: 'utf8toUnicode', type: 'export' },
      { name: 'isTerm', type: 'export' },
      { name: 'isTimeStamp', type: 'export' },
      { name: 'formatTimeStamp', type: 'export' },
      { name: 'unicode2ebcdic', type: 'export' },
      { name: 'parsehex', type: 'export' }
    ]);
  });

  it("complicated module with imports, static, inline prototypes and export", () => {
    const result = parser.expand(getSourcePath("datainto.c"));
    const preprocess = parser.preprocess(result);
    const functions = parser.getMethods(preprocess);
    expect(functions).toMatchObject([
      { name: '__memicmp', type: 'import' },
      { name: '__stricmp', type: 'import' },
      { name: '__strnicmp', type: 'import' },
      { name: '__strdup', type: 'import' },
      { name: 'jx_dataIntoMapObject', type: 'static' },
      { name: 'jx_dataIntoMapArray', type: 'static' },
      { name: 'jx_dataIntoMapValue', type: 'static' },
      { name: 'jx_dataIntoMapNode', type: 'static' },
      { name: 'jx_dataIntoMapper', type: 'static' },
      { name: 'jx_dataInto', type: 'export' }
    ]);
  });

  // TODO: don't run this test if source doesn't exist. We don't include it in the git repo
  it("cpp classes", () => {
    const result = parser.expand(getSourcePath("AbstractDOMParser.cpp"));
    const preprocess = parser.preprocess(result);
    const symbols = parser.getMethods(preprocess, true);
    expect(symbols).toMatchObject([
      { name: 'XMLPScanToken', type: 'import', isClass: true },
      { name: 'XMLScanner', type: 'import', isClass: true },
      { name: 'XMLValidator', type: 'import', isClass: true },
      { name: 'DOMDocumentImpl', type: 'import', isClass: true },
      { name: 'DOMDocumentTypeImpl', type: 'import', isClass: true },
      { name: 'DOMElement', type: 'import', isClass: true },
      { name: 'GrammarResolver', type: 'import', isClass: true },
      { name: 'XMLGrammarPool', type: 'import', isClass: true },
      { name: 'PSVIHandler', type: 'import', isClass: true },
      { name: 'AbstractDOMParser', type: 'export', isClass: true }
    ]);
  });
});
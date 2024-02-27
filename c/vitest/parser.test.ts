import { describe, it, expect } from "vitest";
import { CParser } from "../src/parser";
import { getSourcePath, resolveInclude } from "./fixtures/fixtures";
import { Token } from "../src/types";

describe("include local tests", () => {
  const parser = new CParser();

  parser.setIncludeResolver(resolveInclude);

  it("should not crash", () => {
    parser.getDocument(getSourcePath("simpleMain.c"));
  });

  it("preprocess doesn't crash", () => {
    const result = parser.getDocument(getSourcePath("simpleMain2.c"));
    result.preprocess();
  });

  it("simple main function", () => {
    const doc = parser.getDocument(getSourcePath("simpleMain2.c"));
    doc.preprocess();
    const symbols = doc.getSymbols();
    expect(symbols.length).toBe(1);

    const macros = doc.getMacros();

    expect(Object.keys(macros)).toMatchObject(
      [ 'OSTYPES_H', 'SICKMATE', 'COOL', 'GOOD' ]
    )
  });

  it("macros test", () => {
    const doc = parser.getDocument(getSourcePath("simpleMacro.c"));
    doc.preprocess();
    const macros = doc.getMacros();

    const maxMacro = macros.MAX as Token[];
    expect(maxMacro).toBeDefined();
    expect(maxMacro[0].type).toBe(`block`);
    expect(maxMacro[0].blockType).toBe(`list`);
    expect(maxMacro[1].type).toBe(`block`);
    expect(maxMacro[1].blockType).toBe(`body`);
    
    expect(macros.COOL).toBe(true);
    expect(macros.AWESOME).toMatchObject(
      [ { value: '1337', type: 'word', range: {start: 119, end: 123} } ]
    );
  });

  it("macro expression test (A)", () => {
    const doc = parser.getDocument(getSourcePath("macroIf.c"));
    doc.preprocess();
    const macros = doc.getMacros();

    expect(Object.keys(macros)).toMatchObject([
      `RLS`, `SOGOOD`, `SICKMATE`, `NICE`, `COOLNESS`, `NOICE`
    ])
  });

  it("complicated module with imports, extern and exports", () => {
    const doc = parser.getDocument(getSourcePath("generic.c"));
    doc.preprocess();
    const symbols = doc.getSymbols();
    expect(symbols).toMatchObject([
      { name: '__memicmp', type: 'import' },
      { name: '__stricmp', type: 'import' },
      { name: '__strnicmp', type: 'import' },
      { name: '__strdup', type: 'import' },
      { name: 'vcTrimRight', type: 'import' },
      { name: 'str2vc', type: 'import' },
      { name: 'vc2strtrim', type: 'import' },
      { name: 'substr2vc', type: 'import' },
      { name: 'vc2strcpy', type: 'import' },
      { name: 'vc2str', type: 'import' },
      { name: 'vccpy', type: 'import' },
      { name: 'vccatstr', type: 'import' },
      { name: 'vccatc', type: 'import' },
      { name: 'vccatvc', type: 'import' },
      { name: 'vccatmem', type: 'import' },
      { name: 'vcprintf', type: 'import' },
      { name: 'vccatf', type: 'import' },
      { name: 'vc2varpuchar', type: 'import' },
      { name: 'str2varpuchar', type: 'import' },
      { name: 'vcListNew', type: 'import' },
      { name: 'vcListAdd', type: 'import' },
      { name: 'vcListFree', type: 'import' },
      { name: 'vcListFirst', type: 'import' },
      { name: 'vcListNext', type: 'import' },
      { name: 'vpc2string', type: 'import' },
      { name: 'vpcSetString', type: 'import' },
      { name: 'vpcIsEqual', type: 'import' },
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

    // ensure no dupes are found
    for (const symbol of symbols) {
      const found = symbols.filter(s => s.name === symbol.name);
      expect(found.length).toBe(1);
    }
  });

  it("complicated module with imports, static, inline prototypes and export", () => {
    const doc = parser.getDocument(getSourcePath("datainto.c"));
    doc.preprocess();
    const symbols = doc.getSymbols();
    expect(symbols).toMatchObject([
      { name: '__memicmp', type: 'import' },
      { name: '__stricmp', type: 'import' },
      { name: '__strnicmp', type: 'import' },
      { name: '__strdup', type: 'import' },
      { name: 'vcTrimRight', type: 'import' },
      { name: 'str2vc', type: 'import' },
      { name: 'vc2strtrim', type: 'import' },
      { name: 'substr2vc', type: 'import' },
      { name: 'vc2strcpy', type: 'import' },
      { name: 'vc2str', type: 'import' },
      { name: 'vccpy', type: 'import' },
      { name: 'vccatstr', type: 'import' },
      { name: 'vccatc', type: 'import' },
      { name: 'vccatvc', type: 'import' },
      { name: 'vccatmem', type: 'import' },
      { name: 'vcprintf', type: 'import' },
      { name: 'vccatf', type: 'import' },
      { name: 'vc2varpuchar', type: 'import' },
      { name: 'str2varpuchar', type: 'import' },
      { name: 'vcListNew', type: 'import' },
      { name: 'vcListAdd', type: 'import' },
      { name: 'vcListFree', type: 'import' },
      { name: 'vcListFirst', type: 'import' },
      { name: 'vcListNext', type: 'import' },
      { name: 'vpc2string', type: 'import' },
      { name: 'vpcSetString', type: 'import' },
      { name: 'vpcIsEqual', type: 'import' },
      { name: 'jx_dataIntoMapObject', type: 'static' },
      { name: 'jx_dataIntoMapArray', type: 'static' },
      { name: 'jx_dataIntoMapValue', type: 'static' },
      { name: 'jx_dataIntoMapNode', type: 'static' },
      { name: 'jx_dataIntoMapper', type: 'static' },
      { name: 'jx_dataInto', type: 'export' }
    ]);

    // ensure no dupes are found
    for (const symbol of symbols) {
      const found = symbols.filter(s => s.name === symbol.name);
      expect(found.length).toBe(1);
    }
  });

  // TODO: don't run this test if source doesn't exist. We don't include it in the git repo
  it("cpp classes", () => {
    const cppTestPath = resolveInclude("AbstractDOMParser.cpp");
    if (!cppTestPath) return;

    const doc = parser.getDocument(cppTestPath);
    doc.preprocess();
    const symbols = doc.getSymbols();
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

  // TODO: don't run this test if source doesn't exist. We don't include it in the git repo
  it("overriding prototypes", () => {
    const doc = parser.getDocument(resolveInclude("varchar.c")!);
    doc.preprocess();
    const symbols = doc.getSymbols();
    
    expect(symbols).toMatchObject([
      { name: '__memicmp', type: 'import' },
      { name: '__stricmp', type: 'import' },
      { name: '__strnicmp', type: 'import' },
      { name: '__strdup', type: 'import' },
      { name: 'str2varpuchar', type: 'export' },
      { name: 'vc2varpuchar', type: 'export' },
      { name: 'vc2str', type: 'export' },
      { name: 'vc2strtrim', type: 'export' },
      { name: 'vc2strcpy', type: 'export' },
      { name: 'vccatstr', type: 'export' },
      { name: 'vccatc', type: 'export' },
      { name: 'vccatvc', type: 'export' },
      { name: 'vccatmem', type: 'export' },
      { name: 'vccpy', type: 'export' },
      { name: 'vcTrimRight', type: 'export' },
      { name: 'str2vc', type: 'export' },
      { name: 'substr2vc', type: 'export' },
      { name: 'vcprintf', type: 'export' },
      { name: 'vccatf', type: 'export' },
      { name: 'vcListNew', type: 'export' },
      { name: 'vcListAdd', type: 'export' },
      { name: 'vcListFree', type: 'export' },
      { name: 'vcListFirst', type: 'export' },
      { name: 'vcListNext', type: 'export' },
      { name: 'vpc2string', type: 'export' },
      { name: 'vpcSetString', type: 'export' },
      { name: 'vpcIsEqual', type: 'export' }
    ]);

    // ensure no dupes are found
    for (const symbol of symbols) {
      const found = symbols.filter(s => s.name === symbol.name);
      expect(found.length).toBe(1);
    }
  });

  it("block test with defines", () => {
    const doc = parser.getDocument(resolveInclude("complexTokens.cpp")!);
    doc.preprocess();
  });
});
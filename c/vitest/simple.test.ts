import { describe, it, expect } from "vitest";
import CTokens from "../src/tokens";
import { readFileSync } from "fs";
import { getSourcePath } from "./fixtures/fixtures";

describe("simple", () => {
  it("should not crash", () => {
    const tokens = new CTokens();
    const result = tokens.tokenise([`int main() {`, `\treturn 0;`, ` }`].join(`\n`));
  });

  it("block test", () => {
    const tokens = new CTokens();
    const result = tokens.tokenise(readFileSync(getSourcePath(`block.c`), {encoding: `utf8`}));
    expect(result.length).toBe(3);
  });

  it("block test 2", () => {
    const tokens = new CTokens();
    const result = tokens.tokenise(readFileSync(getSourcePath(`reader.c`), {encoding: `utf8`}));
  });
});
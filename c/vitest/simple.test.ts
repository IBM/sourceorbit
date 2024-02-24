import { describe, it, expect } from "vitest";
import CTokens from "../src/tokens";

describe("simple", () => {
  it("should not crash", () => {
    const tokens = new CTokens();
    const result = tokens.tokenise([`int main() {`, `\treturn 0;`, ` }`].join(`\n`));
  });
});
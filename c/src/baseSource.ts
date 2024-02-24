import { readFileSync } from "fs";
import CTokens from "./tokens";
import { Token } from "./types";

export type IncludeResolveFunction = (path: string) => string;

export class CParser {
  private parser: CTokens = new CTokens();
  private resolveToPath: IncludeResolveFunction|undefined = undefined;

  constructor() {
  }

  setIncludeResolver(resolver: IncludeResolveFunction) {
    this.resolveToPath = resolver;
  }

  static readContent(fullPath: string) {
    return readFileSync(fullPath, {encoding: `utf8`});
  }

  expand(fullPath: string) {
    const tokens = this.parser.tokenise(CParser.readContent(fullPath));

    for (let i = 0; i < tokens.length; i++) {
      const token = tokens[i];

      if (token.type === `directive`) {
        const endIndex = findNextOrEnd(tokens, `newline`, i + 1);
        if (token.value?.toUpperCase() === `#INCLUDE`) {
          const nextToken = tokens[i + 1];

          if (nextToken && nextToken.type === `string` && nextToken.value) {
            if (this.resolveToPath) {
              const resolvedPath = this.resolveToPath(nextToken.value);

              if (resolvedPath) {
                const documentTokens = this.expand(resolvedPath);
                tokens.splice(i, endIndex, ...documentTokens);
              }
            }
          }
        }
      }
    }

    return tokens;
  }
}

function findNextOrEnd(tokens: Token[], type: string, start: number) {
  for (let i = start; i < tokens.length; i++) {
    if (tokens[i].type === type) {
      return i;
    }
  }

  return tokens.length;
}
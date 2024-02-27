import { readFileSync } from "fs";
import CTokens from "./tokens";
import { findNextOrEnd } from "./utils";
import { IncludeResolveFunction, IncludeResolveResult, Token } from "./types";
import { ModuleSource } from "./moduleSource";

interface ExpandResult {
  tokens: Token[];
  includes: IncludeResolveResult[];
}

export class CParser {
  private parser: CTokens = new CTokens();
  private resolveToPath: IncludeResolveFunction | undefined = undefined;

  constructor() {
  }

  setIncludeResolver(resolver: IncludeResolveFunction) {
    this.resolveToPath = resolver;
  }

  static readContent(fullPath: string) {
    return readFileSync(fullPath, { encoding: `utf8` });
  }

  getDocument(fullPath: string) {
    const stream = this.expand(fullPath);
    const module = new ModuleSource(fullPath, stream.tokens);
    module.setResolvedIncludes(stream.includes);
    return module;
  }

  private expand(fullPath: string): ExpandResult {
    const tokens = this.parser.tokenise(CParser.readContent(fullPath));
    const headers: IncludeResolveResult[] = [];

    for (let i = 0; i < tokens.length; i++) {
      const token = tokens[i];

      if (token.type === `directive`) {
        const endIndex = findNextOrEnd(tokens, `newline`, i + 1);
        if (this.resolveToPath) {
          if (token.value?.toUpperCase() === `#INCLUDE`) {
            const nextToken = tokens[i + 1];

            if (nextToken) {
              // Application headers
              if (nextToken.type === `string` && nextToken.value) {
                const resolvedPath = this.resolveToPath(nextToken.value);

                if (resolvedPath) {
                  headers.push({ fullPath: resolvedPath, state: `resolved` });
                  const newStream = this.expand(resolvedPath);
                  headers.push(...newStream.includes);
                  tokens.splice(i, endIndex - i, ...newStream.tokens);
                } else {
                  headers.push({ fullPath: nextToken.value, state: `resolved` });
                }

              // System headers
              } else if (nextToken.type === `opengeneric` && tokens[endIndex-1].type === `closegeneric`) {
                // The join here is a hack.
                // This is because we don't have access to the original document to substring it out.
                // TODO: improve this
                const includeString = tokens.slice(i + 2, endIndex-1).map(x => x.value).join(``);
                const resolvedPath = this.resolveToPath(includeString);

                if (resolvedPath) {
                  headers.push({ fullPath: resolvedPath, state: `resolved` });
                  const newStream = this.expand(resolvedPath);
                  headers.push(...newStream.includes);
                  tokens.splice(i, endIndex - i, ...newStream.tokens);
                } else {
                  // We don't throw for unfound includes, as this is a common case for system level headers
                }
              }
            }
          }
        }
      }
    }

    return {
      tokens,
      includes: headers,
    };
  }
}

export { IncludeResolveResult };

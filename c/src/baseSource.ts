import { readFileSync } from "fs";
import CTokens from "./tokens";
import { BlockType, Token } from "./types";

export type IncludeResolveFunction = (path: string) => string | undefined;

const ignoredKeywords = [`typedef`];

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

  expand(fullPath: string) {
    const tokens = this.parser.tokenise(CParser.readContent(fullPath));

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
                  const documentTokens = this.expand(resolvedPath);
                  tokens.splice(i, endIndex - i, ...documentTokens);
                } else {
                  //TODO: logger console.log(`Could not resolve include: ${nextToken.value}`);
                }

              // System headers
              } else if (nextToken.type === `opengeneric` && tokens[endIndex-1].type === `closegeneric`) {
                // The join here is a hack.
                // This is because we don't have access to the original document to substring it out.
                // TODO: improve this
                const includeString = tokens.slice(i + 2, endIndex-1).map(x => x.value).join(``);
                const resolvedPath = this.resolveToPath(includeString);

                if (resolvedPath) {
                  const documentTokens = this.expand(resolvedPath);
                  tokens.splice(i, endIndex - i, ...documentTokens);
                } else {
                  // We don't throw for unfound includes, as this is a common case for system level headers
                }
              }
            }
          }
        }
      }
    }

    return tokens;
  }

  preprocess(tokens: Token[]): Token[] {
    let defines: { [key: string]: string | boolean } = {};
    let ifBlocks: { conditionMet: boolean, startBlock: { start: number, end: number }, elseBlock?: { start: number, end: number }, endToken?: number }[] = [];

    for (let i = 0; i < tokens.length; i++) {
      const token = tokens[i];

      if (token.type === `directive`) {
        const endIndex = findNextOrEnd(tokens, `newline`, i + 1);
        const statementTokens = tokens.slice(i, endIndex);
        const nextToken = tokens[i + 1];

        switch (token.value?.toUpperCase()) {
          case `#DEFINE`:
            if (nextToken && nextToken.value) {
              const valueToken = tokens[i + 2];

              defines[nextToken.value] = valueToken?.value || true;
            }
            break;

          case `#UNDEF`:
            if (nextToken && nextToken.value) {
              delete defines[nextToken.value];
            }
            break;

          case `#IF`:
            // throw new Error(`#IF not implemented`);
            break;

          case `#IFDEF`:
            if (nextToken && nextToken.value) {
              ifBlocks.push({ startBlock: { start: i, end: endIndex }, conditionMet: defines[nextToken.value] !== undefined });
            }
            break;

          case `#IFNDEF`:
            if (nextToken && nextToken.value) {
              ifBlocks.push({ startBlock: { start: i, end: endIndex }, conditionMet: defines[nextToken.value] === undefined });
            }
            break;

          case `#ELSE`:
            if (ifBlocks.length) {
              ifBlocks[ifBlocks.length - 1].elseBlock = { start: i, end: endIndex };
            }
            break;

          case `#ENDIF`:
            if (ifBlocks.length) {
              ifBlocks[ifBlocks.length - 1].endToken = i;

              const block = ifBlocks.pop();

              if (block) {

                let itemsRemoved = 0;

                if (block.conditionMet) {
                  if (block.elseBlock) {
                    // If the condition is was met, remove the else block
                    itemsRemoved += tokens.splice(block.elseBlock.start, endIndex - block.elseBlock.start).length;
                  } else {
                    // If the condition was met, remove the endif
                    itemsRemoved += tokens.splice(i, endIndex - i).length;
                  }

                  // Also remove the ifxdef
                  itemsRemoved += tokens.splice(block.startBlock.start, block.startBlock.end - block.startBlock.start).length;
                } else {
                  // Also remove the endif
                  itemsRemoved += tokens.splice(i, endIndex - i).length;

                  if (block.elseBlock) {
                    // If the condition was not met, remove the if block
                    itemsRemoved += tokens.splice(block.startBlock.start, (block.elseBlock.end) - block.startBlock.start).length;
                  }
                }

                i -= itemsRemoved;
              }
            }
            break;
        }
      }
    }

    return tokens;
  }

  getMethods(tokens: Token[], includeCpp = false) {
    let results: { name: string, type: "import" | "export" | "static" | "extern", isClass?: boolean }[] = [];

    for (let i = 0; i < tokens.length; i++) {
      if (i + 3 < tokens.length) {
        const prefixToken = tokens[i - 1];
        const typeToken = tokens[i];
        const nameToken = tokens[i + 1];
        const listBlock = tokens[i + 2];
        const possibleBodyI = findNextNot(tokens, `newline`, i + 3);
        const possibleBody = tokens[possibleBodyI];

        const isStatic = prefixToken && prefixToken.type === `deftype` && prefixToken.value === `static`;
        const isExtern = prefixToken && prefixToken.type === `deftype` && prefixToken.value === `extern`;

        if (typeToken.type === `word`) {
          if (!ignoredKeywords.includes(typeToken.value!) && nameToken.type === `word`) {
            if (listBlock.type === `block` && listBlock.blockType === BlockType.List && possibleBody) {
              if (possibleBody.type === `block` && possibleBody.blockType === BlockType.Body) {
                // Function found?
                if (isStatic) {
                  results.push({ name: nameToken.value!, type: `static` });
                } else {
                  results.push({ name: nameToken.value!, type: `export` });
                }
              } else {
                if (isStatic) continue; // We don't care about static imports. This means the function is not exported generally.

                results.push({ name: nameToken.value!, type: `import` });
              }
            } else if (isExtern) {
              const endStatement = findNextOrEnd(tokens, `semicolon`, i);

              // Usually a comma seperated list.
              for (let j = i + 1; j < endStatement; j++) {
                if (tokens[j].type === `word`) {
                  results.push({ name: tokens[j].value!, type: `extern` });
                }
              }
            }
          } else if (includeCpp) {
            const isClass = prefixToken && prefixToken.type === `deftype` && prefixToken.value === `class`;
            // C++ class?

            if (isClass) {
              const endIndex = findNextMatch(tokens, [`block`, `semicolon`], i);
              const className = tokens[endIndex-1] && tokens[endIndex-1].type === `word` ? tokens[endIndex-1].value : undefined;

              if (className && !results.some(x => x.name === nameToken.value && x.isClass)) {
                results.push({ name: className, type: `import`, isClass: true });
              }
            }
          }
        } else
          if (includeCpp && typeToken.type === `linker`) {
            // Check if it is the constructor
            const className = prefixToken?.value;
            const memberName = nameToken.value;

            // If it is the ctor, add add it to the results
            if (className && memberName && className === memberName) {
              const alreadyExistsIndex = results.findIndex(x => x.name === className && x.isClass && x.type === `import`);

              if (alreadyExistsIndex > -1) {
                results.splice(alreadyExistsIndex, 1);
              }

              results.push({ name: className, type: `export`, isClass: true });
            }
          }
      }
    }

    return results;
  }
}

function findNextNot(tokens: Token[], nottype: string, start: number) {
  for (let i = start; i < tokens.length; i++) {
    if (tokens[i].type !== nottype) {
      return i;
    }
  }

  return tokens.length;
}

function findNextMatch(tokens: Token[], types: string[], start: number) {
  for (let i = start; i < tokens.length; i++) {
    if (types.includes(tokens[i].type)) {
      return i;
    }
  }

  return tokens.length;
}

function findNextOrEnd(tokens: Token[], type: string, start: number) {
  for (let i = start; i < tokens.length; i++) {
    if (tokens[i].type === type) {
      return i;
    }
  }

  return tokens.length;
}
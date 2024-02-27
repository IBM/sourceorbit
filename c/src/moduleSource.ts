import { IncludeResolveResult } from "./types";
import { Token, BlockType } from "./types";
import { findNextMatch, findNextNot, findNextOrEnd } from "./utils";

const ignoredKeywords = [`typedef`];

export interface CompiledSymbol {
  name: string, 
  type: "import" | "export" | "static" | "extern", 
  isClass?: boolean
}

export class ModuleSource {
  private macros: { [key: string]: string | boolean } = {};
  private resolvedIncludes: IncludeResolveResult[] = [];
  private isCpp = false;

  constructor(private fullPath: string, private tokens: Token[]) {
    this.isCpp = fullPath.endsWith(`.cpp`);
  }

  setResolvedIncludes(includes: IncludeResolveResult[]) {
    this.resolvedIncludes = includes;
  }

  getResolvedIncludes() {
    return this.resolvedIncludes;
  }

  preprocess() {
    let ifBlocks: { conditionMet: boolean, startBlock: { start: number, end: number }, elseBlock?: { start: number, end: number }, endToken?: number }[] = [];

    for (let i = 0; i < this.tokens.length; i++) {
      const token = this.tokens[i];

      if (token.type === `directive`) {
        const endIndex = findNextOrEnd(this.tokens, `newline`, i + 1);
        const statementTokens = this.tokens.slice(i, endIndex);
        const nextToken = this.tokens[i + 1];

        switch (token.value?.toUpperCase()) {
          case `#DEFINE`:
            if (nextToken && nextToken.value) {
              const valueToken = this.tokens[i + 2];

              this.macros[nextToken.value] = valueToken?.value || true;
            }
            break;

          case `#UNDEF`:
            if (nextToken && nextToken.value) {
              delete this.macros[nextToken.value];
            }
            break;

          case `#IF`:
            // throw new Error(`#IF not implemented`);
            break;

          case `#IFDEF`:
            if (nextToken && nextToken.value) {
              ifBlocks.push({ startBlock: { start: i, end: endIndex }, conditionMet: this.macros[nextToken.value] !== undefined });
            }
            break;

          case `#IFNDEF`:
            if (nextToken && nextToken.value) {
              ifBlocks.push({ startBlock: { start: i, end: endIndex }, conditionMet: this.macros[nextToken.value] === undefined });
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
                    itemsRemoved += this.tokens.splice(block.elseBlock.start, endIndex - block.elseBlock.start).length;
                  } else {
                    // If the condition was met, remove the endif
                    itemsRemoved += this.tokens.splice(i, endIndex - i).length;
                  }

                  // Also remove the ifxdef
                  itemsRemoved += this.tokens.splice(block.startBlock.start, block.startBlock.end - block.startBlock.start).length;
                } else {
                  // Also remove the endif
                  itemsRemoved += this.tokens.splice(i, endIndex - i).length;

                  if (block.elseBlock) {
                    // If the condition was not met, remove the if block
                    itemsRemoved += this.tokens.splice(block.startBlock.start, (block.elseBlock.end) - block.startBlock.start).length;
                  }
                }

                i -= itemsRemoved;
              }
            }
            break;
        }
      }
    }
  }

  getSymbols(): CompiledSymbol[] {
    let results: CompiledSymbol[] = [];

    for (let i = 0; i < this.tokens.length; i++) {
      if (i + 3 < this.tokens.length) {
        const prefixToken = this.tokens[i - 1];
        const typeToken = this.tokens[i];
        const nameToken = this.tokens[i + 1];
        const listBlock = this.tokens[i + 2];
        const possibleBodyI = findNextNot(this.tokens, `newline`, i + 3);
        const possibleBody = this.tokens[possibleBodyI];

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
              const endStatement = findNextOrEnd(this.tokens, `semicolon`, i);

              // Usually a comma seperated list.
              for (let j = i + 1; j < endStatement; j++) {
                if (this.tokens[j].type === `word`) {
                  results.push({ name: this.tokens[j].value!, type: `extern` });
                }
              }
            }
          } else if (this.isCpp) {
            const isClass = prefixToken && prefixToken.type === `deftype` && prefixToken.value === `class`;
            // C++ class?

            if (isClass) {
              const endIndex = findNextMatch(this.tokens, [`block`, `semicolon`], i);
              const className = this.tokens[endIndex-1] && this.tokens[endIndex-1].type === `word` ? this.tokens[endIndex-1].value : undefined;

              if (className && !results.some(x => x.name === nameToken.value && x.isClass)) {
                results.push({ name: className, type: `import`, isClass: true });
              }
            }
          }
        } else
          if (this.isCpp && typeToken.type === `linker`) {
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
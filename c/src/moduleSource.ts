import { i } from "vitest/dist/index-6e18a03a";
import { IncludeResolveResult } from "./types";
import { Token, BlockType } from "./types";
import { findNextMatch, findNextNot, findNextOrEnd } from "./utils";
import CTokens from "./tokens";

const ignoredKeywords = [`typedef`];

export interface CompiledSymbol {
  name: string, 
  type: "import" | "export" | "static" | "extern", 
  isClass?: boolean
}

export class ModuleSource {
  private macros: { [key: string]: Token[]|boolean } = {};
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

  getMacros() {
    return this.macros;
  }

  handleIf(tokens: Token[]): boolean {
    let conditionMet = false;

    interface ValueResult {asStr: string, skipBlock: boolean};

    const getValue = (i: number): ValueResult => {
      const token = tokens[i];
      const blockToken = tokens[i+1];
      let skipBlock = false;
      let tempValue: any;

      if (token.value === `DEFINED` && blockToken && blockToken.blockType === `list`) {
        const blockValue = blockToken.block && blockToken.block.length > 0 ? blockToken.block[0].value : undefined;

        if (blockValue) {
          tempValue = this.macros[blockValue];
        }
        
        skipBlock = true;
      } else {
        tempValue = this.macros[token.value!];
      }

      if (Array.isArray(tempValue)) {
        return {asStr: tempValue[0]?.value, skipBlock};
      } else if (tempValue) {
        return {asStr: tempValue, skipBlock};
      } else {
        return {asStr: token.value!, skipBlock};
      }
    }

    let negate = false;
    let currentValue: ValueResult|undefined = undefined;
    let lastValue: string|undefined = undefined;
    let nextValue: string|undefined = undefined;

    for (let i = 0; i < tokens.length; i++) {
      const token = tokens[i];

      switch (token.type) {
        case `exclamationmark`:
          negate = true;
          continue;

        case `block`:
          conditionMet = this.handleIf(token.block!);

        case `word`:
          currentValue = getValue(i);
          lastValue = currentValue.asStr;
          
          conditionMet = lastValue !== undefined;
          if (currentValue.skipBlock) {i++};
          break;

        case `morethan`:
          currentValue = getValue(i + 1);
          nextValue = currentValue.asStr;

          if (currentValue.skipBlock) {i++};
          i++;

          if (lastValue && nextValue) {
            conditionMet = Number(lastValue) > Number(nextValue);
          } else {
            conditionMet = false;
          }
          break;

        case `lessthan`:
          currentValue = getValue(i + 1);
          nextValue = currentValue.asStr;
          
          if (currentValue.skipBlock) {i++};
          i++;

          if (lastValue && nextValue) {
            conditionMet = Number(lastValue) < Number(nextValue);
          } else {
            conditionMet = false;
          }
          break;

        case `mte`:
          currentValue = getValue(i + 1);
          nextValue = currentValue.asStr;
          
          if (currentValue.skipBlock) {i++};
          i++;

          if (lastValue && nextValue) {
            conditionMet = Number(lastValue) >= Number(nextValue);
          } else {
            conditionMet = false;
          }
          break;

        case `lte`:
          currentValue = getValue(i + 1);
          nextValue = currentValue.asStr;
          
          if (currentValue.skipBlock) {i++};
          i++;

          if (lastValue && nextValue) {
            conditionMet = Number(lastValue) <= Number(nextValue);
          } else {
            conditionMet = false;
          }
          break;

        case `and`:
          if (!conditionMet) {
            return false;
          }
          break;

        case `or`:
          if (conditionMet) {
            return true;
          }

          break;
      }

      if (negate) {
        conditionMet = !conditionMet;
        negate = false;
      }
    }

    return conditionMet;
  
  }

  preprocess() {
    let ifBlocks: { conditionMet: boolean, startBlock: { start: number, end: number }, elseBlock?: { start: number, end: number }, endToken?: number }[] = [];

    const currentBlockIsTrue = (i: number, lastIf?: number): boolean => {
      const currentIfI = lastIf !== undefined ? lastIf : ifBlocks.length - 1;
      const currentIf = ifBlocks[currentIfI];
      let currentCond = true;
      if (currentIf) {
        let inElseBlock = currentIf.elseBlock !== undefined;

        if (!inElseBlock) {
          currentCond = currentIf.conditionMet;
        } else {
          currentCond = !currentIf.conditionMet;
        }

        if (currentCond) currentCond = currentBlockIsTrue(i, currentIfI-1);
      } else {
        return true;
      }

      return currentCond;
    }

    const handleifClose = (i: number, endIndex: number) => {
      const block = ifBlocks.pop();

      let itemsRemoved = 0;

      if (block) {

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
      }

      return itemsRemoved;
    }

    for (let i = 0; i < this.tokens.length; i++) {
      const token = this.tokens[i];

      if (token.type === `directive`) {
        const endIndex = findNextOrEnd(this.tokens, `newline`, i + 1);
        const nextToken = this.tokens[i + 1];

        switch (token.value?.toUpperCase()) {
          case `#DEFINE`:
            if (currentBlockIsTrue(i)) {
              if (nextToken && nextToken.value) {
                const block = CTokens.createBlocks(this.tokens.slice(i+2, endIndex));
                this.macros[nextToken.value] = block.length > 0 ? block : true;
              }
            }
            break;

          case `#UNDEF`:
            if (nextToken && nextToken.value) {
              delete this.macros[nextToken.value];
            }
            break;

          case `#IF`:
            // throw new Error(`#IF not implemented`);
            const block = CTokens.createBlocks(this.tokens.slice(i+1, endIndex));
            ifBlocks.push({ startBlock: { start: i, end: endIndex }, conditionMet: this.handleIf(block)});
            break;

          case `#ELIF`:
            if (ifBlocks.length) {
              ifBlocks[ifBlocks.length - 1].endToken = i;
              const lastConditionMet = ifBlocks[ifBlocks.length - 1].conditionMet;
              i -= handleifClose(i, i);

              const block = CTokens.createBlocks(this.tokens.slice(i+1, endIndex));
              ifBlocks.push({ startBlock: { start: i+1, end: endIndex }, conditionMet: lastConditionMet === false && this.handleIf(block)});
            }
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

              i -= handleifClose(i, endIndex);
            }
            break;
        }
      }
    }

    CTokens.createBlocks(this.tokens);
  }

  getSymbols(): CompiledSymbol[] {
    let results: CompiledSymbol[] = [];

    function removeImport(name: string) {
      const index = results.findIndex(x => x.name === name && x.type === `import`);

      if (index > -1) {
        results.splice(index, 1);
      }
    }

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
                  removeImport(nameToken.value!);

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
              removeImport(className);

              results.push({ name: className, type: `export`, isClass: true });
            }
          }
      }
    }

    return results;
  }
}
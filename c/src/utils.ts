import { Token } from "./types";

export function findNextNot(tokens: Token[], nottype: string, start: number) {
  for (let i = start; i < tokens.length; i++) {
    if (tokens[i].type !== nottype) {
      return i;
    }
  }

  return tokens.length;
}

export function findNextMatch(tokens: Token[], types: string[], start: number) {
  for (let i = start; i < tokens.length; i++) {
    if (types.includes(tokens[i].type)) {
      return i;
    }
  }

  return tokens.length;
}

export function findNextOrEnd(tokens: Token[], type: string, start: number) {
  for (let i = start; i < tokens.length; i++) {
    if (tokens[i].type === type) {
      return i;
    }
  }

  return tokens.length;
}

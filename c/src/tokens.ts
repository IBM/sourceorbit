import { Token } from "./types";


interface Matcher {
  name: string;
  match: {
    type: string;
    match?: Function;
  }[];
  becomes: string;
};

enum ReadState {
  NORMAL,
  IN_CHARACTER,
  IN_STRING,
  IN_SIMPLE_COMMENT,
  IN_BLOCK_COMMENT
}

export default class CTokens {
  readonly matchers: Matcher[] = [
    {
      name: `NEWLINE`,
      match: [{ type: `newliner` }, { type: `newline` }],
      becomes: `newline`,
    },
    {
      name: `DIRECTIVE`,
      match: [{ type: `hash` }, { type: `word` }],
      becomes: `directive`,
    }
  ];
  readonly spaces = [`\t`, ` `];
  readonly splitParts: string[] = [`(`, `)`, `{`, `}`, `[`, `]`, `/`, `.`, `*`, `-`, `+`, `;`, `"`, `&`, `#`, `%`, `,`, `|`, `?`, `:`, `\n`, `\r`, ...this.spaces];
  readonly types: { [part: string]: string } = {
    '(': `openbracket`,
    '{': `openbracket`,
    '[': `openbracket`,
    ')': `closebracket`,
    '}': `closebracket`,
    ']': `closebracket`,
    '/': `forwardslash`,
    '.': `dot`,
    '*': `asterisk`,
    '-': `minus`,
    '+': `plus`,
    ';': `semicolon`,
    '&': `ampersand`,
    '#': `hash`,
    '"': `doublequote`,
    '%': `percent`,
    ',': `comma`,
    '|': `pipe`,
    '?': `questionmark`,
    ':': `colon`,
    '\n': `newline`,
    '\r': `newliner`,
  };
  readonly charCharacter: string = `'`;
  readonly stringCharacter = `"`;

  readonly startCommentString: string = `//`;
  readonly endCommentString = `\n`;

  readonly startCommentBlock = `/*`;
  readonly endCommentBlock = `*/`;

  constructor() { }

  tokenise(content: string) {
    let commentStart = -1;

    let state: ReadState = ReadState.NORMAL;

    let result: Token[] = [];

    let startsAt = 0;
    let currentText = ``;

    for (let i = 0; i < content.length; i++) {
      // Handle when the comment character is found
      if (state === ReadState.NORMAL && content[i] && content[i + 1] && content.substring(i, i + 2) === this.startCommentString) {
        commentStart = i;
        state = ReadState.IN_SIMPLE_COMMENT;

        // Handle when the end of line is there and we're in a comment
      } else if (state === ReadState.IN_SIMPLE_COMMENT && content[i] === this.endCommentString) {
        const preNewLine = i - 1;
        content = content.substring(0, commentStart) + ` `.repeat(preNewLine - commentStart) + content.substring(preNewLine);
        i--; // So we process the newline next
        state = ReadState.NORMAL;

      // Handle block comment
      } else if (state === ReadState.NORMAL && content[i] && content[i + 1] && content.substring(i, i + 2) === this.startCommentBlock) {
        commentStart = i;
        state = ReadState.IN_BLOCK_COMMENT;

        // Handle when the end of line is there and we're in a comment
      } else if (state === ReadState.IN_BLOCK_COMMENT && content[i] && content[i + 1] && content.substring(i, i + 2) === this.endCommentBlock) {
        const endOfBlock = i + 1;
        content = content.substring(0, commentStart) + ` `.repeat(endOfBlock - commentStart) + content.substring(endOfBlock);
        i++;
        state = ReadState.NORMAL;

      // Handle block comment
      } else if (state === ReadState.IN_SIMPLE_COMMENT || state === ReadState.IN_BLOCK_COMMENT) {
        continue;

        // Handle when we're in a string
      } else if (state === ReadState.IN_CHARACTER && content[i] !== this.charCharacter) {
        currentText += content[i];

        // Handle when we're in a name
      } else if (state === ReadState.IN_STRING && content[i] !== this.stringCharacter) {
        currentText += content[i];

      } else {
        switch (content[i]) {
          // When it's the string character..
          case this.charCharacter:
            if (state === ReadState.IN_CHARACTER) {
              currentText += content[i];
              result.push({ value: currentText, type: `string`, range: { start: startsAt, end: startsAt + currentText.length } });
              currentText = ``;
            } else {
              startsAt = i;
              currentText += content[i];
            }

            // @ts-ignore
            state = (state === ReadState.IN_CHARACTER ? ReadState.NORMAL : ReadState.IN_CHARACTER);
            break;

          case this.stringCharacter:
            if (state === ReadState.IN_STRING) {
              result.push({ value: currentText, type: `string`, range: { start: startsAt+1, end: startsAt + currentText.length } });
              currentText = ``;
            } else {
              startsAt = i;
            }

            // @ts-ignore
            state = (state === ReadState.IN_STRING ? ReadState.NORMAL : ReadState.IN_STRING);
            break;

          // When it's any other character...
          default:
            if (this.splitParts.includes(content[i]) && state === ReadState.NORMAL) {
              if (currentText.trim() !== ``) {
                result.push({ value: currentText, type: `word`, range: { start: startsAt, end: startsAt + currentText.length } });
                currentText = ``;
              }

              if (!this.spaces.includes(content[i])) {
                result.push({ value: content[i], type: this.types[content[i]], range: { start: i, end: i + content[i].length } });
              }

              startsAt = i + 1;

            } else {
              currentText += content[i];
            }
            break;
        }
      }
    }

    if (currentText.trim() !== ``) {
      result.push({ value: currentText, type: `word`, range: { start: startsAt, end: startsAt + currentText.length } });
      currentText = ``;
    }

    result = this.fixStatement(result);
    result = CTokens.createBlocks(result);

    return result;
  }

  private fixStatement(tokens: Token[]) {
    for (let i = 0; i < tokens.length; i++) {
      for (let y = 0; y < this.matchers.length; y++) {
        const type = this.matchers[y];
        let goodMatch = true;

        for (let x = 0; x < type.match.length; x++) {
          const match = type.match[x];

          if (tokens[i + x]) {
            if (tokens[i + x].type === match.type) {
              if (match.match) {
                if (match.match(tokens[i + x].value)) {
                  goodMatch = true;
                } else {
                  goodMatch = false;
                  break;
                }
              } else {
                goodMatch = true;
              }
            } else {
              goodMatch = false;
              break;
            }
          } else {
            goodMatch = false;
          }
        }

        if (goodMatch) {
          const matchedTokens = tokens.slice(i, i + type.match.length);
          const value = matchedTokens.map(x => x.value).join(``);
          tokens.splice(i, type.match.length, {
            type: type.becomes,
            value,
            range: {
              start: matchedTokens[0].range.start,
              end: matchedTokens[matchedTokens.length - 1].range.end
            }
          });

          break;
        }
      }
    }

    return tokens;
  }

  static createBlocks(tokens: Token[]) {
    let start = 0;
    let level = 0;

    for (let i = 0; i < tokens.length; i++) {
      switch (tokens[i].type) {
        case `openbracket`:
          if (level === 0) {
            start = i;
          }
          level++;
          break;
        case `closebracket`:
          level--;

          if (level === 0) {
            tokens.splice(start, i - start + 1, {
              type: `block`,
              block: this.createBlocks(tokens.slice(start + 1, i)),
              range: {
                start: tokens[start].range.start,
                end: tokens[i].range.end
              }
            });
            i = start;
          }
          break;
      }
    }

    return tokens;
  }

  static findScalars(tokens: Token[]) {
    for (let i = 0; i < tokens.length; i++) {
      switch (tokens[i].type) {
        case `word`:
          if (tokens[i + 1] && tokens[i + 1].type === `openbracket`) {
            tokens[i].type = `function`
          }
          break;
      }
    }

    return tokens;
  }
}
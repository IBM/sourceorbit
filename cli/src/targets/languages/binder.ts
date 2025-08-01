import path from "path";
import { CLParser, DefinitionType, Module, File } from "vscode-clle/language";
import { FileOptions, ILEObject, ILEObjectTarget, Targets } from "..";
import { infoOut } from "../../cli";
import { trimQuotes } from "../../utils";
import { ExtensionMap } from "../languages";

export const binderExtensions = [`binder`, `bnd`];
export const binderObjects: ExtensionMap = {
  binder: `SRVPGM`,
  bnd: `SRVPGM`,
}

export async function binderTargetCallback(targets: Targets, localPath: string, content: string, ileObject: ILEObject) {
  const clDocs = new CLParser();
  const tokens = clDocs.parseDocument(content);

  const module = new Module();
  module.parseStatements(tokens);

  const target: ILEObjectTarget = {
    ...ileObject,
    deps: [],
    functions: []
  };

  if (ileObject.extension === `binder`) {
    const pathDetail = path.parse(localPath);

    if (targets.suggestions.renames) {
      targets.logger.fileLog(ileObject.relativePath, {
        message: `Rename suggestion`,
        type: `rename`,
        change: {
          rename: {
            path: localPath,
            newName: pathDetail.name + `.bnd`
          }
        }
      });
    } else {
      targets.logger.fileLog(ileObject.relativePath, {
        message: `Extension is '${ileObject.extension}'. Consolidate by using 'bnd'?`,
        type: `warning`,
      });
    }
  }

  const validStatements = module.statements.filter(s => {
    const possibleObject = s.getObject();
    return (possibleObject && possibleObject.name && [`STRPGMEXP`, `ENDPGMEXP`, `EXPORT`].includes(possibleObject.name.toUpperCase()));
  });

  for (const statement of validStatements) {
    const currentCommand = statement.getObject().name.toUpperCase();
    if (currentCommand === `EXPORT`) {
      const parms = statement.getParms();
      const symbolTokens = parms[`SYMBOL`];

      if (symbolTokens.block && symbolTokens.block.length === 1 && symbolTokens.block[0].type === `string` && symbolTokens.block[0].value) {
        // target.exports.push(trimQuotes(symbolTokens.block[0].value));
        target.functions.push({
          name: trimQuotes(symbolTokens.block[0].value),
          export: true,
          lineRange: [0, 0] //TODO: how to get line range?
        });
      } else
        if (symbolTokens.block && symbolTokens.block.length === 1 && symbolTokens.block[0].type === `word` && symbolTokens.block[0].value) {
          // target.exports.push(trimQuotes(symbolTokens.block[0].value, `"`));
          target.functions.push({
            name: trimQuotes(symbolTokens.block[0].value, `"`),
            export: true,
            lineRange: [0, 0] // TODO: how to get line range?
          });
        } else {
          targets.logger.fileLog(ileObject.relativePath, {
            message: `Invalid EXPORT found. Single quote string expected.`,
            type: `warning`,
            range: {
              start: symbolTokens.range.start,
              end: symbolTokens.range.end
            }
          })
        }

    } else
      if (currentCommand === `ENDPGMEXP`) {
        // Return, we only really care about the first export block
        break;
      }
  }

  // Exports are always uppercase
  target.functions = target.functions.map(e => ({
    ...e,
    name: e.name.toUpperCase()
  }));

  targets.addNewTarget(target);
}
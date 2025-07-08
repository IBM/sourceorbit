import path from "path";
import { CLParser, DefinitionType, Module, File } from "vscode-clle/language";
import { FileOptions, ILEObjectTarget, Targets } from "..";
import { infoOut } from "../../cli";

export const clExtensions = [`clle`, `cl`, `clp`];

export async function clleTargetCallback(targets: Targets, filePath: string, content: string, options: FileOptions) {
  const clDocs = new CLParser();
  const tokens = clDocs.parseDocument(content);

  const module = new Module();
  module.parseStatements(tokens);

  const ileObject = await targets.resolvePathToObject(filePath);

  const pathDetail = path.parse(filePath);
  const target: ILEObjectTarget = {
    ...ileObject,
    deps: []
  };

  infoOut(`${ileObject.systemName}.${ileObject.type}: ${ileObject.relativePath}`);

  if (ileObject.extension?.toLowerCase() === `clp`) {
    if (targets.suggestions.renames) {
      targets.logger.fileLog(ileObject.relativePath, {
        message: `Rename suggestion`,
        type: `rename`,
        change: {
          rename: {
            path: filePath,
            newName: pathDetail.name + `.pgm.clle`
          }
        }
      });
    } else {
      targets.logger.fileLog(ileObject.relativePath, {
        message: `Extension is '${ileObject.extension}', but Source Orbit doesn't support CLP. Is it possible the extension should use '.pgm.clle'?`,
        type: `warning`,
      });
    }

  } else {
    if (ileObject.type === `MODULE`) {
      if (targets.suggestions.renames) {
        targets.logger.fileLog(ileObject.relativePath, {
          message: `Rename suggestion`,
          type: `rename`,
          change: {
            rename: {
              path: filePath,
              newName: pathDetail.name + `.pgm` + pathDetail.ext
            }
          }
        });
      } else {
        targets.logger.fileLog(ileObject.relativePath, {
          message: `Type detected as ${ileObject.type} but Source Orbit doesn't support CL modules. Is it possible the extension should include '.pgm'?`,
          type: `warning`,
        });
      }
    }
  }

  const files = module.getDefinitionsOfType<File>(DefinitionType.File);

  // Loop through local file defs to find a possible dep
  files.forEach(def => {
    const possibleObject = def.file;
    if (possibleObject) {
      if (possibleObject.library?.toUpperCase() === `*LIBL`) {
        possibleObject.library = undefined; // targets means lookup as normal
      }

      if (possibleObject.library) {
        targets.logger.fileLog(ileObject.relativePath, {
          message: `Definition to ${possibleObject.library}/${possibleObject.name} ignored due to qualified path.`,
          range: {
            start: def.range.start,
            end: def.range.end
          },
          type: `info`,
        });

      } else {
        if (Targets.ignoredObjects.includes(possibleObject.name.toUpperCase())) return;

        const resolvedPath = targets.searchForObject({ systemName: possibleObject.name.toUpperCase(), type: `FILE` });
        if (resolvedPath) target.deps.push(resolvedPath);
        else {
          targets.logger.fileLog(ileObject.relativePath, {
            message: `no object found for reference '${possibleObject.name}'`,
            range: {
              start: def.range.start,
              end: def.range.end
            },
            type: `warning`,
          });
        }
      }
    }
  });

  module.statements.filter(s => {
    const possibleObject = s.getObject();
    return (possibleObject && possibleObject.name && possibleObject.name === `CALL`);
  }).forEach(s => {

    const parms = s.getParms();
    const pgmParm = parms[`PGM`];

    if (pgmParm && pgmParm.block) {
      const block = pgmParm.block;
      if (block.length === 1) {
        const name = block[0].value!;

        if (Targets.ignoredObjects.includes(name.toUpperCase())) return;

        const resolvedPath = targets.searchForObject({ systemName: name.toUpperCase(), type: `PGM` });
        if (resolvedPath) target.deps.push(resolvedPath);
        else {
          targets.logger.fileLog(ileObject.relativePath, {
            message: `no object found for reference '${name}'`,
            range: {
              start: pgmParm.range.start,
              end: pgmParm.range.end
            },
            type: `warning`,
          });
        }
      } else {
        targets.logger.fileLog(ileObject.relativePath, {
          message: `PGM call not included as possible reference to library.`,
          range: {
            start: pgmParm.range.start,
            end: pgmParm.range.end
          },
          type: `info`,
        });
      }
    }
  });

  // We also look to see if there is a `.cmd` object with the same name
  const possibleCommandObject = targets.searchForObject({ systemName: ileObject.systemName, type: `CMD` });
  if (possibleCommandObject) targets.createOrAppend(possibleCommandObject, target);

  if (target.deps.length > 0)
    infoOut(`Depends on: ${target.deps.map(d => `${d.systemName}.${d.type}`).join(` `)}`);

  targets.addNewTarget(target);
}
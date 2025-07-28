import path from "path";
import { FileOptions, ILEObject, ILEObjectTarget, Targets } from "..";
import { infoOut } from "../../cli";
import Parser from "vscode-rpgle/language/parser";
import { IncludeStatement } from "vscode-rpgle/language/parserTypes";
import { asPosix, toLocalPath, trimQuotes } from "../../utils";
import { isSqlFunction } from "../../languages/sql";
import { ExtensionMap } from "../languages";

export const rpgleExtensions = [`sqlrpgle`, `rpgle`];
export const rpgleObjects: ExtensionMap = {
  sqlrpgle: `MODULE`,
  rpgle: `MODULE`,
}

interface RpgLookup {
  lookup: string,
  line?: number
}

const includeFileCache: { [path: string]: string } = {};

export async function rpgleTargetCallback(targets: Targets, localPath: string, content: string, ileObject: ILEObject) {
  const parser = setupParser(targets);

  const cache = await parser.getDocs(
    localPath,
    content,
    {
      ignoreCache: true,
      withIncludes: true
    }
  );

  if (cache) {
    const isFree = (content.length >= 6 ? content.substring(0, 6).toLowerCase() === `**free` : false);
    const pathDetail = path.parse(localPath);

    // define internal imports
    ileObject.imports = cache.procedures
      .filter((proc: any) => proc.keyword[`EXTPROC`] && !proc.keyword[`EXPORT`])
      .map(ref => {
        const keyword = ref.keyword;
        let importName: string = ref.name;
        const extproc: string | boolean = keyword[`EXTPROC`];
        if (extproc) {
          if (extproc === true) importName = ref.name;
          else importName = extproc;
        }

        if (importName.includes(`:`)) {
          const parmParms = importName.split(`:`);
          importName = parmParms.filter(p => !p.startsWith(`*`)).join(``);
        }

        if (importName.startsWith(`*`)) {
          importName = ref.name;
        } else {
          importName = trimQuotes(importName);
        }

        return importName;
      });

    // define exported functions
    if (cache.keyword[`NOMAIN`]) {
      ileObject.type = `MODULE`;

      // Note that we store exports as uppercase.
      ileObject.exports = cache.procedures
        .filter((proc: any) => proc.keyword[`EXPORT`])
        .map(ref => ref.name.toUpperCase());
    }

    infoOut(`${ileObject.systemName}.${ileObject.type}: ${ileObject.relativePath}`);

    if (cache.includes && cache.includes.length > 0) {
      ileObject.headers = [];

      cache.includes.forEach((include: IncludeStatement) => {
        // RPGLE includes are always returned as posix paths
        // even on Windows. We need to do some magic to convert here for Windows systems
        include.toPath = toLocalPath(include.toPath);

        const includeDetail = path.parse(include.toPath);

        if (includeDetail.ext.toLowerCase() !== `.rpgleinc`) {
          const possibleName = includeDetail.name.toLowerCase().endsWith(`.pgm`) ? includeDetail.name.substring(0, includeDetail.name.length - 4) : includeDetail.name;

          if (targets.suggestions.renames) {
            const renameLogPath = targets.getRelative(include.toPath);

            // We need to make sure the .rpgleinc rename is most important
            if (targets.logger.exists(renameLogPath, `rename`)) {
              targets.logger.flush(renameLogPath);
            }

            targets.logger.fileLog(renameLogPath, {
              message: `Rename suggestion`,
              type: `rename`,
              change: {
                rename: {
                  path: include.toPath,
                  newName: `${possibleName}.rpgleinc`
                }
              }
            });
          } else {
            if (!targets.shouldAssumePrograms()) {
              targets.logger.fileLog(targets.getRelative(include.toPath), {
                message: `referenced as include, but should use the '.rpgleinc' extension.`,
                type: `warning`,
              });
            }
          }
        }

        const theIncludePath = asPosix(targets.getRelative(include.toPath));

        ileObject.headers.push(theIncludePath);

        if (targets.suggestions.includes) {
          targets.logger.fileLog(ileObject.relativePath, {
            message: `Will update to use unix style path.`,
            type: `includeFix`,
            line: include.line,
            change: {
              lineContent: (isFree ? `` : ``.padEnd(6)) + `/copy '${theIncludePath}'`
            }
          });
        } else {
          targets.logger.fileLog(ileObject.relativePath, {
            message: `Include at line ${include.line} found, to path '${theIncludePath}'`,
            type: `info`,
            line: include.line,
          });
        }

        if (targets.shouldAssumePrograms()) {
          const mistakenObject = targets.getResolvedObject(include.toPath);
          if (mistakenObject) {
            targets.removeObject(mistakenObject);
          }
        }
      });
    }

    const target: ILEObjectTarget = {
      ...ileObject,
      deps: []
    };

    // This usually means .pgm is in the name
    if (ileObject.type === `PGM` && cache.keyword[`NOMAIN`]) {
      const possibleName = pathDetail.name.toLowerCase().endsWith(`.pgm`) ? pathDetail.name.substring(0, pathDetail.name.length - 4) : pathDetail.name;

      if (targets.suggestions.renames) {
        targets.logger.fileLog(ileObject.relativePath, {
          message: `Rename suggestion`,
          type: `rename`,
          change: {
            rename: {
              path: localPath,
              newName: possibleName + pathDetail.ext
            }
          }
        })
      } else {
        targets.logger.fileLog(ileObject.relativePath, {
          message: `type detected as ${ileObject.type} but NOMAIN keyword found.`,
          type: `warning`,
        });
      }
    }

    // This usually means it's source name is a module (no .pgm) but doesn't have NOMAIN.
    // We need to do this for other language too down the line
    if (ileObject.type === `MODULE` && !cache.keyword[`NOMAIN`]) {
      if (targets.suggestions.renames) {
        targets.logger.fileLog(ileObject.relativePath, {
          message: `Rename suggestion`,
          type: `rename`,
          change: {
            rename: {
              path: localPath,
              newName: pathDetail.name + `.pgm` + pathDetail.ext
            }
          }
        });
      } else {
        targets.logger.fileLog(ileObject.relativePath, {
          message: `type detected as ${ileObject.type} but NOMAIN keyword was not found.`,
          type: `warning`,
        });
      }
    }

    if (cache.keyword[`BNDDIR`]) {
      targets.logger.fileLog(ileObject.relativePath, {
        message: `has the BNDDIR keyword. Binding directory should be set at global level or object level.`,
        type: `info`,
      });
    }

    // Find external programs
    cache.procedures
      .filter((proc: any) => proc.keyword[`EXTPGM`])
      .map((ref): RpgLookup => {
        const keyword = ref.keyword;
        let fileName = ref.name;
        const extpgm = keyword[`EXTPGM`];
        if (extpgm) {
          if (extpgm === true) fileName = ref.name;
          else fileName = trimQuotes(extpgm);
        }

        return {
          lookup: fileName.toUpperCase(),
          line: ref.position ? ref.position.range.line : undefined
        };
      })
      .forEach((ref: RpgLookup) => {
        // Don't add ignored objects (usually system APIs)
        if (Targets.ignoredObjects.includes(ref.lookup)) return;
        // Don't add itself
        if (ref.lookup === ileObject.systemName) return;

        const resolvedObject = targets.searchForObject({ systemName: ref.lookup, type: `PGM` });
        if (resolvedObject) {
          // because of legacy fixed CALL, there can be dupliicate EXTPGMs with the same name :(
          if (!target.deps.some(d => d.systemName === resolvedObject.systemName && d.type && resolvedObject.type)) {
            target.deps.push(resolvedObject)
          }
        }

        else {
          targets.logger.fileLog(ileObject.relativePath, {
            message: `No object found for reference '${ref.lookup}'`,
            type: `warning`,
            line: ref.line
          });
        }
      });

    // Scan the multiple scopes available in an RPGLE program
    const scopes = [cache, ...cache.procedures.map(p => p.scope)].filter(s => s);

    for (const scope of scopes) {

      // Find external data structure sources
      scope.structs
        .filter((struct: any) => struct.keyword[`EXTNAME`])
        .map((struct): RpgLookup => {
          const keyword = struct.keyword;
          const value = trimQuotes(keyword[`EXTNAME`]);

          return {
            lookup: value.split(`:`)[0].toUpperCase(),
            line: struct.position ? struct.position.range.line : undefined
          };
        })
        .forEach((ref: RpgLookup) => {
          const resolvedObject = targets.searchForObject({ systemName: ref.lookup, type: `FILE` });
          if (resolvedObject) target.deps.push(resolvedObject)
          else {
            targets.logger.fileLog(ileObject.relativePath, {
              message: `No object found for reference '${ref.lookup}'`,
              type: `warning`,
              line: ref.line
            });
          }
        });

      // Find external files
      scope.files
        .map((file): RpgLookup => {
          let possibleName: string = file.name;
          const keyword = file.keyword;

          const extNameValue = keyword[`EXTFILE`];
          if (extNameValue) {
            possibleName = trimQuotes(extNameValue).split(`:`)[0]
          }

          if (possibleName.toLowerCase() === `*extdesc`) {
            const extDescValue = keyword[`EXTDESC`];
            if (extDescValue) {
              possibleName = trimQuotes(extDescValue);
            } else {
              targets.logger.fileLog(ileObject.relativePath, {
                message: `*EXTDESC is used for '${file.name}' but EXTDESC keyword not found`,
                type: `warning`,
              });
            }
          }

          return {
            lookup: possibleName.toUpperCase(),
            line: file.position ? file.position.range.line : undefined
          };
        })
        .forEach((ref: RpgLookup) => {
          if (Targets.ignoredObjects.includes(ref.lookup)) return;

          const previouslyScanned = target.deps.some((r => (ref.lookup === r.systemName || ref.lookup === r.longName?.toUpperCase()) && r.type === `FILE`));
          if (previouslyScanned) return;

          const resolvedObject = targets.searchForObject({ systemName: ref.lookup, type: `FILE` });
          if (resolvedObject) target.deps.push(resolvedObject)
          else {
            targets.logger.fileLog(ileObject.relativePath, {
              message: `No object found for reference '${ref.lookup}'`,
              type: `warning`,
              line: ref.line
            });
          }
        });

      // We ignore anything with hardcoded schemas
      scope.sqlReferences
        .filter(ref => !ref.description)
        .map((ref): RpgLookup => ({
          lookup: trimQuotes(ref.name, `"`).toUpperCase(),
          line: ref.position ? ref.position.range.line : undefined
        }))
        .forEach((ref: RpgLookup) => {
          const previouslyScanned = target.deps.some((r => (ref.lookup === r.systemName || ref.lookup === r.longName?.toUpperCase()) && r.type === `FILE`));
          if (previouslyScanned) return;
          const resolvedObject = targets.searchForObject({ systemName: ref.lookup, type: `FILE` });
          if (resolvedObject) target.deps.push(resolvedObject)
          else if (!isSqlFunction(ref.lookup)) {
            targets.logger.fileLog(ileObject.relativePath, {
              message: `No object found for reference '${ref.lookup}'`,
              type: `warning`,
              line: ref.line
            });
          }
        });

      // Find external data areas
      scope.structs
        .filter((struct: any) => struct.keyword[`DTAARA`])
        .map((ref): RpgLookup => {
          const keyword = ref.keyword;
          let fileName: string = ref.name;
          const dtaara = keyword[`DTAARA`];
          if (dtaara) {
            if (dtaara === true) fileName = ref.name;
            else fileName = trimQuotes(dtaara);
          }

          return {
            lookup: fileName.toUpperCase(),
            line: ref.position ? ref.position.range.line : undefined
          };
        })
        .forEach((ref: RpgLookup) => {
          if (Targets.ignoredObjects.includes(ref.lookup.toUpperCase())) return;

          const resolvedObject = targets.searchForObject({ systemName: ref.lookup, type: `DTAARA` });
          if (resolvedObject) target.deps.push(resolvedObject)
          else {
            targets.logger.fileLog(ileObject.relativePath, {
              message: `No object found for reference '${ref.lookup}'`,
              type: `warning`,
              line: ref.line
            });
          }
        });

      scope.variables
        .filter((struct: any) => struct.keyword[`DTAARA`])
        .map((ref): RpgLookup => {
          const keyword = ref.keyword;
          let fileName: string = ref.name;
          const dtaara = keyword[`DTAARA`];
          if (dtaara) {
            if (dtaara === true) fileName = ref.name;
            else fileName = trimQuotes(dtaara);
          }

          return {
            lookup: fileName.toUpperCase(),
            line: ref.position ? ref.position.range.line : undefined
          };
        })
        .forEach((ref: RpgLookup) => {
          const resolvedObject = targets.searchForObject({ systemName: ref.lookup, type: `DTAARA` });
          if (resolvedObject) target.deps.push(resolvedObject)
          else {
            targets.logger.fileLog(ileObject.relativePath, {
              message: `No object found for reference '${ref.lookup}'`,
              type: `warning`,
              line: ref.line
            });
          }
        });
    }

    // TODO: did we duplicate this?
    // We also look to see if there is a `.cmd` object with the same name
    const resolvedObject = targets.searchForObject({ systemName: ileObject.systemName, type: `CMD` });
    if (resolvedObject) targets.createOrAppend(resolvedObject, target);

    if (target.deps.length > 0)
      infoOut(`Depends on: ${target.deps.map(d => `${d.systemName}.${d.type}`).join(` `)}`);

    targets.addNewTarget(target);
  }
}

function setupParser(targets: Targets): Parser {
  const parser = new Parser();

  parser.setIncludeFileFetch(async (baseFile: string, includeFile: string) => {
    if (includeFile.startsWith(`'`) && includeFile.endsWith(`'`)) {
      includeFile = includeFile.substring(1, includeFile.length - 1);
    }

    let file: string;

    if (includeFile.includes(`,`)) {
      // If the member include path is qualified with a source file
      // then we should convert to be a unix style path so we can
      // search the explicit directories.
      includeFile = includeFile.replace(/,/g, `/`) + `.*`;

      // Keep making the path less specific until we find a possible include
      let parts = includeFile.split(`/`);
      while (!file && parts.length > 0) {
        file = await targets.resolveLocalFile(includeFile);

        if (!file) {
          parts.shift();
          includeFile = parts.join(`/`);
        }
      }
    } else if (!includeFile.includes(`/`)) {
      const parent = path.basename(path.dirname(baseFile));
      includeFile = `${parent}/${includeFile}`;


      file = await targets.resolveLocalFile(includeFile);
    } else {
      file = await targets.resolveLocalFile(includeFile);
    }

    if (file) {
      if (includeFileCache[file]) {
        return {
          found: true,
          uri: file,
          content: includeFileCache[file]
        }

      } else {
        const content = await targets.rfs.readFile(file);
        includeFileCache[file] = content;

        return {
          found: true,
          uri: file,
          content: content
        }
      }
    }

    return {
      found: false
    };
  });

  parser.setTableFetch(async (table: string, aliases = false) => {
    // Can't support tables in CLI mode I suppose?
    return [];
  });

  return parser;
}
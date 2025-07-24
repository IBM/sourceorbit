import path from "path";
import { FileOptions, ILEObject, ILEObjectTarget, Targets } from "..";
import { infoOut } from "../../cli";

import Document from "vscode-db2i/src/language/sql/document";
import { StatementType } from 'vscode-db2i/src/language/sql/types';
import { isSqlFunction } from "../../languages/sql";
import { trimQuotes } from "../../utils";
import { ExtensionMap } from "../languages";

const sqlTypeExtension = {
  'TABLE': `table`,
  'VIEW': `view`,
  'PROCEDURE': `sqlprc`,
  'FUNCTION': `sqludf`,
  'TRIGGER': `sqltrg`,
  'ALIAS': `sqlalias`,
  'SEQUENCE': `sqlseq`
};

export const sqlExtensions = [`sql`, `table`, `view`, `index`, `alias`, `sqlprc`, `sqludf`, `sqludt`, `sqltrg`, `sqlalias`, `sqlseq`];
export const sqlObjects: ExtensionMap = {
  'sql': `FILE`,
  'table': `FILE`,
  'view': `FILE`,
  'index': `FILE`,
  'alias': `FILE`,
  'sqludf': `FILE`,
  'sqludt': `FILE`,
  'sqlalias': `FILE`,
  'sqlseq': `FILE`,
  'sequence': `FILE`,
  'function': `SRVPGM`,
  'procedure': `PGM`,
  'sqlprc': `PGM`,
  'trigger': `PGM`,
  'sqltrg': `PGM`
}

export async function sqlTargetCallback(targets: Targets, localPath: string, content: string, ileObject: ILEObject) {
  const document = new Document(content);

  const pathDetail = path.parse(localPath);
  const relativePath = targets.getRelative(localPath);

  const groups = document.getStatementGroups();

  // TODO: Note, this returns high level definitions.
  // If the index/view/etc specifies a table dep,
  // they will not appear as a dependency

  const createCount = groups.filter(g => g.statements[0].type === StatementType.Create).length;

  if (createCount > 1) {
    targets.logger.fileLog(relativePath, {
      message: `Includes multiple create statements. They should be in individual sources. This file will not be parsed.`,
      type: `warning`,
    });

    return;
  }

  for (const group of groups) {
    const statement = group.statements[0];
    const defs = statement.getObjectReferences();
    const mainDef = defs[0];

    if (mainDef && mainDef.createType && mainDef.object.name) {
      const tokens = mainDef.tokens;
      if (mainDef.object.schema) {
        targets.logger.fileLog(relativePath, {
          message: `${mainDef.object.schema}/${mainDef.object.name} (${mainDef.createType}) reference not included as possible reference to library found.`,
          range: {
            start: tokens[0].range.start,
            end: tokens[tokens.length - 1].range.end
          },
          type: `warning`,
        });

      } else {
        switch (statement.type) {
          // Alters are a little weird in that they can exist
          // in any file, so we can't assume the current source
          // is the name of the object. Sad times
          case StatementType.Alter:
            // We don't do anything for alter currently
            // because it's too easy to create circular deps.
            // This is bad!!
            targets.logger.fileLog(relativePath, {
              message: `${mainDef.object.name} (${mainDef.createType}) alter not tracked due to possible circular dependency.`,
              range: {
                start: tokens[0].range.start,
                end: tokens[tokens.length - 1].range.end
              },
              type: `info`,
            });

            // let currentTarget: ILEObjectTarget|undefined;
            // const resolvedPath = targets.resolveLocalObjectQuery(mainDef.object.name + `.*`);
            // const currentRelative = path.basename(resolvedPath);
            // if (resolvedPath) { 
            // 		currentTarget = {
            // 		...targets.resolveObject(resolvedPath),
            // 		deps: []
            // 	};
            // }

            // if (currentTarget) {
            // 	info(`${currentTarget.name}.${currentTarget.type}`);
            // 	info(`\tSource: ${currentTarget.relativePath}`);

            // 	if (defs.length > 1) {
            // 		for (const def of defs.slice(1)) {
            // 			const subResolvedPath = targets.resolveLocalObjectQuery(def.object.name + `.*`, currentRelative);
            // 			if (subResolvedPath) currentTarget.deps.push(targets.resolveObject(subResolvedPath))
            // 			else info(`\tNo object found for reference '${def.object.name}'`);
            // 		}
            // 	}

            // 	if (currentTarget.deps.length > 0) {
            // 		info(`Depends on: ${currentTarget.deps.map(d => `${d.name}.${d.type}`).join(` `)}`);

            // 		targets.pushDep(currentTarget);
            // 	}
            // }
            break;

          // Creates should be in their own unique file
          case StatementType.Create:
            let hasLongName = mainDef.object.name && mainDef.object.name.length > 10 ? mainDef.object.name : undefined;
            let objectName = mainDef.object.system || trimQuotes(mainDef.object.name, `"`);

            // let ileObject: ILEObject = {
            //   systemName: objectName.toUpperCase(),
            //   longName: hasLongName,
            //   type: targets.getObjectType(relativePath, mainDef.createType),
            //   text: defaultObject.text,
            //   relativePath,
            //   extension
            // }

            ileObject.systemName = objectName.toUpperCase();
            ileObject.longName = hasLongName;
            ileObject.type = targets.getObjectType(relativePath, mainDef.createType);

            let suggestRename = false;
            const sqlFileName = pathDetail.name;

            // First check the file name
            if (ileObject.systemName.length <= 10) {
              if (ileObject.systemName.toUpperCase() !== sqlFileName.toUpperCase() && ileObject.longName !== sqlFileName) {
                suggestRename = true;
              }
            }

            // Then make an extension suggestion
            if (ileObject.extension.toUpperCase() === `SQL` && mainDef.createType) {
              suggestRename = true;
            }

            // Let them know to use a system name in the create statement if one is not present
            if (ileObject.systemName.length > 10 && mainDef.object.system === undefined) {
              targets.logger.fileLog(ileObject.relativePath, {
                message: `${ileObject.systemName} (${ileObject.type}) name is longer than 10 characters. Consider using 'FOR SYSTEM NAME' in the CREATE statement.`,
                type: `warning`,
                range: {
                  start: tokens[0].range.start,
                  end: tokens[tokens.length - 1].range.end
                },
              });

              suggestRename = false;
            }

            let newTarget: ILEObjectTarget = {
              ...ileObject,
              deps: []
            };

            infoOut(`${newTarget.systemName}.${newTarget.type}: ${newTarget.relativePath}`);

            // Now, let's go through all the other statements in this group (BEGIN/END)
            // and grab any references to other objects :eyes:
            let otherDefs = defs.slice(1);

            for (let i = 1; i < group.statements.length; i++) {
              const currentStatement = group.statements[i];
              if ([StatementType.Alter, StatementType.Insert, StatementType.Delete, StatementType.With, StatementType.Select, StatementType.Call].includes(currentStatement.type)) {
                otherDefs.push(...group.statements[i].getObjectReferences());
              }
            }

            for (const def of otherDefs) {
              const refTokens = def.tokens;
              const simpleName = trimQuotes(def.object.name, `"`);
              // TODO: do we need to look for SRVPGM (function) or PGM (procedure) here?
              const resolvedObject = targets.searchForAnyObject({ name: simpleName, types: [`FILE`, `SRVPGM`, `PGM`] });
              if (resolvedObject) {
                if (!newTarget.deps.find(d => d.systemName === resolvedObject.systemName && d.type === resolvedObject.type)) {
                  newTarget.deps.push(resolvedObject);
                }
              }
              else if (!isSqlFunction(def.object.name)) {
                targets.logger.fileLog(newTarget.relativePath, {
                  message: `No object found for reference '${def.object.name}'`,
                  type: `warning`,
                  range: {
                    start: refTokens[0].range.start,
                    end: refTokens[refTokens.length - 1].range.end
                  },
                });
              }
            }

            if (newTarget.deps.length > 0) {
              infoOut(`Depends on: ${newTarget.deps.map(d => `${d.systemName}.${d.type}`).join(` `)}`);
            }

            targets.addNewTarget(newTarget);

            // If the extension is SQL, let's make better suggestions
            // based on the create type in the CREATE statement
            if (suggestRename) {
              const newExtension = sqlTypeExtension[mainDef.createType.toUpperCase()];

              if (newExtension) {
                const possibleName = (ileObject.longName ? ileObject.longName : ileObject.systemName.toLowerCase()) + `.` + newExtension;

                if (targets.suggestions.renames) {
                  const renameLogPath = relativePath;

                  // We need to make sure the .rpgleinc rename is most important
                  if (targets.logger.exists(renameLogPath, `rename`)) {
                    targets.logger.flush(renameLogPath);
                  }

                  targets.logger.fileLog(renameLogPath, {
                    message: `Rename suggestion`,
                    type: `rename`,
                    change: {
                      rename: {
                        path: localPath,
                        newName: possibleName
                      }
                    }
                  });
                } else {
                  targets.logger.fileLog(relativePath, {
                    message: `Extension should be based on type. Suggested name is '${possibleName}'`,
                    type: `warning`,
                  });
                }
              }
            }

            break;
        }

      }
    }
  }
}
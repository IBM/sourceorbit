import { warningOut } from "../../cli";
import { ObjectType, Targets } from "../../targets";
import { getFiles } from "../../utils";
import { readFileSync } from "fs";
import * as path from "path";
import { MakeProject } from ".";

/**
 * Scan for all rules.mk files and read attributes and custom
 * dependencies into the targets.
 */
export function readAllRules(targets: Targets, project: MakeProject) {
  // getFiles is case insensitive
  const rulesFiles = getFiles(targets.getCwd(), `**/rules.mk`);

  const settings = project.getSettings();

  for (const rulesFile of rulesFiles) {
    const relative = targets.getRelative(rulesFile);

    try {
      const content = readFileSync(rulesFile, { encoding: `utf-8` });
      const lines = content.split(`\n`);

      for (const line of lines) {
        if (line.trim().length === 0) continue; // Skip empty lines
        if (line[0] === `#`) continue; // Skip comments
        if (line[0] === ` ` || line[0] === `\t`) continue; // Skip lines that start with a space or tab

        const nameSplit = line.indexOf(`:`);
        if (nameSplit < 0) continue;

        const name = line.substring(0, nameSplit).trim().toUpperCase();
        const value = line.substring(nameSplit + 1).trim();

        let assignmentSplit = -1;
        let assignmentSplitLength = 0;

        // Assignment can be with multiple operators
        const assignmentOps = [`:=`, `=`];
        for (const op of assignmentOps) {
          assignmentSplit = value.indexOf(op);
          if (assignmentSplit >= 0) {
            assignmentSplitLength = op.length;
            break;
          }
        }

        if (assignmentSplit >= 0) {
          // If there is an assignment value, this means we're
          // setting a compile parameter on a specific object
          const key = value.substring(0, assignmentSplit).trim().toLowerCase();
          const val = value.substring(assignmentSplit + assignmentSplitLength).trim();

          if (!settings.objectAttributes[name]) settings.objectAttributes[name] = {};

          settings.objectAttributes[name][key] = val;

        } else {
          // Otherwise, we're overriding the deps with hardcoded deps

          const nameParts = name.split(`.`);
          const targetName = nameParts[0];
          const targetType = nameParts[1] as ObjectType;

          if (targetName && targetType) {
            const currentTarget = targets.getTarget({systemName: targetName, type: targetType});

            if (currentTarget) {
              // We set this to empty since we're overriding them
              currentTarget.deps = [];

              const parts = value.split(` `).map(p => p.trim()).filter(p => p.length > 0);

              // We always skip the first because ibmi-bob wants the first entry to be the source name.
              for (let i = 1; i < parts.length; i++) {
                const part = parts[i];
                const partSplit = part.split(`.`);
                const objName = partSplit[0];
                const objType = partSplit[1] as ObjectType;

                if (objName && objType) {
                  const obj = targets.searchForObject({systemName: objName, type: objType});

                  if (obj) {
                    currentTarget.deps.push(obj);
                  } else {
                    warningOut(`make: Failed to find '${part}' in '${relative}'`);
                  }
                }
              }
            }
        }

        }
      }

    } catch (e) {
      warningOut(`make: Failed to read ${relative}.`);
    }
  }
}
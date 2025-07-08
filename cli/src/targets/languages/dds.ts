import path from "path";
import { DisplayFile as dds } from "vscode-displayfile/src/dspf";
import { FileOptions, ILEObjectTarget, Targets } from "..";
import { infoOut } from "../../cli";

export const ddsExtension = [`pf`, `lf`, `dspf`, `prtf`];

export async function ddsTargetCallback(targets: Targets, filePath: string, content: string, options: FileOptions) {
  const eol = content.indexOf(`\r\n`) >= 0 ? `\r\n` : `\n`;

  const ddsFile = new dds();
  ddsFile.parse(content.split(eol));

  const ileObject = await targets.resolvePathToObject(filePath, options.text);

  const target: ILEObjectTarget = {
    ...ileObject,
    deps: []
  };

  infoOut(`${ileObject.systemName}.${ileObject.type}: ${ileObject.relativePath}`);

  // We have a local cache of refs found so we don't keep doing global lookups
  // on objects we already know to depend on in this object.

  let alreadyFoundRefs: string[] = [];

  const handleObjectPath = (currentKeyword: string, recordFormat: any, value: string) => {
    const qualified = value.split(`/`);

    let objectName: string | undefined;
    if (qualified.length === 2 && qualified[0].toLowerCase() === `*libl`) {
      objectName = qualified[1];
    } else if (qualified.length === 1) {
      objectName = qualified[0];
    }

    if (objectName) {
      const upperName = objectName.toUpperCase();
      if (alreadyFoundRefs.includes(upperName)) return;

      const resolvedPath = targets.searchForObject({ systemName: upperName, type: `FILE` });
      if (resolvedPath) {
        target.deps.push(resolvedPath);
        alreadyFoundRefs.push(upperName);
      }
      else {
        targets.logger.fileLog(ileObject.relativePath, {
          message: `no object found for reference '${objectName}'`,
          type: `warning`,
          line: recordFormat.range.start
        });
      }
    } else {
      targets.logger.fileLog(ileObject.relativePath, {
        message: `${currentKeyword} reference not included as possible reference to library found.`,
        type: `info`,
        line: recordFormat.range.start
      });
    }
  }

  // PFILE -> https://www.ibm.com/docs/en/i/7.5?topic=80-pfile-physical-file-keywordlogical-files-only
  // REF -> https://www.ibm.com/docs/en/i/7.5?topic=80-ref-reference-keywordphysical-files-only

  const ddsRefKeywords = [`PFILE`, `REF`, `JFILE`];

  for (const recordFormat of ddsFile.formats) {

    // Look through this record format keywords for the keyword we're looking for
    for (const keyword of ddsRefKeywords) {
      const keywordObj = recordFormat.keywords.find(k => k.name === keyword);
      if (keywordObj) {
        const wholeValue: string = keywordObj.value;
        const parts = wholeValue.split(` `).filter(x => x.length > 0);

        // JFILE can have multiple files referenced in it, whereas 
        // REF and PFILE can only have one at the first element
        const pathsToCheck = (keyword === `JFILE` ? parts.length : 1);

        for (let i = 0; i < pathsToCheck; i++) {
          handleObjectPath(keyword, recordFormat, parts[i]);
        }
      }
    }

    // REFFLD -> https://www.ibm.com/docs/en/i/7.5?topic=80-reffld-referenced-field-keywordphysical-files-only

    // Then, let's loop through the fields in this format and see if we can find REFFLD
    for (const field of recordFormat.fields) {
      const refFld = field.keywords.find(k => k.name === `REFFLD`);

      if (refFld) {
        const [fieldRef, fileRef] = refFld.value.trim().split(` `);

        if (fileRef) {
          handleObjectPath(`REFFLD`, recordFormat, fileRef);
        }
      }
    }
  }

  if (target.deps.length > 0)
    infoOut(`Depends on: ${target.deps.map(d => `${d.systemName}.${d.type}`).join(` `)}`);

  targets.addNewTarget(target);
}
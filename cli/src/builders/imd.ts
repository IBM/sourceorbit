import { existsSync, readFileSync, writeFileSync } from 'fs';
import path from 'path';
import { ILEObject, Targets } from '../targets';

const TypeEmoji = {
  "PGM": ":hammer_and_pick:",
  "SRVPGM": ":package:",
  "MODULE": ":pick:",
  "FILE": ":open_file_folder:",
  "BNDDIR": ":closed_book:",
  "MENU": ":scroll:",
  "DTAARA": ":page_with_curl:",
  "CMD": ":desktop_computer:"
}

const LogEmoji = {
  'warning': `:warning:`,
  'info': `:information_source:`
}

export class ImpactMarkdown {
	constructor(private cwd: string, private targets: Targets, private relativePaths: string[]) { }

  getContent() {
    let lines: string[] = [`## Impact Analysis`, ``];

    const possibleObjects = this.relativePaths.map(r => this.targets.getResolvedObject(path.join(this.cwd, r))).filter(x => x && x.relativePath);

    lines.push(
      `Touched objects: `, ``, 
      ...possibleObjects.map(ileObject => `* ${TypeEmoji[ileObject.type] || `:question:`} \`${ileObject.name}.${ileObject.type}\`: \`${ileObject.relativePath}\``), 
      ``,
      `---`,
      ``
    );

    for (const ileObject of possibleObjects) {
      const newLines = this.getImpactFor(ileObject);

      lines.push(
        `#### \`${ileObject.name}.${ileObject.type}\``,
        ``,
        ...newLines,
        ``
      )
    }

    lines.push(
      `## Messages`,
      ``
    );

    let hasShownMessages = false;

    for (const relativePath in this.relativePaths) {
      const fileLogs = this.targets.logger.getLogsFor(path.join(this.cwd, relativePath))

      if (fileLogs && fileLogs.length > 0) {
        hasShownMessages = true;
        lines.push(
          `##### \`${relativePath}\``,
          ``,
          ...fileLogs.map(log => `* ${LogEmoji[log.type] || `:grey_question:`} ${log.message}`),
          ``
        )
      }
    }

    if (!hasShownMessages) {
      lines.push(`No messages to show.`);
    }

    return lines;
  }

  getImpactFor(theObject: ILEObject) {
    let lines: string[] = [];

    const allDeps = this.targets.getDeps();
    let currentTree: ILEObject[] = [];
  
    function lookupObject(ileObject: ILEObject) {
      lines.push(`${''.padEnd(currentTree.length, `\t`)}* ${TypeEmoji[ileObject.type] || `:question:`} \`${ileObject.name}.${ileObject.type}\` (${ileObject.relativePath ? `\`${ileObject.relativePath}\`` : `no source`})`);
  
      currentTree.push(ileObject);
  
      for (const target of allDeps) {
        const containsLookup = target.deps.some(d => d.name === ileObject.name && d.type === ileObject.type);
        const circurlar = currentTree.some(d => d.name === target.name && d.type === target.type);
  
        if (containsLookup && !circurlar) {
          lookupObject(target);
        }
      }
  
      currentTree.pop();
    }
  
    lookupObject(theObject);

    if (lines.length === 1) {
      lines.push(
        ``,
        `Changes to this object have no impact.`
      )
    }

    return lines;
  }
}
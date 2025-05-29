import path from 'path';
import { ILEObject, Targets } from '../targets';
import { FileLog } from '../logger';

const TypeEmoji = {
  "PGM": "🛠️",
  "SRVPGM": "📦",
  "MODULE": "⛏️",
  "FILE": "📁",
  "BNDDIR": "📒",
  "MENU": "📜",
  "DTAARA": "📃",
  "CMD": "⌨️"
}

const LogEmoji = {
  'warning': `⚠️`,
  'info': `ℹ️`,
  'none': `✅`
}

export class ImpactMarkdown {
  constructor(private cwd: string, private targets: Targets, private relativePaths: string[]) { }

  getContent() {
    let lines: string[] = [];

    // First let's build a touched objects list

    if (this.relativePaths.length > 0) {
      lines.push(`## Impact Analysis`, ``);

      const possibleObjects = this.relativePaths.map(r => this.targets.getResolvedObject(path.join(this.cwd, r))).filter(x => x && x.source);

      lines.push(
        `Touched objects: `, ``,
        ...possibleObjects.map(ileObject => `* ${TypeEmoji[ileObject.type] || `❔`} \`${ileObject.systemName}.${ileObject.type}\`: \`${ileObject.source.relativePath}\``),
        ``,
        `---`,
        ``
      );

      // Then for each of those, we print the impact

      for (const ileObject of possibleObjects) {
        const newLines = this.getImpactFor(ileObject);

        lines.push(
          `### \`${ileObject.systemName}.${ileObject.type}\``,
          ``,
          `<details><summary>Click to expand</summary><br>`,
          ``,
          ...newLines,
          ``,
          `</details>`,
          ``
        )
      }

      // Then we add any messages from the logger

      lines.push(
        `## Messages`,
        ``
      );

      let hasShownMessages = false;

      for (const relativePath of this.relativePaths) {
        const fileLogs = this.targets.logger.getLogsFor(relativePath)

        if (fileLogs && fileLogs.length > 0) {
          hasShownMessages = true;
          lines.push(
            `##### \`${relativePath}\``,
            ``,
            ...fileLogs.map(log => `* ${LogEmoji[log.type] || `❔`} ${log.message}`),
            ``
          )
        }
      }

      if (!hasShownMessages) {
        lines.push(`No messages to show.`);
      }

      lines.push(``, `---`, ``);
    }

    // Then let's add full project listing for reference

    lines.push(
      ``,
      `## Project Listing`,
      ``,
      `<details><summary>Click to expand</summary><br>`,
      ``,
      ...this.getObjectList(),
      ``,
      `</details>`,
      ``,
    )

    const allFiles = this.targets.logger.getAllLogs();
    let warningMarkdown: string[] = [];

    for (const relativePath in allFiles) {
      const logs = allFiles[relativePath];
      if (logs.some(l => l.type === 'warning')) {
        warningMarkdown.push(
          `**\`${relativePath}\`**`,
          ``,
          ...logs.filter(l => l.type === 'warning').map(log => `* [ ] ${LogEmoji[log.type] || `❔`} ${log.message}`),
          ``
        )
      }
    }

    if (warningMarkdown.length > 0) {
      lines.push(
        `### Warnings`,
        ``,
        `<details><summary>Click to expand</summary><br>`,
        ``,
        ...warningMarkdown,
        ``,
        `</details>`,
        ``
      )
    }

    return lines;
  }

  getImpactFor(theObject: ILEObject) {
    let lines: string[] = [];

    const allDeps = this.targets.getTargets();
    let currentTree: ILEObject[] = [];

    function lookupObject(ileObject: ILEObject) {
      let resultLines: string[] = [];

      resultLines.push(`${''.padEnd(currentTree.length, `\t`)}* ${TypeEmoji[ileObject.type] || `❔`} \`${ileObject.systemName}.${ileObject.type}\` (${ileObject.source ? `\`${ileObject.source.relativePath}\`` : `no source`})`);

      currentTree.push(ileObject);

      for (const target of allDeps) {
        const containsLookup = target.deps.some(d => d.systemName === ileObject.systemName && d.type === ileObject.type);
        const circular = currentTree.some(d => d.systemName === target.systemName && d.type === target.type);

        if (containsLookup && !circular) {
          resultLines.push(...lookupObject(target));
        }
      }

      currentTree.pop();
      return resultLines;
    }

    const depTreeMd = lookupObject(theObject);

    if (depTreeMd.length === 1) {
      lines.push(
        ``,
        `Changes to this object have no impact.`
      )
    } else {
      lines.push(...depTreeMd);
    }

    return lines;
  }

  getObjectList() {
    let lines = [];

    lines.push(`| - | Object | Type | Path | Warnings | Parents | Children |`);
    lines.push(`| --- | --- | --- | --- | --- | --- | --- |`);

    this.targets.getResolvedObjects().forEach(ileObject => {

      let logs = this.targets.logger.getLogsFor(ileObject.source?.relativePath) || [];
      let parents = this.targets.getTargets().filter(t => t.deps.some(d => d.systemName === ileObject.systemName && d.type === ileObject.type));
      let children = this.targets.getTarget(ileObject)?.deps || [];

      lines.push(`| ` + [
        TypeEmoji[ileObject.type] || `❔`,
        ileObject.systemName,
        ileObject.type,
        `\`${ileObject.source?.relativePath}\``,
        ImpactMarkdown.createLogExpand(logs),
        ImpactMarkdown.createObjectExpand(parents),
        ImpactMarkdown.createObjectExpand(children)
      ].join(' | ') + ` |`);
    });
    
    lines.push(``, `* *Parents* are objects that depend on this object.`, `* *Children* are objects that this object depends on.`);

    return lines;
  }

  static createObjectExpand(objs: ILEObject[]) {
    return objs.length === 0 ? `0` : `<details><summary>${objs.length}</summary>${objs.map(o => `${o.systemName}.${o.type}`).join(`, `)}</details>`;
  }

  static createLogExpand(logs: FileLog[] = []) {
    let logIcon = LogEmoji.none;

    if (logs.length > 0) {
      logIcon = logs.some(l => l.type === 'warning') ? LogEmoji.warning : LogEmoji.info;
    }

    return logs.length === 0 ? `${logIcon}` : `<details><summary>${logIcon}</summary><br>${logs.map(o => o.message).join(`<br>`)}</details>`;
  }
}
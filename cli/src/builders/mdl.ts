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

export class MarkdownListing {
	constructor(private cwd: string, private targets: Targets) { }

  getContent() {
    let lines = [];

    lines.push(`| - | Object | Type | Path | Warnings | Children | Parents |`);
    lines.push(`| --- | --- | --- | --- | --- | --- | --- |`);

    this.targets.getResolvedObjects().forEach(ileObject => {

      let logs = this.targets.logger.getLogsFor(ileObject.relativePath);
      let parents = this.targets.getTargets().filter(t => t.deps.some(d => d.systemName === ileObject.systemName && d.type === ileObject.type));
      let children = this.targets.getTarget(ileObject).deps;

      lines.push(`| ` + [
        TypeEmoji[ileObject.type] || `:question:`,
        ileObject.systemName,
        ileObject.type,
        `\`${ileObject.relativePath}\``,
        MarkdownListing.createLogExpand(logs),
        MarkdownListing.createObjectExpand(children),
        MarkdownListing.createObjectExpand(parents)
      ].join(' | ') + ` |`);
    });

    return lines;
  }

  static createObjectExpand(objs: ILEObject[]) {
    return objs.length === 0 ? `0` : `<details><summary>${objs.length}</summary>${objs.map(o => `${o.systemName}.${o.type}`).join(`, `)}</details>`;
  }
  
  static createLogExpand(logs: FileLog[]) {
    let logIcon = LogEmoji.none;

    if (logs.length > 0) {
      logIcon = logs.some(l => l.type === 'warning') ? LogEmoji.warning : LogEmoji.info;
    }

    return logs.length === 0 ? `${logIcon}` : `<details><summary>${logIcon}</summary><br>${logs.map(o => o.message).join(`<br>`)}</details>`;
  }
}
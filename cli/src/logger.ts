import { infoOut, warningOut } from "./cli";

type LogType = "info"|"warning"|"includeFix"|"rename";

type FileLogs = {[path: string]: FileLog[]};

interface ChangeSuggestion {
  rename?: {
    path: string;
    newName: string;
  },
  lineContent?: string;
}

export interface FileLog {
  type: LogType;
  message: string;
  line?: number;
  range?: {
    start: number,
    end: number
  }
  change?: ChangeSuggestion;
}

export class Logger {
  private logs: FileLogs = {};

  constructor() {}

  flush(relativePath?: string) {
    if (relativePath) {
      this.logs[relativePath] = [];
    } else {
      this.logs = {}
    }
  }

  fileLog(relativePath: string, log: FileLog) {
    switch (log.type) {
      case `info`: infoOut(`${relativePath}${log.line ? `:${log.line}` : ``} - ${log.message}`); break;
      case `warning`: warningOut(`${relativePath}${log.line ? `:${log.line}` : ``} - ${log.message}`); break;
    }

    if (!this.logs[relativePath]) {
      this.logs[relativePath] = [];
    }

    if (log.type === `rename`) {
      // If this path already contains a rename, ignore this
      if (this.logs[relativePath].some(l => l.type === `rename`)) return;
    }

    this.logs[relativePath].push(log);
  }

  exists(relativePath: string, type: LogType) {
    return this.logs[relativePath] && this.logs[relativePath].some(l => l.type === type)
  }

  getAllLogs() {
    return this.logs;
  }

  getLogsFor(relativePath: string): FileLog[]|undefined {
    return this.logs[relativePath];
  }
}
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

  flush(specificPath?: string) {
    if (specificPath) {
      this.logs[specificPath] = [];
    } else {
      this.logs = {}
    }
  }

  fileLog(path: string, log: FileLog) {
    switch (log.type) {
      case `info`: infoOut(`${path}${log.line ? `:${log.line}` : ``} - ${log.message}`); break;
      case `warning`: warningOut(`${path}${log.line ? `:${log.line}` : ``} - ${log.message}`); break;
    }

    if (!this.logs[path]) {
      this.logs[path] = [];
    }

    if (log.type === `rename`) {
      // If this path already contains a rename, ignore this
      if (this.logs[path].some(l => l.type === `rename`)) return;
    }

    this.logs[path].push(log);
  }

  exists(path: string, type: LogType) {
    return this.logs[path] && this.logs[path].some(l => l.type === type)
  }

  getAllLogs() {
    return this.logs;
  }

  getLogsFor(path: string): FileLog[]|undefined {
    return this.logs[path];
  }
}
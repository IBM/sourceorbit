import ffs from 'fs';
import fs from 'fs/promises';
import { glob } from 'glob';

export class ReadFileSystem {
  async getFiles(cwd: string, globPath: string): Promise<string[]> {
    return glob.sync(globPath, {
      cwd,
      absolute: true,
      nocase: true,
    });
  }

  readFile(filePath: string): Promise<string> {
    return fs.readFile(filePath, { encoding: `utf8` });
  }

  async exists(filePath: string): Promise<boolean> {
    return ffs.existsSync(filePath);
  }
}
import fs from 'fs/promises';
import ffs from 'fs';
import glob from "glob";
import path from 'path';
import os from 'os';
import { getFiles } from './utils';

export class ReadFileSystem {
  constructor() {}

  async getFiles(cwd: string, globPath, additionalOpts: any = {}): Promise<string[]> {
    return getFiles(cwd, globPath, additionalOpts);
  }

  readFile(filePath: string): Promise<string> {
    return fs.readFile(filePath, { encoding: `utf8` });
  }

  async exists(filePath: string): Promise<boolean> {
    return ffs.existsSync(filePath);
  }
}
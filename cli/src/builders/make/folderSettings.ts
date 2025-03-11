import * as path from "path";
import { readFileSync } from "fs";
import { warningOut } from "../../cli";
import { ReadFileSystem } from "../../readFileSystem";
import { getFiles } from "../../utils";

export interface FolderOptions {
	version?: "0.0.1",
	build?: {
		// objlib?: string, We don't support objlib
		tgtCcsid?: string
	}
}

/**
 * Fetch the folder specific settings for a project
 */
export function getFolderOptions(cwd: string) {
  // Then fetch the directory specific settings
  const ibmiFiles = getFiles(cwd, `**/.ibmi.json`);

  let folderSettings: { [key: string]: FolderOptions } = {};

  for (const ibmiFile of ibmiFiles) {
    const relative = path.relative(cwd, ibmiFile);
    const folder = path.dirname(relative);

    try {
      folderSettings[folder] = JSON.parse(readFileSync(ibmiFile, { encoding: `utf-8` }));
    } catch (e) {
      warningOut(`make: Failed to read ${relative}.`);
    }
  }

  return folderSettings;
}
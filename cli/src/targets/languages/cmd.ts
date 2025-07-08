import { FileOptions, Targets } from "..";

export const cmdExtensions = [`cmd`];

export async function cmdTargetCallback(targets: Targets, localPath: string, content: string, options: FileOptions) {
  targets.resolvePathToObject(localPath, options.text);
}
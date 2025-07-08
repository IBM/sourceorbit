import { Targets, FileOptions } from "..";

export const noSourceObjects = [`dtaara`, `mnucmd`, `msgf`, `dtaq`, `bnddir`];

export async function noSourceTargetCallback(targets: Targets, localPath: string, content: string, options: FileOptions) {
  targets.resolvePathToObject(localPath, options.text);
}
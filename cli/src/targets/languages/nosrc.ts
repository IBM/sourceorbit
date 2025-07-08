import { Targets, FileOptions } from "..";
import { ExtensionMap } from "../languages";

export const noSourceObjects = [`dtaara`, `mnucmd`, `msgf`, `dtaq`, `bnddir`];
export const noSourceTargetObjects: ExtensionMap = {
  dtaara: `DTAARA`,
  mnucmd: `CMD`,
  msgf: `FILE`,
  dtaq: `DTAQ`,
  bnddir: `BNDDIR`
}

export async function noSourceTargetCallback(targets: Targets, localPath: string, content: string, options: FileOptions) {
  targets.resolvePathToObject(localPath, options.text);
}
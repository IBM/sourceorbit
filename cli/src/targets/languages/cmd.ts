import { FileOptions, ILEObject, Targets } from "..";
import { ExtensionMap } from "../languages";

export const cmdExtensions = [`cmd`];
export const cmdObjects: ExtensionMap = {
  cmd: `CMD`
}

export async function cmdTargetCallback(targets: Targets, localPath: string, content: string, ileObject: ILEObject) {
  // Do nothing!
}
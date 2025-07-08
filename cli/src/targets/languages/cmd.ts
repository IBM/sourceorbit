import path from "path";
import { CLParser, DefinitionType, Module, File } from "vscode-clle/language";
import { FileOptions, ILEObjectTarget, Targets } from "..";
import { infoOut } from "../../cli";
import { trimQuotes } from "../../utils";

export async function cmdTargetCallback(targets: Targets, localPath: string, content: string, options: FileOptions) {
  targets.resolvePathToObject(localPath, options.text);
}
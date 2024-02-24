import { existsSync, readFileSync } from "fs";
import path from "path";

export function getSourcePath(name: string) {
  return path.join(__dirname, name);
}

export function resolveInclude(name: string) {
  const fullPath = getSourcePath(name);
  if (existsSync(fullPath)) {
    return fullPath;
  } else {
    return undefined;
  }
}
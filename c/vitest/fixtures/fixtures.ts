import { existsSync, readFileSync } from "fs";
import path from "path";

export function getSourcePath(name: string) {
  return path.join(__dirname, name);
}

export function resolveInclude(name: string) {
  console.log(__dirname);
  
  const fullPath = getSourcePath(name);
  if (existsSync(fullPath)) {
    return fullPath;
  }

  throw new Error(`File not found: ${name}`);
}
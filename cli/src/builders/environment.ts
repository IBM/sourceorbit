import { str } from "crc-32/crc32c";

export function getBranchLibraryName(currentBranch: string) {
  return `VS${(str(currentBranch, 0)>>>0).toString(16).toUpperCase()}`;
}
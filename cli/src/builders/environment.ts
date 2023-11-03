
export function getBranchLibraryName(currentBranch: string) {
  const parts = branchSplit(currentBranch);
  if (parts.length > 1) {
    // We make A LOT of assumptions here about a valid branch name
    const branchType = parts[0].length > 3 ? parts[0].substring(0, 3) : parts[0];
    const possibleId = parts.find(p => p.length <= 7 && !isNaN(parseInt(p)));
    const backupId = parts[1].length > 7 ? parts[1].substring(0, 7) : parts[1];

    const actualId = possibleId || backupId;

    return (branchType + actualId).trim().toUpperCase();

  } else if (currentBranch.length > 10) {
    return currentBranch.substring(0, 10).toUpperCase();

  } else {
    return currentBranch.toUpperCase();
  }
}

function branchSplit(value: string) {
  let parts: string[] = [];
  let c = ``;

  for (const v of value) {
    if (v === `/` || v === `-` || v === ` `) {
      parts.push(c);
      c = ``;
    } else {
      c += v;
    }
  }

  parts.push(c);

  return parts;
}
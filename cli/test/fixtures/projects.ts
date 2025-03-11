import * as fs from "fs";
import * as path from "path";

const projectFolder = path.join(__dirname, `..`, `..`, `..`, `testData`);

export function getAllFixtures(): string[] {
  const dirs = fs.readdirSync(__dirname, {withFileTypes: true})
    .filter(dirent => dirent.isDirectory())
    .map(dirent => dirent.name);

  return dirs;
}

export function setupFixture(folderName: string): {cwd: string, setup: () => void, copy: () => void} {
  const fixturePath = path.join(__dirname, folderName);
  const projectPath = path.join(projectFolder, folderName);

  return {
    cwd: projectPath,
    setup: () => {
      // fs.cpSync(fixturePath, projectPath, {recursive: true});
    },
    copy: () => {
      deleteDir(projectPath);
      mkdir(projectPath);
      fs.cpSync(fixturePath, projectPath, {recursive: true});
    }
  };
}

export function createTestBuildScript() {
  const lines = [
    `# First build company system`,
    `system -qi "DLTLIB LIB(CMPSYSTST)"`,
    `cd company_system`,
    `gmake BIN_LIB=CMPSYSTST`,
    ``,
    `# Then build multi module`,
    `cd ../multi_module`,
    `system -qi "DLTLIB LIB(CMPSYSTST)"`,
    `gmake BIN_LIB=MMODTEST`,
    ``,
    `# Cleanup`,
    `system -qi "DLTLIB LIB(CMPSYSTST)"`,
    `system -qi "DLTLIB LIB(MMODTEST)"`
  ].join(`\n`);

  mkdir(projectFolder);
  const scriptPath = path.join(projectFolder, `build.sh`);
  fs.writeFileSync(scriptPath, lines);
}

function mkdir(dirPath: string) {
  try {
    fs.mkdirSync(dirPath, {recursive: true});
  } catch (e) {};
}

function deleteDir(dirPath: string) {
  try {
    fs.rmSync(dirPath, {recursive: true, force: true});
  } catch (e) {};
}
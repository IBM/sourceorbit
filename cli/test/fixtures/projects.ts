import * as fs from "fs";
import * as path from "path";

const projectFolder = path.join(__dirname, `..`, `..`, `..`, `testData`);

export function setupProjectFromQsys() {
  const fixturePath = path.join(__dirname, `from_qsys`);
  const projectPath = path.join(projectFolder, `from_qsys`);

  
  deleteDir(projectPath);
  mkdir(projectPath);
  fs.cpSync(fixturePath, projectPath, {recursive: true});

  return projectPath;
}

export function setupIncludeFix() {
  const fixturePath = path.join(__dirname, `include_fix`);
  const projectPath = path.join(projectFolder, `include_fix`);

  deleteDir(projectPath);
  mkdir(projectPath);
  fs.cpSync(fixturePath, projectPath, {recursive: true});

  return projectPath;
}

export function setupCompanySystem() {
  const fixturePath = path.join(__dirname, `company_system`);
  const projectPath = path.join(projectFolder, `company_system`);

  deleteDir(projectPath);
  mkdir(projectPath);
  fs.cpSync(fixturePath, projectPath, {recursive: true});

  return projectPath;
}

export function setupSqlReferencesSystem() {
  const fixturePath = path.join(__dirname, `sql_references`);
  const projectPath = path.join(projectFolder, `sql_references`);

  deleteDir(projectPath);
  mkdir(projectPath);
  fs.cpSync(fixturePath, projectPath, {recursive: true});

  return projectPath;
}

export function setupMultiModule() {
  const fixturePath = path.join(__dirname, `multi_module`);
  const projectPath = path.join(projectFolder, `multi_module`);

  deleteDir(projectPath);
  mkdir(projectPath);
  fs.cpSync(fixturePath, projectPath, {recursive: true});

  return projectPath;
}

export function setupPseudo() {
  const fixturePath = path.join(__dirname, `pseudo`);
  const projectPath = path.join(projectFolder, `pseudo`);

  deleteDir(projectPath);
  mkdir(projectPath);
  fs.cpSync(fixturePath, projectPath, {recursive: true});

  return projectPath;
}

export function setupFixture(folderName: string) {
  const fixturePath = path.join(__dirname, folderName);
  const projectPath = path.join(projectFolder, folderName);

  deleteDir(projectPath);
  mkdir(projectPath);
  fs.cpSync(fixturePath, projectPath, {recursive: true});

  return projectPath;
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
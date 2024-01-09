import * as fs from "fs";
import * as path from "path";

const projectFolder = path.join(__dirname, `..`, `..`, `..`, `projects`);

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

export function setupMultiModule() {
  const fixturePath = path.join(__dirname, `multi_module`);
  const projectPath = path.join(projectFolder, `multi_module`);

  deleteDir(projectPath);
  mkdir(projectPath);
  fs.cpSync(fixturePath, projectPath, {recursive: true});

  return projectPath;
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
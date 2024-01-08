import * as fs from "fs";
import * as path from "path";

export function setupProjectFromQsys() {
  const fixturePath = path.join(__dirname, `from_qsys`);
  const projectPath = path.join(__dirname, `..`, `..`, `..`, `from_qsys`);

  deleteDir(projectPath);
  fs.cpSync(fixturePath, projectPath, {recursive: true});

  return projectPath;
}

export function setupIncludeFix() {
  const fixturePath = path.join(__dirname, `include_fix`);
  const projectPath = path.join(__dirname, `..`, `..`, `..`, `include_fix`);

  deleteDir(projectPath);
  fs.cpSync(fixturePath, projectPath, {recursive: true});

  return projectPath;
}

export function setupCompanySystem() {
  const fixturePath = path.join(__dirname, `company_system`);
  const projectPath = path.join(__dirname, `..`, `..`, `..`, `company_system`);

  deleteDir(projectPath);
  fs.cpSync(fixturePath, projectPath, {recursive: true});

  return projectPath;
}

export function setupMultiModule() {
  const fixturePath = path.join(__dirname, `multi_module`);
  const projectPath = path.join(__dirname, `..`, `..`, `..`, `multi_module`);

  deleteDir(projectPath);
  fs.cpSync(fixturePath, projectPath, {recursive: true});

  return projectPath;
}

function deleteDir(dirPath: string) {
  try {
    fs.rmSync(dirPath, {recursive: true, force: true});
  } catch (e) {};
}
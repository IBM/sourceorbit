import { Extension, extensions } from "vscode";
import { IBMiProjectExplorer } from "@ibm/vscode-ibmi-projectexplorer-types/ibmiProjectExplorer";
import { ProjectManager } from "@ibm/vscode-ibmi-projectexplorer-types/projectManager";
import ProjectExplorer from "@ibm/vscode-ibmi-projectexplorer-types/views/projectExplorer";

let baseExtension: Extension<IBMiProjectExplorer> | undefined;

export function loadIBMiProjectExplorer(): IBMiProjectExplorer | undefined {
  if (!baseExtension) {
    baseExtension = (extensions ? extensions.getExtension<IBMiProjectExplorer>(`IBM.vscode-ibmi-projectexplorer`) : undefined);
  }

  return (baseExtension && baseExtension.isActive && baseExtension.exports ? baseExtension.exports : undefined);
}

/**
 * Get the access to the Project Manager APIs.
 */
export function getProjectManager(): typeof ProjectManager | undefined {
  return (baseExtension && baseExtension.isActive && baseExtension.exports ? baseExtension.exports.projectManager : undefined);
}

/**
 * Get the access to the Project Explorer APIs.
 */
export function getProjectExplorer(): ProjectExplorer | undefined {
  return (baseExtension && baseExtension.isActive && baseExtension.exports ? baseExtension.exports.projectExplorer : undefined);
}
import { ThemeIcon, TreeItem, TreeItemCollapsibleState, Uri, WorkspaceFolder } from "vscode";
import { ProjectExplorerTreeItem } from "@ibm/vscode-ibmi-projectexplorer-types/views/projectExplorer/projectExplorerTreeItem";

import { ILEObject } from "@ibm/source-orbit/dist/src/targets";
import { getDeps, getResolvedObjects } from '../requests';
import path = require('path');
import { TypeIcons } from './utils';


/**
 * Tree item for the Project Metadata heading.
 */
export class ObjectsView extends TreeItem implements ProjectExplorerTreeItem {

  constructor(public workspaceFolder: WorkspaceFolder) {
    super(`Objects`, TreeItemCollapsibleState.Collapsed);
    this.contextValue = `objectsView`;
  }

  async getChildren(): Promise<ProjectExplorerTreeItem[]> {
    const objects = await getResolvedObjects(this.workspaceFolder);

    return objects.map(o => new ILEObjectTreeItem(this.workspaceFolder, o, true));
      
    // if (viewMode === "impact") {
    //   if (impactsOf.length > 0) {
    //     const impacts = await getImpacts(this.workspaceFolder, impactsOf);

    //     return impacts.map(i => new ILEImpactedObject(this.workspaceFolder, i));
    //   } else {
    //     return [new Notice(this.workspaceFolder, `Open source code to see changes impact.`)];
    //   }
    // }
  }
}

/**
* Tree item for metadata information.
*/
export class ILEObjectTreeItem extends TreeItem implements ProjectExplorerTreeItem {
  constructor(public workspaceFolder: WorkspaceFolder, private ileObject: ILEObject, canExpand = false) {
    super(`${ileObject.name}.${ileObject.type}`, canExpand ? TreeItemCollapsibleState.Collapsed : TreeItemCollapsibleState.None);
    // const logs = TargetsManager.getLogs(workspaceFolder, ileObject);

    this.description = [ileObject.text, ileObject.extension ? `(${ileObject.extension})` : undefined].join(` `);
    this.iconPath = new ThemeIcon(TypeIcons[ileObject.type] || `unverified`);
    this.contextValue = `ileObject`;

    if (ileObject.relativePath) {
      this.resourceUri = Uri.from({scheme: `file`, path: path.join(this.workspaceFolder.uri.fsPath, ileObject.relativePath)});
    }
  }

  async getChildren(): Promise<(ILEObjectTreeItem|Notice)[]> {
    const deps = await getDeps(this.workspaceFolder, this.ileObject);
    if (deps.length > 0) {
      return deps.map(d => new ILEObjectTreeItem(this.workspaceFolder, d, false));

    } else {
      return [new Notice(this.workspaceFolder, `No depenedencies.`)];
    }
  }
}

export class Notice extends TreeItem implements ProjectExplorerTreeItem {
  constructor(public workspaceFolder: WorkspaceFolder, message: string) {
    super(message, TreeItemCollapsibleState.None);

    this.iconPath = new ThemeIcon(`unverified`);
  }

  async getChildren(): Promise<ILEObjectTreeItem[]> {
    return [];
  }
}

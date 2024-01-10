import { ThemeIcon, TreeItem, TreeItemCollapsibleState, Uri, WorkspaceFolder } from "vscode";
import { EventEmitter, Event } from "vscode";

import { ProjectExplorerTreeItem } from "@ibm/vscode-ibmi-projectexplorer-types/views/projectExplorer/projectExplorerTreeItem";

import { ILEObject } from "@ibm/sourceorbit/dist/src/targets";
import { getDeps, getResolvedObjects, isReady, reloadProject } from '../requests';
import path = require('path');
import { TypeIcons } from './utils';
import { enableViews } from '../extension';


/**
 * Tree item for the objects heading.
 */
export class ObjectsView extends TreeItem implements ProjectExplorerTreeItem {
	private _onDidChangeTreeData: EventEmitter<TreeItem | undefined | null | void> = new EventEmitter<TreeItem | undefined | null | void>();
	readonly onDidChangeTreeData: Event<TreeItem | undefined | null | void> = this._onDidChangeTreeData.event;

  constructor(public workspaceFolder: WorkspaceFolder) {
    super(`Source Orbit`, TreeItemCollapsibleState.Collapsed);
    this.iconPath = new ThemeIcon(`globe`);
    this.contextValue = `objectsView`;
  }

	refresh() {
		this._onDidChangeTreeData.fire();
	}

  async getChildren(): Promise<ProjectExplorerTreeItem[]> {
    const isProjectReady = await isReady(this.workspaceFolder);
    if (!isProjectReady) {
      await reloadProject(this.workspaceFolder);
      await enableViews();
    }
    
    const objects = await getResolvedObjects(this.workspaceFolder);

    return objects.map(o => new ILEObjectTreeItem(this.workspaceFolder, o, true));
  }
}

/**
* Tree item for an ILE object.
*/
export class ILEObjectTreeItem extends TreeItem implements ProjectExplorerTreeItem {
  constructor(public workspaceFolder: WorkspaceFolder, private ileObject: ILEObject, canExpand = false) {
    super(`${ileObject.systemName}.${ileObject.type}`, canExpand ? TreeItemCollapsibleState.Collapsed : TreeItemCollapsibleState.None);
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
      return [new Notice(this.workspaceFolder, `No dependencies.`)];
    }
  }
}

export class Notice extends TreeItem implements ProjectExplorerTreeItem {
  constructor(public workspaceFolder: WorkspaceFolder, message: string) {
    super(message, TreeItemCollapsibleState.None);

    this.iconPath = new ThemeIcon(`unverified`);
  }

  setCommand(command: string, arg?: string) {
    this.command = {command: command, title: ``, arguments: [this.workspaceFolder, arg]};
  }

  async getChildren(): Promise<ILEObjectTreeItem[]> {
    return [];
  }
}

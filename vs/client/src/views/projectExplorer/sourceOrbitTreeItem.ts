import { ProjectExplorerTreeItem } from "@ibm/vscode-ibmi-projectexplorer-types/views/projectExplorer/projectExplorerTreeItem";
import { Event, EventEmitter, ThemeIcon, TreeItem, TreeItemCollapsibleState, WorkspaceFolder } from "vscode";
import { LanguageClientManager } from '../../languageClientManager';
import { ILEObjectTreeItem } from './ileObjectTreeItem';

export class SourceOrbitTreeItem extends TreeItem implements ProjectExplorerTreeItem {
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
    const isProjectReady = await LanguageClientManager.isReady(this.workspaceFolder);
    if (!isProjectReady) {
      await LanguageClientManager.reloadProject(this.workspaceFolder);
    }

    const objects = await LanguageClientManager.getResolvedObjects(this.workspaceFolder);
    return objects.map(o => new ILEObjectTreeItem(this.workspaceFolder, o, true));
  }
}
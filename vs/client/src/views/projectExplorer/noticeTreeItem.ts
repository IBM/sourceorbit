import { ProjectExplorerTreeItem } from "@ibm/vscode-ibmi-projectexplorer-types/views/projectExplorer/projectExplorerTreeItem";
import { ThemeIcon, TreeItem, TreeItemCollapsibleState, WorkspaceFolder } from "vscode";
import { ILEObjectTreeItem } from './ileObjectTreeItem';

export class NoticeTreeItem extends TreeItem implements ProjectExplorerTreeItem {
	constructor(public workspaceFolder: WorkspaceFolder, message: string) {
		super(message, TreeItemCollapsibleState.None);
		this.iconPath = new ThemeIcon(`unverified`);
	}

	setCommand(command: string, arg?: string) {
		this.command = { command: command, title: ``, arguments: [this.workspaceFolder, arg] };
	}

	async getChildren(): Promise<ILEObjectTreeItem[]> {
		return [];
	}
}
import { ILEObject } from "@ibm/sourceorbit/dist/src/targets";
import { ProjectExplorerTreeItem } from "@ibm/vscode-ibmi-projectexplorer-types/views/projectExplorer/projectExplorerTreeItem";
import { ThemeIcon, TreeItem, TreeItemCollapsibleState, Uri, WorkspaceFolder } from "vscode";
import { LanguageClientManager } from '../../languageClientManager';
import { TypeIcons } from '../utils';
import { NoticeTreeItem } from './noticeTreeItem';
import path = require('path');

export class ILEObjectTreeItem extends TreeItem implements ProjectExplorerTreeItem {
	constructor(public workspaceFolder: WorkspaceFolder, private ileObject: ILEObject, canExpand = false) {
		super(`${ileObject.systemName}.${ileObject.type}`, canExpand ? TreeItemCollapsibleState.Collapsed : TreeItemCollapsibleState.None);
		// const logs = TargetsManager.getLogs(workspaceFolder, ileObject);

		this.description = [ileObject.text, ileObject.extension ? `(${ileObject.extension})` : undefined].join(` `);
		this.iconPath = new ThemeIcon(TypeIcons[ileObject.type] || `unverified`);
		this.contextValue = `ileObject`;

		if (ileObject.relativePath) {
			this.resourceUri = Uri.from({ scheme: `file`, path: path.join(this.workspaceFolder.uri.fsPath, ileObject.relativePath) });
		}
	}

	async getChildren(): Promise<(ILEObjectTreeItem | NoticeTreeItem)[]> {
		const deps = await LanguageClientManager.getDeps(this.workspaceFolder, this.ileObject);
		if (deps.length > 0) {
			return deps.map(d => new ILEObjectTreeItem(this.workspaceFolder, d, false));
		} else {
			return [new NoticeTreeItem(this.workspaceFolder, `No dependencies`)];
		}
	}
}
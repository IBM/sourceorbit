import { ImpactedObject } from '@ibm/sourceorbit/dist/src/targets';
import { ThemeIcon, TreeItem, TreeItemCollapsibleState, Uri, WorkspaceFolder } from "vscode";
import { TypeIcons } from '../utils';
import path = require('path');

export class ILEImpactedObjectTreeItem extends TreeItem {
	constructor(public workspaceFolder: WorkspaceFolder, private impactedObject: ImpactedObject) {
		super(`${impactedObject.ileObject.systemName}.${impactedObject.ileObject.type}`, impactedObject.children.length > 0 ? TreeItemCollapsibleState.Expanded : TreeItemCollapsibleState.None);
		// const logs = TargetsManager.getLogs(workspaceFolder, ileObject);

		this.description = impactedObject.ileObject.relativePath || `No source`;
		this.iconPath = new ThemeIcon(TypeIcons[impactedObject.ileObject.type] || `unverified`);

		this.contextValue = `ileObject`;

		if (impactedObject.ileObject.relativePath) {
			this.resourceUri = Uri.from({ scheme: `file`, path: path.join(this.workspaceFolder.uri.fsPath, impactedObject.ileObject.relativePath) });
		}
	}

	async getChildren(): Promise<(ILEImpactedObjectTreeItem)[]> {
		return this.impactedObject.children.map(c => new ILEImpactedObjectTreeItem(this.workspaceFolder, c));
	}
}
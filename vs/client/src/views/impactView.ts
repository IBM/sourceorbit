import { ImpactedObject } from '@ibm/source-orbit/dist/src/targets';
import { EventEmitter, Uri, WorkspaceFolder, workspace } from "vscode";
import { Event, ThemeIcon, TreeDataProvider, TreeItem, TreeItemCollapsibleState } from "vscode";
import { TypeIcons } from './utils';
import { getImpacts } from '../requests';
import path = require('path');

export class ImpactView implements TreeDataProvider<any> {
	private _onDidChangeTreeData: EventEmitter<TreeItem | undefined | null | void> = new EventEmitter<TreeItem | undefined | null | void>();
	readonly onDidChangeTreeData: Event<TreeItem | undefined | null | void> = this._onDidChangeTreeData.event;

	private impactOf: Uri[] = [];

	refresh() {
		this._onDidChangeTreeData.fire();
	}

	getTreeItem(element: any): TreeItem | Thenable<TreeItem> {
		return element;
	}

	showImpactFor(uris: Uri[]) {
		this.impactOf = uris;
		this.refresh();
	}

	async getChildren(element?: ILEImpactedObject): Promise<(ILEImpactedObject|TreeItem)[]> {
		if (element && element.getChildren) {
			return element.getChildren();
		}
		else if (this.impactOf && this.impactOf.length > 0) {
			const workspaceFolder = workspace.getWorkspaceFolder(this.impactOf[0]);

			if (workspaceFolder) {
				const impacts = await getImpacts(workspaceFolder, this.impactOf);

				return impacts.map(i => new ILEImpactedObject(workspaceFolder, i));
			}
		} else {
			return [new TreeItem(`Open source code to see changes impact.`)];
		}


		return [];
	}
}

export class ILEImpactedObject extends TreeItem {
	constructor(public workspaceFolder: WorkspaceFolder, private impactedObject: ImpactedObject) {
		super(`${impactedObject.ileObject.name}.${impactedObject.ileObject.type}`, impactedObject.children.length > 0 ? TreeItemCollapsibleState.Expanded : TreeItemCollapsibleState.None);
		// const logs = TargetsManager.getLogs(workspaceFolder, ileObject);

		this.description = impactedObject.ileObject.relativePath || `No source`;
		this.iconPath = new ThemeIcon(TypeIcons[impactedObject.ileObject.type] || `unverified`);

    this.contextValue = `ileObject`;

    if (impactedObject.ileObject.relativePath) {
      this.resourceUri = Uri.from({scheme: `file`, path: path.join(this.workspaceFolder.uri.fsPath, impactedObject.ileObject.relativePath)});
    }
	}

	async getChildren(): Promise<(ILEImpactedObject)[]> {
		return this.impactedObject.children.map(c => new ILEImpactedObject(this.workspaceFolder, c));
	}
}
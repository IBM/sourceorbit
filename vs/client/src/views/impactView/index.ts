import { Event, EventEmitter, TreeDataProvider, TreeItem, Uri, workspace } from "vscode";
import { LanguageClientManager } from '../../languageClientManager';
import { ILEImpactedObjectTreeItem } from './ileImpactedObjectTreeItem';

export class ImpactView implements TreeDataProvider<any> {
	private _onDidChangeTreeData: EventEmitter<TreeItem | undefined | null | void> = new EventEmitter<TreeItem | undefined | null | void>();
	readonly onDidChangeTreeData: Event<TreeItem | undefined | null | void> = this._onDidChangeTreeData.event;
	public impactOf: Uri[] = [];

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

	async getChildren(element?: ILEImpactedObjectTreeItem): Promise<(ILEImpactedObjectTreeItem | TreeItem)[]> {
		if (element && element.getChildren) {
			return element.getChildren();
		} else if (this.impactOf && this.impactOf.length > 0) {
			const workspaceFolder = workspace.getWorkspaceFolder(this.impactOf[0]);

			if (workspaceFolder) {
				const isProjectReady = await LanguageClientManager.isReady(workspaceFolder);
				if (!isProjectReady) {
					await LanguageClientManager.reloadProject(workspaceFolder);
				}

				const impacts = await LanguageClientManager.getImpacts(workspaceFolder, this.impactOf);
				if (impacts.length > 0) {
					return impacts.map(i => new ILEImpactedObjectTreeItem(workspaceFolder, i));
				} else {
					return [new TreeItem(`No impacted objects`)];
				}
			}
		}

		return [];
	}
}
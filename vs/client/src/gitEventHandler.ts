
import { ExtensionContext, WorkspaceFolder } from 'vscode';
import { getDeployGitFiles as getChangedFiles, getGitAPI, lastBranch } from './git';
import { LanguageClientManager } from './languageClientManager';
import { ImpactView } from './views/impactView/impactView';

export namespace GitEventHandler {
	export function setup(context: ExtensionContext, gitImpactView: ImpactView, workspaceFolders: WorkspaceFolder[]) {
		const gitApi = getGitAPI();

		if (gitApi) {
			for (const workspaceFolder of workspaceFolders) {
				const repo = gitApi.getRepository(workspaceFolder.uri);
				if (repo) {
					const workspaceUri = workspaceFolder.uri.toString();
					const head = repo.state.HEAD;
					if (head && head.name) {
						lastBranch[workspaceUri] = head.name;

						context.subscriptions.push(repo.state.onDidChange((_e) => {
							const currentBranch = head.name;
							if (currentBranch) {
								if (lastBranch[workspaceUri] && currentBranch !== lastBranch[workspaceUri]) {
									gitImpactView.showImpactFor([]);
									LanguageClientManager.reloadProject(workspaceFolder);

								} else {
									getChangedFiles(workspaceFolder).then(files => {
										gitImpactView.showImpactFor(files);
									});

								}

								lastBranch[workspaceUri] = currentBranch;
							}
						}));
					}
				}
			}
		}
	}
}
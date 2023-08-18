import { Uri, WorkspaceFolder, extensions } from 'vscode';
import { API, Change, GitExtension, Status } from './git';

let gitLookedUp: boolean;
let gitAPI: API | undefined;

export const lastBranch: {[workspaceUri: string]: string} = {};

export function getGitAPI(): API | undefined {
	if (!gitLookedUp) {
		try {
			gitAPI = extensions.getExtension<GitExtension>(`vscode.git`)?.exports.getAPI(1);
		}
		catch (error) {
			console.log(`Git extension issue.`, error);
		}
		finally {
			gitLookedUp = true;
		}
	}
	return gitAPI;
}

export async function getDeployGitFiles(workspaceFolder: WorkspaceFolder, changeType: 'staged' | 'working' | 'both' = 'both'): Promise<Uri[]> {
	const gitApi = getGitAPI();

	if (gitApi && gitApi.repositories.length > 0) {
		const repository = gitApi.repositories.find(r => r.rootUri.fsPath === workspaceFolder.uri.fsPath);

		if (repository) {
			const staged = repository.state.indexChanges;
			const working = repository.state.workingTreeChanges;

			let gitFiles: Change[];

			switch (changeType) {
				case `both`:
					gitFiles = [...staged, ...working];
					break;
				case `staged`:
					gitFiles = staged;
					break;
				case `working`:
					gitFiles = working;
					break;
			}

			// Do not attempt to upload deleted files.
			// https://github.com/microsoft/vscode/blob/main/extensions/git/src/api/git.d.ts#L69
			gitFiles = gitFiles.filter(change => change.status !== Status.DELETED);

			if (gitFiles.length > 0) {
				const uris = gitFiles.map(change => change.uri);
				const unique = uris.filter((value, index, array) => array.findIndex(u => u.toString() === value.toString()) === index);
				return unique;
			} else {
				return [];
			}
		} else {
			throw new Error(`No repository found for ${workspaceFolder.uri.fsPath}`);
		}
	} else {
		throw new Error(`No repositories are open.`);
	}
}
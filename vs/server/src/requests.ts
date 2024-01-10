import { Connection } from 'vscode-languageserver';
import { TargetsManager } from './TargetsManager';

import { ILEObject, TargetSuggestions, Targets } from "@ibm/sourceorbit/dist/src/targets";
import { URI } from 'vscode-uri';
import { fixProject, initAndRefresh } from './setup';

export function setupRequestHandler(connection: Connection) {
	connection.onRequest(`isReady`, async (params: [string]) => {
		return TargetsManager.isReady(params[0]);
	});

	connection.onRequest(`getResolvedObjects`, async (params: [string]) => {
		return TargetsManager.getResolvedObjects(params[0]);
	});

	connection.onRequest(`getDeps`, async (params: [string, ILEObject]) => {
		return TargetsManager.getDepsForTarget(params[0], params[1]);
	});

	connection.onRequest(`getImpacts`, async (params: [string, string[]]) => {
		const target = TargetsManager.getTargetsForWorkspaceUri(params[0]);

		if (target) {
			const uris = params[1];

			const possibleObjects = uris.map(fileUri => target.getResolvedObject(URI.parse(fileUri).fsPath)).filter(x => x && x.relativePath);

			return possibleObjects.map(ileObject => target.getImpactFor(ileObject));
		}

		return [];
	});

	connection.onRequest(`reloadProject`, (params: [string]) => {
		return initAndRefresh(params[0]);
	});

	connection.onRequest(`fixProject`, async (params: [string, keyof TargetSuggestions]) => {
		const suggestions: TargetSuggestions = {};

		suggestions[params[1]] = true;

		return fixProject(params[0], suggestions);
	});
}
import { Connection } from 'vscode-languageserver';
import { TargetsManager } from './TargetsManager';

import { ILEObject, TargetSuggestions, Targets } from "@ibm/sourceorbit/dist/src/targets";
import { URI } from 'vscode-uri';
import { fixProject, initAndRefresh } from './setup';
import { reResolve } from './ui';

export function setupRequestHandler(connection: Connection) {
	connection.onRequest(`isReady`, async (params: [string]) => {
		return TargetsManager.isReady(params[0]);
	});

	connection.onRequest(`getResolvedObjects`, async (params: [string]) => {
		return TargetsManager.getResolvedObjects(params[0]);
	});

	connection.onRequest(`getDeps`, async (params: [string, ILEObject]) => {
		return TargetsManager.getDepsFor(params[0], params[1]);
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

	connection.onRequest(`reloadProject`, async (params: [string]) => {
		initAndRefresh(params[0], true);
	});

	connection.onRequest(`resolveProject`, async (params: [string]) => {
		reResolve(params);
	});

	connection.onRequest(`fixProject`, async (params: [string, keyof TargetSuggestions]) => {
		const suggestions: TargetSuggestions = {};

		suggestions[params[1]] = true;

		return fixProject(params[0], suggestions);
	});
}
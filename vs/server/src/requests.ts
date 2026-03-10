import { Connection } from 'vscode-languageserver';
import { TargetsManager } from './TargetsManager';

import { ILEObject, TargetSuggestions } from "@ibm/sourceorbit/dist/src/targets";
import { URI } from 'vscode-uri';
import { fixProject, generateBuildFile, initAndRefresh } from './setup';

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

	connection.onRequest(`getImpactsToUris`, async (params: [string, string[]]) => {
		const target = TargetsManager.getTargetsForWorkspaceUri(params[0]);

		if (target) {
			const uris = params[1];

			const possibleObjects = uris
				.map(fileUri => target.getResolvedObject(URI.parse(fileUri).fsPath.replace(/\\/g, '/')))
				.filter(x => x && x.relativePath);

			return possibleObjects.map(ileObject => target.getImpactFor(ileObject));
		}

		return [];
	});

	connection.onRequest(`getImpactsToObjects`, async (params: [string, ILEObject[]]) => {
		const target = TargetsManager.getTargetsForWorkspaceUri(params[0]);

		if (target) {
			const ileObjects = params[1];

			return ileObjects.map(ileObject => target.getImpactFor(ileObject));
		}

		return [];
	});

	connection.onRequest(`reloadProject`, (params: [string]) => {
		return initAndRefresh(params[0]);
	});

	connection.onRequest(`resolvePathToObject`, async (params: [string, string]) => {
		const target = TargetsManager.getTargetsForWorkspaceUri(params[0]);

		if (target) {
			return await target.resolvePathToObject(params[1]);
		}

		return undefined;
	});

	connection.onRequest(`getExports`, async (params: [string]) => {
		const target = TargetsManager.getTargetsForWorkspaceUri(params[0]);

		if (target) {
			return target.getExports();
		}

		return {};
	});

	connection.onRequest(`fixProject`, (params: [string, keyof TargetSuggestions]) => {
		const suggestions: TargetSuggestions = {};

		suggestions[params[1]] = true;

		return fixProject(params[0], suggestions);
	});

	connection.onRequest(`generateBuildFile`, (params: [string, string]) => {
		return generateBuildFile(params[0], params[1]);
	});
}
import { Uri, WorkspaceFolder } from 'vscode';
import { LanguageClient } from 'vscode-languageclient/node';
import { ILEObject, ImpactedObject, TargetSuggestions } from "@halcyontech/source-orbit/dist/src/targets";

let client: LanguageClient;

export function setClient(lc: LanguageClient) {
	client = lc;
}

export async function isReady(workspaceFolder: WorkspaceFolder): Promise<boolean> {
	if (!client) return false;

	const result = await client.sendRequest(`isReady`, [workspaceFolder.uri]);
	return (result as boolean);
}

export async function getResolvedObjects(workspaceFolder: WorkspaceFolder): Promise<ILEObject[]> {
	if (!client) return [];
	
	const result = await client.sendRequest(`getResolvedObjects`, [workspaceFolder.uri.toString()]);
	return (result as ILEObject[]);
}

export async function getDeps(workspaceFolder: WorkspaceFolder, ileObject: ILEObject): Promise<ILEObject[]> {
	if (!client) return [];
	
	const result = await client.sendRequest(`getDeps`, [workspaceFolder.uri.toString(), ileObject]);
	return (result as ILEObject[]);
}

export async function getImpacts(workspaceFolder: WorkspaceFolder, fileUris: Uri[]): Promise<ImpactedObject[]> {
	if (!client) return [];
	
	const result = await client.sendRequest(`getImpacts`, [workspaceFolder.uri.toString(), fileUris.map(uri => uri.toString())]);
	return (result as ImpactedObject[]);
}

export function reloadProject(workspaceFolder: WorkspaceFolder){
	if (!client) return;
	
	return client.sendRequest(`reloadProject`, [workspaceFolder.uri.toString()]);
}

export function resolveProject(workspaceFolder: WorkspaceFolder){
	if (!client) return;
	
	return client.sendRequest(`resolveProject`, [workspaceFolder.uri.toString()]);
}

export function fixProject(workspaceFolder: WorkspaceFolder, suggestion: string) {
	if (!client) return;
	
	return client.sendRequest(`fixProject`, [workspaceFolder.uri.toString(), suggestion]);
}
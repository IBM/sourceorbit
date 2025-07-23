
export type BuildFiles = "none" | "make" | "bob" | "imd" | "json";

export let cliSettings = {
	cliMode: false,
	infoMessages: false,
	buildFile: "none" as BuildFiles,
	withActions: false,
	fixIncludes: false,
	autoRename: false,
	lookupFiles: undefined as string[]|undefined,
	userBranch: ``,
	makefileIsPartial: false,
	makefileWithParents: false,
	assumeSourcesArePrograms: false,
};

export function infoOut(message: string) {
	if (cliSettings.cliMode && cliSettings.infoMessages) console.log(message);
}

export function warningOut(message: string) {
	if (cliSettings.cliMode) console.log(message);
}

export function error(message: string) {
	console.log(`[ERROR] ${message}`);
}
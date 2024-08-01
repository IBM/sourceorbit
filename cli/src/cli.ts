
export type BuildFiles = "none" | "make" | "bob" | "imd" | "json";

export let cliSettings = {
	cliMode: false,
	infoMessages: false,
	buildFile: "none" as BuildFiles,
	fixIncludes: false,
	autoRename: false,
	fileList: false,
	lookupFiles: [] as string[],
	userBranch: ``,
	makeFileNoChildren: false,
	watchMode: false
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
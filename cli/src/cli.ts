
export type BuildFiles = "none" | "make" | "bob" | "imd" | "json";

export let cliSettings = {
	cliMode: false,
	infoMessages: false,
	buildFile: "none" as BuildFiles,
	fixIncludes: false,
	autoRename: false,

	fileList: false,
	lookupMode: false,
	lookupFiles: [] as string[]
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
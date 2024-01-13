import glob from "glob";
import { Logger } from "./logger";
import { infoOut, warningOut } from "./cli";

import * as fs from "fs";
import * as path from "path";
import * as os from "os"

export function getSystemNameFromPath(inputName: string) {

	let baseName = inputName.includes(`-`) ? inputName.split(`-`)[0] : inputName;

	// If the name is of valid length, return it
	if (baseName.length <= 10) {
		return baseName.toUpperCase();
	}

	// We also support prefixes to the name, such as UA_
	let prefix = ``;
	let name = baseName;

	if (baseName.includes(`_`)) {
		const parts = baseName.split(`_`);
		prefix = parts[0];
		name = parts[1];
	}

	// We start the system name with the suppliedPrefix
	let systemName = prefix;

	for (let i = 0; i < name.length && systemName.length <= 10; i++) {
		const char = name[i];
		if (char === char.toUpperCase() || i === 0) {
			systemName += char;
		}
	}

	// If we only have one character, then no capitals were used in the name. Let's just use the first 10 characters
	if (systemName.length === 1) {
		systemName = name.substring(0, 10);
	}

	return systemName.toUpperCase();
}

export function getFiles(cwd: string, globPath: string): string[] {
	let paths: string[] = glob.sync(globPath, {
		cwd,
		absolute: true,
		nocase: true,
	});

	if (os.platform() === `win32`) {
		paths = paths.map(p => p.split(path.posix.sep).join(path.sep))
	}

	return paths;
}

export function asPosix(inPath?: string) {
	return inPath ? inPath.split(path.sep).join(path.posix.sep) : ``;
}

export function toLocalPath(inPath: string) {
	if (os.platform() === `win32`) return inPath.split(path.posix.sep).join(path.sep);
	else return inPath.split(path.win32.sep).join(path.sep);
}

export function replaceIncludes(logger: Logger) {
	warningOut(`Starting include fix process. Do not end process.`);
	const allLogs = logger.getAllLogs();

	for (const filePath in allLogs) {
		const content = fs.readFileSync(filePath, { encoding: `utf8` });
		const eol = content.includes(`\r\n`) ? `\r\n` : `\n`;
		const lines = content.split(eol);

		const logs = allLogs[filePath].filter(l => l.type === `includeFix` && l.line);

		if (logs.length > 0) {
			for (const log of logs) {
				if (log.change && log.change.lineContent) {
					lines[log.line] = log.change.lineContent;
				}
			}

			infoOut(`${filePath} changes: ${logs.length}`);
			fs.writeFileSync(filePath, lines.join(eol));
		}
	}
}

export function renameFiles(logger: Logger) {
	warningOut(`Starting rename process. Do not end process.`);
	const allLogs = logger.getAllLogs();

	let validRenames: {[path: string]: string} = {};

	for (const filePath in allLogs) {
		const logs = allLogs[filePath].filter(l => l.type === `rename`);

		for (const log of logs) {
			if (log.change && log.change.rename) {
				const detail = log.change.rename;
				validRenames[detail.path] = detail.newName;
			}
		}
	}


	for (const ogPath in validRenames) {
		fs.renameSync(ogPath, path.join(path.dirname(ogPath), validRenames[ogPath]));
		infoOut(`'${ogPath}' -> '${validRenames[ogPath]}'`);
	}
}
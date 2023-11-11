import glob from "glob";
import { Logger } from "./logger";
import { infoOut, warningOut } from "./cli";

import * as fs from "fs";
import * as path from "path";
import * as os from "os"

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
import glob from "glob";
import { Logger } from "./logger";
import { infoOut, warningOut } from "./cli";

import * as fs from "fs";
import * as path from "path";
import * as os from "os"
import { ILEObject, ObjectType } from "./targets";
import { CommandParameters } from "./builders/environment";
import { ReadFileSystem } from "./readFileSystem";

export function getSystemNameFromPath(inputName: string) {
	const isTest = inputName.toUpperCase().endsWith(`.TEST`);
	if (isTest) {
		// Remove the .TEST part
		inputName = inputName.substring(0, inputName.length - 5);
	}

	const baseName = inputName.includes(`-`) ? inputName.split(`-`)[0] : inputName;

	// Test -> If the name with test prefix T is of valid length, return it
	if (isTest && `T${baseName}`.length <= 10) {
		return `T${baseName}`.toUpperCase();
	}

	// Non-test -> If the name is of valid length, return it
	if (!isTest && baseName.length <= 10) {
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

	for (let i = 0; i < name.length && systemName.length < 10; i++) {
		const char = name[i];
		if (char === char.toUpperCase() || i === 0) {
			systemName += char;
		}
	}

	// If we only have one character, then no capitals were used in the name. Let's just use the first 10 characters
	if (systemName.length === 1) {
		systemName = name.substring(0, 10);
	}

	// If it is a test, we prefix it with T
	if (isTest) {
		systemName = `T${systemName}`;
	}

	// System name could exceed 10 characters (ie. if prefix is long or because of T prefix) so substring one last time
	return systemName.substring(0, 10).toUpperCase();
}

/**
 * @deprecated Use {@link ReadFileSystem} instead
 */
export function getFiles(cwd: string, globPath: string, additionalOpts: any = {}): string[] {
	let paths: string[] = glob.sync(globPath, {
		cwd,
		absolute: true,
		nocase: true,
		...additionalOpts
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

	let validRenames: { [path: string]: string } = {};

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

export function getReferenceObjectsFrom(content: string) {
	const pseudoObjects: ILEObject[] = [];

	const newLine = content.includes(`\r\n`) ? `\r\n` : `\n`;

	const lines = content.split(newLine);

	let currentObject: ILEObject;

	for (let line of lines) {
		const upperLine = line.trim().toUpperCase();
		if (upperLine.length === 0) continue;
		if (upperLine.startsWith(`#`)) continue;

		// If the line starts with space, then it's an export of the parent
		if (line.startsWith(` `) || line.startsWith(`\t`)) {
			if (currentObject) {
				currentObject.exports.push(upperLine);
			}

		} else {
			const objectParts = upperLine.split(`.`);

			if (objectParts[0].length > 10) {
				warningOut(`Trying to add referenced object: object name '${objectParts[0]}' is too long.`);
			}

			currentObject = {
				systemName: objectParts[0],
				type: objectParts[1] as ObjectType, //TODO: validate type
				exports: [],
				reference: true,
			}

			pseudoObjects.push(currentObject);
		}
	}

	return pseudoObjects;
}

export function fromCl(cl: string): { command: string, parameters: CommandParameters } {
	let gotCommandnName = false;
	let parmDepth = 0;

	let currentCommand = ``;
	let currentParmName = ``;
	let currentParmValue = ``;
	let parameters: CommandParameters = {};

	for (const c of cl.split(``)) {
		if (c === `(`) {
			parmDepth++;
			if (parmDepth > 1) {
				// If we are already in a parameter, we just add the character to the value
				currentParmValue += c;
			}
		} else if (c === `)`) {
			if (parmDepth === 1) {
				parameters[currentParmName.toLowerCase()] = currentParmValue;
				currentParmValue = ``;
				currentParmName = ``;
			} else {
				// If we are deeper than 1, we just add the character to the value
				currentParmValue += c;
			}
			parmDepth--;
		} else if (c === ` ` && !gotCommandnName) {
			gotCommandnName = true;
			currentCommand = currentCommand.trim();
		} else if (c === ` ` && gotCommandnName) {
			currentParmName = currentParmName.trim();
		} else if (parmDepth > 0) {
			currentParmValue += c;
		} else if (gotCommandnName) {
			currentParmName += c;
		} else {
			currentCommand += c;
		}
	}

	return {
		command: currentCommand,
		parameters
	}
}

/**
 * 
 * @param command Optionally qualified CL command
 * @param parameters A key/value object of parameters
 * @returns Formatted CL string
 */
export function toCl(command: string, parameters?: CommandParameters) {
	let cl = command;

	if (parameters) {
		for (const [key, value] of Object.entries(parameters)) {
			let parmValue;

			if (value) {
				if (value.startsWith(`'`) && value.endsWith(`'`)) {
					parmValue = value;
				}
				else if (value.trim() !== ``) {
					if (value === value.toLocaleUpperCase()) {
						parmValue = value;
					} else {
						parmValue = value.replace(/'/g, `''`);
						parmValue = `'${parmValue}'`;
					}
				}

				cl += ` ${key.toUpperCase()}(${parmValue})`;
			}
		}
	}

	return cl;
}

export function checkFileExists(file) {
	return fs.promises.access(file, fs.constants.F_OK)
		.then(() => true)
		.catch(() => false)
}
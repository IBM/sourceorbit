
import { readFileSync } from 'fs';

import Parser from "vscode-rpgle/language/parser";
import { Targets } from './targets';

let includeFileCache: { [path: string]: string } = {};

export function setupParser(targets: Targets): Parser {
	const parser = new Parser();

	parser.setIncludeFileFetch(async (baseFile: string, includeFile: string) => {
		if (includeFile.startsWith(`'`) && includeFile.endsWith(`'`)) {
			includeFile = includeFile.substring(1, includeFile.length - 1);
		}

		if (includeFile.includes(`,`)) {
			includeFile = includeFile.split(`,`)[1] + `.*`;
		}

		const file = targets.searchForObject(includeFile);

		if (file) {
			if (includeFileCache[file]) {
				return {
					found: true,
					uri: file,
					lines: includeFileCache[file].split(`\n`)
				}

			} else {
				const content = readFileSync(file, { encoding: `utf-8` });
				includeFileCache[file] = content;

				return {
					found: true,
					uri: file,
					lines: content.split(`\n`)
				}
			}
		}

		return {
			found: false
		};
	});

	parser.setTableFetch(async (table: string, aliases = false) => {
		// Can't support tables in CLI mode I suppose?
		return [];
	});

	return parser;
}
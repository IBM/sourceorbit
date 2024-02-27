
import { readFileSync } from 'fs';
import * as path from 'path';

import Parser from "vscode-rpgle/language/parser";
import { Targets } from './targets';
import { CParser } from 'ileclang/src/parser';

let includeFileCache: { [path: string]: string } = {};

export function setupRpgParser(targets: Targets): Parser {
	const parser = new Parser();

	parser.setIncludeFileFetch(async (baseFile: string, includeFile: string) => {
		if (includeFile.startsWith(`'`) && includeFile.endsWith(`'`)) {
			includeFile = includeFile.substring(1, includeFile.length - 1);
		}

		if (includeFile.includes(`,`)) {
			// If the member include path is qualified with a source file
			// then we should convert to be a unix style path so we can
			// search the explicit directories.
			includeFile = includeFile.replace(/,/g, `/`) + `.*`;
		} else if (!includeFile.includes(`/`)) {
			const parent = path.basename(path.dirname(baseFile));
			includeFile = `${parent}/${includeFile}`;
		}

		const file = targets.resolveLocalFile(includeFile);

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

export function setupCParser(targets: Targets) {
	const parser = new CParser();
	parser.enableCache();
	parser.setIncludeResolver((includeRef: string) => {
		return targets.resolveLocalFile(includeRef);
	});
	return parser;
}
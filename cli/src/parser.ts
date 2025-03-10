
import { readFileSync } from 'fs';
import * as path from 'path';

import Parser from "vscode-rpgle/language/parser";
import { Targets } from './targets';

let includeFileCache: { [path: string]: string } = {};

export function setupParser(targets: Targets): Parser {
	const parser = new Parser();

	parser.setIncludeFileFetch(async (baseFile: string, includeFile: string) => {
		if (includeFile.startsWith(`'`) && includeFile.endsWith(`'`)) {
			includeFile = includeFile.substring(1, includeFile.length - 1);
		}

		let file: string;

		if (includeFile.includes(`,`)) {
			// If the member include path is qualified with a source file
			// then we should convert to be a unix style path so we can
			// search the explicit directories.
			includeFile = includeFile.replace(/,/g, `/`) + `.*`;

			// Keep making the path less specific until we find a possible include
			let parts = includeFile.split(`/`);
			while (!file && parts.length > 0) {
				file = targets.resolveLocalFile(includeFile);

				if (!file) {
					parts.shift();
					includeFile = parts.join(`/`);
				}
			}
		} else if (!includeFile.includes(`/`)) {
			const parent = path.basename(path.dirname(baseFile));
			console.log(parent);
			includeFile = `${parent}/${includeFile}`;


			file = targets.resolveLocalFile(includeFile);
		} else {
			file = targets.resolveLocalFile(includeFile);
		}

		if (file) {
			if (includeFileCache[file]) {
				return {
					found: true,
					uri: file,
					content: includeFileCache[file]
				}

			} else {
				const content = readFileSync(file, { encoding: `utf-8` });
				includeFileCache[file] = content;

				return {
					found: true,
					uri: file,
					content: content
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
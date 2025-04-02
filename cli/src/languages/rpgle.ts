
import { readFileSync } from 'fs';
import * as path from 'path';

import Parser from "vscode-rpgle/language/parser";
import { SourceSymbol, SymbolReferences, Targets } from '../targets';
import Cache from "vscode-rpgle/language/models/cache";
import Declaration from 'vscode-rpgle/language/models/declaration';
import { trimQuotes } from '../utils';

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

function toSymbolRefs(def: Declaration): SymbolReferences {
	if (def.references.length > 0) {
		let refs: SymbolReferences = {};
		for (const ref of def.references) {
			if (!refs[ref.uri]) {
				refs[ref.uri] = [];
			}
			refs[ref.uri].push(ref.offset);
		}

		return refs;
	}

	return {};
}

function handleSubitems(cwd, def: Declaration, newSymbol: SourceSymbol) {
	if (def.subItems.length > 0) {
		newSymbol.children = def.subItems.map(sub => {
			return {
				name: sub.name,
				type: sub.type,
				relativePath: path.relative(cwd, def.position.path), // subitems are in the same file
				references: toSymbolRefs(def)
			};
		});
	}
}

export function getExtPrRef(ref: Declaration, type: "EXTPROC"|"EXTPGM" = "EXTPROC"): string {
	const keyword = ref.keyword;
	let importName: string = ref.name;
	const ext: string | boolean = keyword[type];
	if (ext) {
		if (ext === true) importName = ref.name;
		else importName = ext;
	}

	if (importName.includes(`:`)) {
		const parmParms = importName.split(`:`);
		importName = parmParms.filter(p => !p.startsWith(`*`)).join(``);
	}

	if (importName.startsWith(`*`)) {
		importName = ref.name;
	} else {
		importName = trimQuotes(importName);
	}

	return importName;
}

export function rpgleDocToSymbolList(cwd: string, doc: Cache): SourceSymbol[] {
	let symbols: SourceSymbol[] = [];

	const allDefs = [
		...doc.constants,
		...doc.files,
		...doc.sqlReferences,
		...doc.structs,
		...doc.subroutines,
		...doc.variables
	];

	for (const def of allDefs) {
		let newSymbol: SourceSymbol = {
			name: def.name,
			type: def.type,
			relativePath: path.relative(cwd, def.position.path),
			references: toSymbolRefs(def)
		}

		handleSubitems(cwd, def, newSymbol);

		symbols.push(newSymbol);
	}

	for (const proc of doc.procedures) {
		let newSymbol: SourceSymbol = {
			name: proc.name,
			type: `procedure`,
			relativePath: path.relative(cwd, proc.position.path),
			references: toSymbolRefs(proc)
		};

		// TODO: check on parameters when there is and isn't a scope
		if (proc.scope) {
			newSymbol.children = rpgleDocToSymbolList(cwd, proc.scope);
		} else if (proc.subItems.length > 0) {
			handleSubitems(cwd, proc, newSymbol);
		}

		symbols.push(newSymbol);
	}

	return symbols;
}
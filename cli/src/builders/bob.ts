import { existsSync, readFileSync, writeFileSync } from 'fs';
import path from 'path';
import { ILEObject, ILEObjectTarget, ObjectType, Targets } from '../targets';

type OutFiles = {[filePath: string]: string};
type DirectoryTargets = {[dirname: string]: ILEObjectTarget[]};

export class BobProject {
	private dirTargets: DirectoryTargets = {};
	constructor(private targets: Targets) {
		this.dirTargets = BobProject.buildDirTargets(targets);
	}

	private static buildDirTargets(targets: Targets) {
		let list: DirectoryTargets = {};

		for (let target of targets.getTargets()) {
			if (target.relativePath) {
				const dirname = path.dirname(target.relativePath);
				if (list[dirname] === undefined) list[dirname] = [];

				list[dirname].push(target);
			}
		}

		return list;
	}

	public createRules(): OutFiles {
		let output: OutFiles = {};
		const subdirs = Object.keys(this.dirTargets);

		output[`Rules.mk`] = `SUBDIRS = ${subdirs.join(` `)}`;

		for (const subdir in this.dirTargets) {
			const targets = this.dirTargets[subdir];
			let lines: string[] = [];

			for (let target of targets) {
				lines.push(`${target.systemName}.${target.type}: ${path.relative(subdir, target.relativePath)} ${target.deps.map(d => `${d.systemName}.${d.type}`).join(` `)}`);
			}

			output[path.join(subdir, `Rules.mk`)] = lines.join(`\n`);
		}

		return output;
	}
}
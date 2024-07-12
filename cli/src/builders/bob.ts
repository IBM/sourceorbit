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

	public 

	public createRules(): OutFiles {
		let output: OutFiles = {};
		const subdirs = Object.keys(this.dirTargets);

		output[`Rules.mk`] = `SUBDIRS = ${subdirs.join(` `)}`;

		for (const subdir in this.dirTargets) {
			const targets = this.dirTargets[subdir];
			const currentRulesFile = path.join(subdir, `Rules.mk`);
			let rulesContent: string[] = [];

			if (existsSync(path.join(this.targets.getCwd(), currentRulesFile))) {
				rulesContent = readFileSync(currentRulesFile, { encoding: `utf-8` }).split(`\n`);
			}

			const rulesFile = new RulesFile(subdir, rulesContent);

			let lines: string[] = [];

			for (let target of targets) {
				rulesFile.applyRule(target);
			}

			output[currentRulesFile] = rulesFile.getContent();
		}

		return output;
	}
}

interface Rule {
	ogLine: string;
	target?: String,
	content?: String,
	isSetting?: boolean,
};

class RulesFile {
	private parsed: Rule[] = [];
	constructor(private subdir: string, lines: string[]) {
		for (let line of lines) {
			let currentRule: Rule = { ogLine: line };
			if (line.includes(`:`)) {
				const [target, content] = line.split(`:`);
				currentRule.target = target.trim().toUpperCase();
				currentRule.content = content.trim();
				currentRule.isSetting = content.includes(`=`);
			}

			this.parsed.push(currentRule);
		}
	}

	applyRule(target: ILEObjectTarget) {
		const objName = `${target.systemName}.${target.type}`;

		const existingLine = this.parsed.find(r => r.target === objName && r.isSetting !== true);

		const lineContent = `${path.relative(this.subdir, target.relativePath)} ${target.deps.filter(d => d.reference !== true).map(d => `${d.systemName}.${d.type}`).join(` `)}`;

		if (existingLine) {
			existingLine.ogLine = `${objName}: ${lineContent}`;
			existingLine.content = lineContent;
		} else {
			this.parsed.push({ ogLine: `${objName}: ${lineContent}`, target: objName, content: lineContent });
		}
	}

	getContent() {
		return this.parsed.map(r => r.ogLine).join(`\n`);
	}
}
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
			let dirname: string|undefined;
			if (target.relativePath) {
				dirname = path.dirname(target.relativePath);
			} else if (target.type === `PGM`) {
				// If there is no relative path, this might mean it's a multimodule program
				const possibleModule = targets.getTarget({systemName: target.systemName, type: `MODULE`});
				if (possibleModule) {
					dirname = path.dirname(possibleModule.relativePath);
				}
			}

			if (targets.binderRequired()) {
				if (target.deps.some(d => d.type === `SRVPGM`)) {
					target.deps = target.deps.filter(d => d.type !== `SRVPGM`);
					target.deps.push(targets.getBinderTarget());
				}
			}

			if (dirname) {
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

		let bnddirRulesDir: string|undefined = undefined;

		if (this.targets.binderRequired()) {
			const servicePrograms = this.targets.getResolvedObjects(`SRVPGM`);
			const relativePaths = [...new Set(servicePrograms.map(sp => path.dirname(sp.relativePath)))];

			if (relativePaths.length >= 1) {
				// We know the rules
				bnddirRulesDir = relativePaths[0];
			}
		}

		for (const subdir in this.dirTargets) {
			const targets = this.dirTargets[subdir];
			const currentRulesFile = path.join(subdir, `Rules.mk`);
			const currentFullPath = path.join(this.targets.getCwd(), currentRulesFile);
			let rulesContent: string[] = [];

			if (existsSync(currentFullPath)) {
				rulesContent = readFileSync(currentFullPath, { encoding: `utf-8` }).split(`\n`);
			}

			const rulesFile = new RulesFile(subdir, rulesContent);

			for (let target of targets) {
				rulesFile.applyRule(target);
			}

			if (subdir === bnddirRulesDir) {
				rulesFile.applyRule({
					...this.targets.getBinderTarget(),
					deps: this.targets.getResolvedObjects(`SRVPGM`),
				});
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
	isUserWritten?: boolean,
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
				currentRule.isUserWritten = content.includes(`=`) || content.trimStart().startsWith(`#`);
			}

			this.parsed.push(currentRule);
		}
	}

	applyRule(target: ILEObjectTarget) {
		const objName = `${target.systemName}.${target.type}`;

		const existingLine = this.parsed.find(r => r.target === objName && r.isUserWritten !== true);

		const lineContent = `${target.relativePath ? path.relative(this.subdir, target.relativePath) + ' ' : ``}${target.headers ? target.headers.join(` `) + ` ` : ``}${target.deps.filter(d => d.reference !== true).map(d => `${d.systemName}.${d.type}`).join(` `)}`.trimEnd();

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
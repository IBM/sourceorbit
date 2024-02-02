import { warningOut } from "./cli";
import { FileLog, Logger } from "./logger";
import { ILEObject, ILEObjectTarget, Targets } from "./targets";
import * as path from "path";

export class TestBuilder {
  constructor(private testModules: ILEObject[], private logger: Logger) {
  }

  static getRunnerSourcePath(pgm?: boolean) {
    return pgm ? `.so/runner.pgm.rpgle` : `.so/runner.rpgle`;
  }

  static generatePrototypesForTestExports(exports: string[]) {
    return exports.map(e => [`Dcl-Pr ${e} ExtProc;`, `End-Pr;`, ``].join(`\n`)).join(`\n`);
  }

  getRunnerStructure() {
    let entryModule: ILEObject = {
      systemName: `RUNNER`,
      type: `MODULE`,
      relativePath: TestBuilder.getRunnerSourcePath(),
      extension: `rpgle`,
      testModule: true,
      text: `Module for test runner`,
    };

    let runnerProgram: ILEObjectTarget = {
      systemName: `RUNNER`,
      type: `PGM`,
      extension: `pgm`,
      deps: [entryModule, ...this.testModules],
      text: `Program for test runner`,
    };

    let lines = [
      `**free`,
      ``,
      `ctl-opt main(main);`,
      ``,
      `Dcl-PR printf Int(10) extproc('printf');`,
      `  *n Pointer value options(*string);`,
      `end-pr;`,
      ``,
      TestBuilder.generatePrototypesForTestExports(this.testModules.map(m => m.exports).flat()),
      ``,
    ];

    lines.push(
      `dcl-proc main;`,
      `dcl-pi *n;`,
      `  runName char(32);`,
      `end-pi;`,
    );

    for (const testModule of this.testModules) {
      this.logger.flush(testModule.relativePath);
      const pathDetail = path.parse(testModule.relativePath);

      if (testModule.exports === undefined || testModule.exports?.length === 0) {
        this.logger.fileLog(testModule.relativePath, { type: `warning`, message: `No exports found in module.` });
        continue;
      }

      if (testModule.imports) {
        if (!testModule.imports.some(i => i.toUpperCase() === `ASSERT`)) {
          this.logger.fileLog(testModule.relativePath, { type: `warning`, message: `No assert import found in module.` });
        }
      }

      for (const exportName of testModule.exports) {
        if (exportName.length > 32) {
          this.logger.fileLog(testModule.relativePath, { type: `info`, message: `Export name ${exportName} is more than 32 characters. It is recommended to use shorter than 32 characters.` });
        }

        lines.push(`if (runName = '' OR runName = '${pathDetail.name}' OR runName = '${exportName}');`);
        lines.push(`printf('${exportName}:START' + x'25');`);
        lines.push(`monitor;`);
        lines.push(`  ${exportName}();`);
        lines.push(`  printf('${exportName}:SUCCESS' + x'25');`);
        lines.push(`on-error;`);
        lines.push(`  printf('${exportName}:LOG:Use CALL RUNNER ''${exportName}'')' + x'25');`);
        lines.push(`  printf('${exportName}:CRASH' + x'25');`);
        lines.push(`endmon;`);
        lines.push(`printf('${exportName}:END' + x'25');`);
        lines.push(`endif;`);
      }
    }

    lines.push(`end-proc;`);

    return {
      lines,
      newObjects: {
        program: runnerProgram,
        module: entryModule,
      }
    }
  }
}
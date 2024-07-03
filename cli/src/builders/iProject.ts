import { fromCl } from "../utils";
import { CompileData, CommandParameters, CompileAttribute, getDefaultCompiles, Action, getObjectType } from "./environment";

export class iProject {
  includePaths?: string[] = [];
	compiles?: CompileAttribute = getDefaultCompiles();
	objectAttributes?: {
		[object: string]: CommandParameters
	} = {};
	binders?: string[] = [];

  constructor() {

  }

  applySettings(input: Partial<iProject>) {
    if (input.includePaths && input.includePaths.length > 0) {
      this.includePaths = input.includePaths;
    }
  
    if (input.binders && input.binders.length > 0) {
      this.binders = input.binders;
    }
  
    if (input.compiles) {
      for (const [ext, data] of Object.entries(input.compiles)) {
        // We don't want to fully overwrite the default settings,
        // perhaps the user is only changing the `dir`?
        this.compiles[ext] = {
          ...(this.compiles[ext] || {}),
          ...data
        };
      }
    }
  }

  applyAction(newAction: Action) {
    if (newAction.environment && newAction.environment === `ile` && newAction.extensions && newAction.extensions.length > 0) {
      if (!newAction.extensions.includes(`GLOBAL`)) {
        const firstExt = newAction.extensions[0].toLowerCase();
        const becomesObject = getObjectType(firstExt);
        const commandData = fromCl(newAction.command);
        this.compiles[firstExt] = {
          becomes: becomesObject,
          ...commandData
        };
      }
    }
  }
}
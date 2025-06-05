import { ObjectType } from "../targets";
import { CommandParameters, CompileAttribute, getDefaultCompiles } from "./environment";

export class iProject {
  includePaths?: string[] = [];
	compiles?: CompileAttribute = getDefaultCompiles();
	objectAttributes?: {
		[object: string]: CommandParameters
	} = {};
	binders?: string[] = [];

  constructor() {

  }

  getCompileDataForType(type: ObjectType) {
    return Object.values(this.compiles).find(data => data.becomes === type);
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
}
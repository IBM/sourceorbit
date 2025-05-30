import { ReadFileSystem } from "../../readFileSystem";
import { Targets } from "../../targets";
import path from "path";

export interface Action {
  name: string,
  command: string,
  environment: "ile",
  extensions: string[],
}

export class ProjectActions {
  private actions: { [relativePath: string]: Action[] } = {};

  constructor(private readonly targets: Targets, private readonly readFileSystem: ReadFileSystem) {}

  get getActionPaths() {
    return Object.keys(this.actions);
  }

  public async loadAllActions() {
    const cwd = this.targets.getCwd();
    const files = await this.readFileSystem.getFiles(cwd, `**/actions.json`, {dot: true});

    for (const file of files) {
      const relativePath = path.relative(cwd, file);
      const contents = await this.readFileSystem.readFile(file);
      try {
        const possibleActions = JSON.parse(contents) as Partial<Action[]>;

        for (const possibleAction of possibleActions) {
          if (!possibleAction.name || !possibleAction.command || !possibleAction.environment || !possibleAction.extensions) {
            // TODO: Log a warning about missing required fields
            continue; // Skip if required fields are missing
          }

          possibleAction.extensions = possibleAction.extensions.map(ext => ext.toLowerCase());
        }

        this.actions[relativePath] = possibleActions as Action[];
      } catch (e) {
        console.log(`Error parsing actions.json at ${relativePath}:`, e);
      }
    }

    const vscodePath = path.join(`.vscode`, `actions.json`);
    if (this.actions[vscodePath]) {
      // If there is a .vscode/actions.json, it is the project actions
      this.actions[`actions.json`] = this.actions[vscodePath];
      delete this.actions[vscodePath];
    }
  }

  getActionForPath(relativeSourcePath: string): Action|undefined {
    let allPossibleActions: Action[] = [];
    let parent: string = relativeSourcePath;

    while (true) {
      parent = path.dirname(parent);

      const actionFile = path.join(parent, `actions.json`);

      if (this.actions[actionFile]) {
        allPossibleActions = allPossibleActions.concat(this.actions[actionFile]);
      }

      if (parent === `.`) {
        // Reached the root directory, stop searching
        break;
      }
    }

    let extension = path.extname(relativeSourcePath).toLowerCase();

    if (extension.startsWith(`.`)) {
      extension = extension.slice(1);
    }

    allPossibleActions = allPossibleActions.filter((action) => {
      return action.extensions.includes(extension);
    });

    return allPossibleActions[0];
  }
}
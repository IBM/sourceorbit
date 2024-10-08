import { CustomExecution, EventEmitter, ExtensionContext, Pseudoterminal, Task, TaskDefinition, TaskScope, TerminalDimensions, WorkspaceFolder, tasks } from 'vscode';
import { LanguageClientManager } from './languageClientManager';

export namespace SourceOrbitTask {
	interface SourceOrbitTask extends TaskDefinition {
		builder: "bob" | "make" | "json";
	}

	export function initializeTaskProvider(context: ExtensionContext) {
		context.subscriptions.push(
			tasks.registerTaskProvider('sourceorbit', {
				provideTasks: () => {
					return [];
				},
				resolveTask(_task, _token) {
					const task = _task.definition as SourceOrbitTask;
					// A Rake task consists of a task and an optional file as specified in RakeTaskDefinition
					// Make sure that this looks like a Rake task by checking that there is a task.
					if (task) {
						// resolveTask requires that the same definition object be used.
						const workspaceFolder = _task.scope as WorkspaceFolder;

						return new Task(
							task,
							TaskScope.Workspace,
							`Source Orbit Builder`,
							'sourceorbit',
							new CustomExecution(async (e) => {
								const writeEmitter = new EventEmitter<string>();
								const closeEmitter = new EventEmitter<number>();

								const term: Pseudoterminal = {
									onDidWrite: writeEmitter.event,
									onDidClose: closeEmitter.event,
									open: async (_initialDimensions: TerminalDimensions | undefined) => {
										writeEmitter.fire(`Generating ${task.builder} files for ${workspaceFolder.name}...\r\n`);
										try {
											await LanguageClientManager.generateBuildFile(workspaceFolder, task.builder);

											writeEmitter.fire(`Finished\r\n`);
											closeEmitter.fire(0);
										} catch (_e) {
											writeEmitter.fire(`Failed\r\n`);
											writeEmitter.fire(JSON.stringify(e, null, 2) + `\r\n`);
											closeEmitter.fire(1);
										}
									},
									close: function (): void { console.log(); }
								};

								return term;
							})
						);
					}
					return undefined;
				},
			})
		);
	}
}
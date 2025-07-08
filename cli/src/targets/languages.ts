import { FileOptions, Targets } from ".";
import { clExtensions, clleTargetCallback } from "./languages/clle";
import { ddsExtension, ddsTargetCallback } from "./languages/dds";
import { rpgExtensions, rpgleTargetParser } from "./languages/rpgle";
import { sqlExtensions, sqlTargetCallback } from "./languages/sql";
import { binderExtensions, binderTargetCallback } from "./languages/binder";
import { cmdExtensions, cmdTargetCallback } from "./languages/cmd";
import { noSourceObjects, noSourceTargetCallback } from "./languages/nosrc";

export type LanguageCallback = (targets: Targets, relativePath: string, content: string, options: FileOptions) => Promise<void>
interface LanguageGroup {
  extensions: string[];
  callback: LanguageCallback;
}

export class TargetsLanguageProvider {
  private languages: LanguageGroup[] = [
    {extensions: clExtensions, callback: clleTargetCallback},
    {extensions: sqlExtensions, callback: sqlTargetCallback},
    {extensions: ddsExtension, callback: ddsTargetCallback},
    {extensions: binderExtensions, callback: binderTargetCallback},
    {extensions: cmdExtensions, callback: cmdTargetCallback},
    {extensions: noSourceObjects, callback: noSourceTargetCallback}
  ];

  constructor(private readonly targets: Targets) {
    const rpgleTargets = new rpgleTargetParser(this.targets);
    this.languages.push({
      extensions: rpgExtensions,
      callback: (targets, relativePath, content, options) => {
        return rpgleTargets.rpgleTargetCallback(targets, relativePath, content, options);
      }
    })
  }

  public getExtensions() {
    return this.languages.map(lang => lang.extensions).flat();
  }

  public getGlob() {
    const allExtensions = this.getExtensions();
    return `**/*.{${allExtensions.join(`,`)},${allExtensions.map(e => e.toUpperCase()).join(`,`)}}`;
  }

  public async handleLanguage(relativePath: string, content: string, options: FileOptions = {}) {
    const ext = relativePath.split('.').pop()?.toLowerCase();
    const language = this.languages.find(lang => lang.extensions.includes(ext));
    if (ext && language) {
      await language.callback(this.targets, relativePath, content, options);
    }
  }

  public registerLanguage(extensions: string[], callback: LanguageCallback) {
    for (const ext of extensions) {
      if (this.languages.some(lang => lang.extensions.includes(ext))) {
        throw new Error(`Language with extension '${ext}' is already registered.`);
      }
    }

    this.languages.push({extensions, callback});
  }
}
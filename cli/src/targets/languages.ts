import { FileOptions, Targets } from ".";
import { clExtensions, cmdExtensions, ddsExtension, rpgExtensions, sqlExtensions, srvPgmExtensions } from "../extensions";
import { clleTargetCallback } from "./languages/clle";
import { ddsTargetCallback } from "./languages/dds";
import { rpgleTargetParser } from "./languages/rpgle";
import { sqlTargetCallback } from "./languages/sql";
import { binderTargetCallback } from "./languages/binder";
import { cmdTargetCallback } from "./languages/cmd";

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
    {extensions: srvPgmExtensions, callback: binderTargetCallback},
    {extensions: cmdExtensions, callback: cmdTargetCallback}
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
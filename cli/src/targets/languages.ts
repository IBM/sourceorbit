import { FileOptions, ObjectType, Targets } from ".";
import { clExtensions, clleTargetCallback, clObjects } from "./languages/clle";
import { ddsExtension, ddsObjects, ddsTargetCallback } from "./languages/dds";
import { rpgExtensions, rpgleTargetParser, rpgObjects } from "./languages/rpgle";
import { sqlExtensions, sqlObjects, sqlTargetCallback } from "./languages/sql";
import { binderExtensions, binderObjects, binderTargetCallback } from "./languages/binder";
import { cmdExtensions, cmdObjects, cmdTargetCallback } from "./languages/cmd";
import { noSourceObjects, noSourceTargetCallback, noSourceTargetObjects } from "./languages/nosrc";

export type LanguageCallback = (targets: Targets, relativePath: string, content: string, options: FileOptions) => Promise<void>
interface LanguageGroup {
  extensions: string[];
  callback: LanguageCallback;
}

export type ExtensionMap = {[ext: string]: ObjectType};

export class TargetsLanguageProvider {
  private languageTargets: LanguageGroup[] = [];
  private extensionMap: ExtensionMap = {};

  constructor(private readonly targets: Targets) {
    const rpgleTargets = new rpgleTargetParser(this.targets);

    this.registerLanguage(clExtensions, clleTargetCallback, clObjects);
    this.registerLanguage(sqlExtensions, sqlTargetCallback, sqlObjects);
    this.registerLanguage(ddsExtension, ddsTargetCallback, ddsObjects);
    this.registerLanguage(binderExtensions, binderTargetCallback, binderObjects);
    this.registerLanguage(cmdExtensions, cmdTargetCallback, cmdObjects);
    this.registerLanguage(noSourceObjects, noSourceTargetCallback, noSourceTargetObjects);

    this.registerLanguage(rpgExtensions, (targets, relativePath, content, options) => {
      return rpgleTargets.rpgleTargetCallback(targets, relativePath, content, options);
    }, rpgObjects);
  }

  public getExtensions() {
    return this.languageTargets.map(lang => lang.extensions).flat();
  }

  public getGlob() {
    const allExtensions = this.getExtensions();
    return `**/*.{${allExtensions.join(`,`)},${allExtensions.map(e => e.toUpperCase()).join(`,`)}}`;
  }

  public async handleLanguage(relativePath: string, content: string, options: FileOptions = {}) {
    const ext = relativePath.split('.').pop()?.toLowerCase();
    const language = this.languageTargets.find(lang => lang.extensions.includes(ext));
    if (ext && language) {
      await language.callback(this.targets, relativePath, content, options);
    }
  }

  public registerLanguage(extensions: string[], callback: LanguageCallback, objectTypes: ExtensionMap = {}) {
    for (const ext of extensions) {
      if (this.languageTargets.some(lang => lang.extensions.includes(ext))) {
        throw new Error(`Language with extension '${ext}' is already registered.`);
      }
    }

    this.extensionMap = {...this.extensionMap, ...objectTypes};

    this.languageTargets.push({extensions, callback});
  }

  public getObjectType(ext: string): ObjectType | undefined {
    return this.extensionMap[ext.toLowerCase()];
  }
}
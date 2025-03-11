import { Targets } from "../../targets";
import { writeFileSync } from "fs";

export class SqlBuilder {
  constructor(private targets: Targets) {}

  write(toPath: string) {
    const content = [
      `-- This file was generated by Source Orbit`,
      ``,
      SqlBuilder.getCreateStatements(),
      ``,
      this.generateResolved(),
      ``,
      this.generateDependencies(),
      ``,
      this.generateExports()
    ].join(`\n`);

    writeFileSync(toPath, content, { encoding: `utf-8` });
  }

  static getCreateStatements(): string {
    let sql = ``;

    sql += `create table if not exists resolved_objects (\n`;
    sql += `  systemName varchar(10) not null,\n`;
    sql += `  longName varchar(50),\n`;
    sql += `  objectType varchar(10) not null,\n`;
    sql += `  text varchar(255),\n`;
    sql += `  relativePath varchar(255),\n`;
    sql += `  extension varchar(10),\n`;
    sql += `  reference boolean,\n`;
    sql += `  primary key (systemName, objectType)\n`;
    sql += `);\n\n`;

    sql += `create table if not exists dependencies (\n`;
    sql += `  baseSystemName varchar(10) not null,\n`;
    sql += `  baseObjectType varchar(10) not null,\n`;
    sql += `  depSystemName varchar(10) not null,\n`;
    sql += `  depObjectType varchar(10) not null,\n`;
    sql += `  primary key (baseSystemName, baseObjectType, depSystemName, depObjectType)\n`;
    sql += `);\n\n`;

    sql += `create table if not exists symbols (\n`;
    sql += `  exportName varchar(255) not null,\n`;
    sql += `  symbolType varchar(10) not null,\n`;
    sql += `  systemName varchar(10) not null,\n`;
    sql += `  objectType varchar(10) not null,\n`;
    sql += `  primary key (exportName, symbolType, systemName, objectType)\n`;
    sql += `);\n\n`;
    
    sql += `delete from resolved_objects;\n\n`;
    sql += `delete from dependencies;\n\n`;
    sql += `delete from symbols;\n\n`;

    return sql;
  }

  generateResolved(): string {
    const resolved = this.targets.getResolvedObjects();

    let sql = `insert into resolved_objects (systemName, longName, objectType, text, relativePath, extension, reference) values\n`;

    for (const obj of resolved) {
      sql += `('${obj.systemName}', '${obj.longName || ``}', '${obj.type}', '${obj.text || ``}', '${obj.relativePath || ``}', '${obj.extension || ``}', ${obj.reference === true}),\n`;
    }

    sql = sql.slice(0, -2) + `;`;

    return sql;
  }

  generateDependencies(): string {
    let sql = `insert into dependencies (baseSystemName, baseObjectType, depSystemName, depObjectType) values\n`;

    const toBeBuilt = this.targets.getTargets();

    for (const obj of toBeBuilt) {
      const dependencies = obj.deps;

      for (const dep of dependencies) {
        sql += `('${obj.systemName}', '${obj.type}', '${dep.systemName}', '${dep.type}'),\n`;
      }
    }

    sql = sql.slice(0, -2) + `;`;
    return sql;
  }

  generateExports(): string {
    let sql = `insert into symbols (exportName, symbolType, systemName, objectType) values\n`;

    const allObjects = this.targets.getResolvedObjects();

    for (const obj of allObjects) {
      const exports = obj.exports;
      const imports = obj.imports;

      if (exports) {
        for (const exp of exports) {
          sql += `('${exp}', 'export', '${obj.systemName}', '${obj.type}'),\n`;
        }
      }

      if (imports) {
        for (const imp of imports) {
          sql += `('${imp}', 'import', '${obj.systemName}', '${obj.type}'),\n`;
        }
      }
    }

    sql = sql.slice(0, -2) + `;`;
    return sql;
  }
}
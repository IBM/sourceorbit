# Source Code Rules

This page will outline the rules of an IBM i project, including what extensions are expected.

> [!WARNING]
> Source Orbit does not care about project structure, but does enforce these rules for source code.

## Supported extensions

| Extension  | Extension meaning  | Notes                               |
| ---------- | ------------------ | ----------------------------------- |
| `sqlrpgle` | RPGLE (SQL)        | Can use `.pgm` notation             |
| `rpgle`    | RPGLE              | Can use `.pgm` notation             |
| `clle`     | CLLE               | Can use `.pgm` notation             |
| `cl`       | CLLE               | Can use `.pgm` notation             |
| `clp`      | CL (OPM)           | Should be renamed to `clle`         |
| `pf`       | Physical File      | Table                               |
| `lf`       | Logical File       | View/Index                          |
| `dspf`     | Display File       |                                     |
| `prtf`     | Printer File       |                                     |
| `msgf`     | Message filee      |                                     |
| `sql`      | Any SQL object     | Type determined by create statement |
| `table`    | SQL table          | Expects `CREATE TABLE`              |
| `view`     | SQL view           | Expects `CREATE VIEW`               |
| `index`    | SQL index          | Expects `CREATE INDEX`              |
| `alias`    | SQL alias          | Expects `CREATE ALIAS`              |
| `sqlalias` | SQL alias          | Expects `CREATE ALIAS`              |
| `sqlprc`   | SQL procedure      | Expects `CREATE PROCEDURE`          |
| `sqludf`   | SQL function       | Expects `CREATE FUNCTION`           |
| `sqludt`   | SQL table function | Expects `CREATE FUNCTION`           |
| `sqltrg`   | SQL trigger        | Expects `CREATE TRIGGER`            |
| `sqlseq`   | SQL sequence       | Expects `CREATE SEQUENCE`           |
| `bnd`      | Binder source      |                                     |
| `binder`   | Binder source      | Should be renamed `bnd`             |
| `cmd`      | Command            |                                     |

## The system object name is the basename of the source file

| Source name           | Resulting object |
| --------------------- | ---------------- |
| `LCUS.LF`             | `LCUS.FILE`      |
| `CUS.PF`              | `PCUS.FILE`      |
| `dothing.cmd`         | `DOTHING.CMD`    |
| `pos123.dspf`         | `POS123.FILE`    |
| `faq500.rpgle`        | `FAQ500.MODULE`  |
| `faq401.pgm.sqlrpgle` | `FAQ401.PGM`     |
| `faq500.bnd`          | `FAQ500.SRVPGM`  |

* if your source includes `.pgm`, then it will become a program.
   * `mypgm.pgm.rpgle` becomes a `MYPGM.PGM` object
   * If you want to use multi-module programs, go ahead and add the prototypes to export functions/procedures in other modules and Source Orbit will automatically take care of the binding. See *Service Programs and Binder Source* below.
* if your source does not include `.pgm`, then it will become a module, cmd, dtaara, etc.
* assumes binder source (`.bnd`/`.binder`) is a service program. 
   * Source Orbit will scan the binder source to find the exported functions/procedures from modules inside the project.
* Source Orbit does support SQL long names. But, where a long name is used, a system name should also be used (`FOR SYSTEM NAME` or `SPECIFIC`)

### Long file names

Typically, the basename of the file is also the object name. But, Source Orbit will generate a deterministic object name based on the file name.

| Source name                      | Resulting object | Note                                                                    |
| -------------------------------- | ---------------- | ----------------------------------------------------------------------- |
| `abcd.rpgle`                     | `ABCD.MODULE`    | No change                                                               |
| `ab_cd.pgm.rpgle`                | `AB_CD.PGM`      | No change                                                               |
| `thisIsASuperLongName.pgm.clle`  | `TIASLN.PGM`     | First character plus following capitals                                 |
| `FetchUserData.cmd`              | `FUD.CMD`        | All capitals from file name                                             |
| `ua_fetchUserData.sqlrpgle`      | `UAFUD.MODULE`   | Prefix, followed by first post-prefix character plus following capitals |
| `ART200D-Work_with_Article.DSPF` | `ART200D.FILE`   | Support for ibmi-bob file name                                          |

Even if you use long file names, your source code still needs to reference the object name for object resolves (not including *include directives* of course.)

## Unresolved objects

When using Source Orbit, you may find that you want to reference objects that do not existing in the same repository. Perhaps those objects/sources exist in another repository or already exist in a library on the system you're building on this. Usually, Source Orbit will complain about unresolved objects with an error like:

> `no object found for reference 'COUNTRY'`

If you know those objects do exist but outside of this project, then you can use the `.objrefs` file (references file) in the project root to list those objects and if that object is a service program, you can also list the exports by indenting after the service program reference. Comments can start with `#`

```
# Files that exist outside this project

COUNTRY.FILE

# Service programs outside this project

UTILS.SRVPGM
  toUpper
  toLower
```

If you use a references file, but you have source that defines the object, the source will take precedence over the reference and a log will be issued on the source.

## Service Programs and Binder Source

For service programs to be created, binder source must exist for it. Source Orbit will read the binder source, find all the referenced export functions/procedures and will find the correct module that exports them to create the service program.

When Source Orbit is resolving programs & service programs, and specifically resolving their import functions/procedures, it will:

1. Look to see if a service program exports this named function/procedure and then depend on that service program
2. If no service program is found, then Source Orbit will search all available modules for that named function/procedure. If a module is found, then the module will be bound into the program (and therefore will become a multi module program)

## SQL sources

SQL sources can be defined with many different extensions like `.sql`, `.table`, `.sqlprc`, `.view`, and so on. It is important that the name of source matches the system name of the object that is going to be created.

```sql
-- cusord.sql

CREATE OR REPLACE TABLE CUSORD (...)
```

## Embedded SQL in RPGLE C specs

**This section is not true as of Source Orbit CLI 1.1.0**

Source Orbit does not support embedded SQL (`exec sql`) used in a C spec. - no problem with mixed-format or free-format. We recommend:

1. converting the embedded SQL statements (`exec sql`) to not have C specs, or
2. you use user-open (`USROPN`) file definitions for those files, which you would never open.

```rpgle
      * User open files as refs, they are never opened
     FPSBORDS   IF   E           K DISK    USROPN
     FPMESSGS   IF   E           K DISK    USROPN
```

## Terminology

| Term              | Definitions                                                                                                    |
| ----------------- | -------------------------------------------------------------------------------------------------------------- |
| Pseudo            | CL source code to build an object that doesn't typically have source code (like a MSGF, DTAARA, etc)           |
| Referenced object | An object where the source is not contained in the project, but does exist at build time (in the library list) |
| Incremental build | The building on only specific objects, not the entire project                                                  |
| Full build        | The build of the entire project                                                                                |
| Dependency        | An object which is required in order for a build of a certain object to happen                                 |
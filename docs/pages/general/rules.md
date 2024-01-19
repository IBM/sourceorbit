This page will outline the rules of an IBM i project, including what extensions are expected.

Source Orbit does not care about project structure, but does enforce these rules for source code:

### Supported extensions

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

### The system object name is the basename of the source file

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
   * If you want to use multi-module programs, go ahead and add the prototypes to export functions/procedures in other modules and Source Orbit will automatically take care of the binding.
* if your source does not include `.pgm`, then it will become a module, cmd, dtaara, etc.
* assumes binder source (`.bnd`/`.binder`) is a service program. 
   * Source Orbit will scan the binder source to find the exported functions/procedures from modules inside the project.
* Source Orbit does not yet support SQL long name references. (Coming soon)

#### Long file names

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

### SQL sources

SQL sources can be defined with many different extensions like `.sql`, `.table`, `.sqlprc`, `.view`, and so on. It is important that the name of source matches the system name of the object that is going to be created.

```sql
-- cusord.sql

CREATE OR REPLACE TABLE CUSORD (...)
```

### Embedded SQL in RPGLE C specs

Source Orbit does not support embedded SQL (`exec sql`) used in a C spec. - no problem with mixed-format or free-format. We recommend:

1. converting the embedded SQL statements (`exec sql`) to not have C specs, or
2. you use user-open (`USROPN`) file definitions for those files, which you would never open.

```rpgle
      * User open files as refs, they are never opened
     FPSBORDS   IF   E           K DISK    USROPN
     FPMESSGS   IF   E           K DISK    USROPN
```
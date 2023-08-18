This page will outline the rules of an IBM i project, including what extensions are expected.

Source Orbit does not care about project structure, but does enforce these rules for source code:

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
   * Source Orbit only supports single module programs
* if your source does not include `.pgm`, then it will become a module, cmd, dtaara, etc.
   * `mysrvpgm.rpgle` becomes `MYSRVPGM.MODULE` and then `MYSRVPGM.PGM`
   * though this is only if this module is not automatically resolved via binder source.
* assumes binder sources (`.bnd`/`.binder`) is a service program. Source Orbit will scan the binder source to find the modules for the service program automatically.
* Source Orbit does not yet support SQL long name references. (Coming soon)

### SQL sources

SQL sources can be defined with many different extenions like `.sql`, `.table`, `.sqlprc`, `.view`, and so on. It is important that the name of source matches the system name of the object that is going to be created.

```sql
-- cusord.sql

CREATE OR REPLACE TABLE CUSORD (...)
```

### Embedded SQL in RPGLE C specs

Source Orbit does not support embedded SQL (`exec sql`) used in a C spec. - no problem with mixed-format or free-format. We recommend 

1. converting the embedded SQL statements (`exec sql`) to not have C specs, or
2. you use user-open (`USROPN`) file definitions for those files, which you would never open.

```rpgle
      * User open files as refs, they are never opened
     FPSBORDS   IF   E           K DISK    USROPN
     FPMESSGS   IF   E           K DISK    USROPN
```
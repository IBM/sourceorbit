# Source Orbit

Source Orbit is a depenendency management tool for RPGLE/CLLE/SQL/DDS/binder sources.

## General usage

Source Orbit will scan the sources in the current working directory (CWD) and can do a few things:

* List all project objects with `--verbose`
* Show object dependencies with `-l`
* Generate a makefile based on the dependency tree to build the project
* Rename files easily (`.rpgle`/`.clle`/`.clp` -> `.pgm.rpgle`/`.pgm.clle`/`.clle`) with `-ar`
* Fix RPGLE includes if the source is found locally (useful for converting from member include style)

```sh
so -h
so -bf make && gmake BIN_LIB=libname
```
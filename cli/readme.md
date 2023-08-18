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

## Project structure

Source Orbit does not care about project structure, but does enforce these rules for source code names:

* the system object name is the basename of the source file
* if your source includes `.pgm`, then it will become a program.
   * `mypgm.pgm.rpgle` becomes a `MYPGM.PGM` object
   * Source Orbit only supports single module programs
* if your source does not include `.pgm`, then it will become a module, cmd, dtaara, etc.
   * `mysrvpgm.rpgle` becomes `MYSRVPGM.MODULE` and then `MYSRVPGM.PGM`
* assumes binder sources (`.bnd`/`.binder`) is a service program. Source Orbit will scan the binder source to find the modules for the service program automatically.
* Source Orbit does not yet support SQL long name references.

## Binding handled automatically

If you are making use of service programs, Source Orbit will automatically maintain this for you. Service programs will be added to the binding directory automatically and programs automatically include them. No need to use the `BNDDIR` header. Be careful, because changing the custom compile commands from the default may break this functionality.

## Changing compile options

This section only applies if you use `-bf make`. ibmi-bob handles compile commands seperatly.

The `iproj.json` can contain a `compiles` property to customise how each extension type gets compiled. To generate the defaults, you can use `so --init` to create or update the `iproj.json` file. When you define an extension in the `compiles` property, the properties of it will override the `so` defaults.

Here is the schema for the compiles option:

```ts
interface iProjFile {
	compiles: {[extension: string]: CompileData};
}

interface CompileData {
	/** indicates what type of object will be built from this source */
	becomes: ObjectType;
	/** will copy the source to a temp member first */
	member?: boolean,
	/** `commands` do not respect the library list and is run before 'command' */
	commands?: string[]
	/** `command` does respect the library list */
	command?: string;
	/** Used if the commands are built up from source. Usually means `command` and `commands` is blank */
	commandSource?: boolean;
	/** if the non-source object now requires source. Use make generic name like `qbndsrc/%.bnd` */
	targetSource?: string;
};
```

### Example: build object from commands

Objects like data areas do not have source. In the `compiles` property, there is a flag that can be used to tell Source Orbit to get the commands from the source.

```json
"dtaara": {
	"becomes": "DTAARA",
	"commandSource": true
}
```

So if a file exists named `version.dtaara`, then the CL commands inside it will be used to create that `.DTAARA` object.

```cl
CRTDTAARA DTAARA(*CURLIB/VERSION) TYPE(*CHAR) LEN(20)
CHGDTAARA DTAARA(*CURLIB/VERSION) VALUE('1.0')
```

When `commandSource` is enabled, CL commands may error but that will not stop the build as these errors are ignored.

## Embedded SQL

Source Orbit does not support embedded SQL (`exec sql`) used in a C spec. - no problem with mixed-format or free-format. We recommend 

1. converting the embedded SQL statements (`exec sql`) to not have C specs, or
2. you use user-open (`USROPN`) file definitions for those files, which you would never open.

```rpgle
      * User open files as refs, they are never opened
     FPSBORDS   IF   E           K DISK    USROPN
     FPMESSGS   IF   E           K DISK    USROPN
```


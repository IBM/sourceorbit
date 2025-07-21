# Make

This page describes information about the `makefile` that is generated for a project when using `-bf make`.

This section only applies if you use `-bf make`.

## Entire builds vs Incremental builds

Using `so -bf make` will generate a makefile to rebuild the entire project. This is fine when deploying to new systems. It is also possible to do incremental builds.

To do an incremental build, also referred to as a partial build, you can specify the targets you want to build:

```bash
so -bf make -f 'qrpglesrc/ord500.pgm.rpgle'
so -bf make -l `git diff --name-only origin/main origin/${GITHUB_HEAD_REF}`
```

A incremental build means building a specific targets, their parents and optionally their children. Let's assume this is our dependency tree:

```
--> ORD500.PGM (qrpglesrc/ord500.pgm.rpgle)
--^---> ORD501.PGM (qrpglesrc/ord500.pgm.rpgle)
------^---> DEPARTMENT.FILE (qsqlsrc/department.sql)
------^---> DEPTS.FILE (qddssrc/depts.dspf)
```

Next, assume that we want to do a incremental build of `ORD501.PGM`, which has

* one parent: `ORD500.PGM`
* two children: `DEPARTMENT.FILE` and `DEPTS.FILE`

So that means that 3 objects are going to be rebuilt. Sometimes, we don't want to rebuild the children because they haven't changed (and can depend on the library list to find the existing objects).

Usually, parents always need to be rebuilt to ensure level checking happens. If you use the `-wp` (with-parents) options, then the `makefile` will also include targets to rebuild the parent objects too (`ORD500`), but the `all` target will only build the specified target (`ORD501`).

### When is a incremental build right?

It is always recommended to do a incremental build when working in a new branch. Ensure that you have a library of objects from a previous full build on the library list.

## `iproj.json` properties

Source Orbit makes use of a few properties that you can put inside your `iproj.json` file.

| Property       | Type                                                                                                                             |
| -------------- | -------------------------------------------------------------------------------------------------------------------------------- |
| `includePaths` | Array of strings. Use this if you want to include headers that can be found in the IFS at compile time. **You do not need to list directories that are part of the repository.** |

## Binding handled automatically

If you are making use of service programs, Source Orbit will automatically maintain this for you. Your service programs will be added to the binding directory automatically and programs automatically include them. **No need to use the `BNDDIR` header** in your source code. Be careful, because changing the custom compile commands from the default may break this functionality. **You should still create binder source for all your service programs** in order for Source Orbit to create them automatically.

If your project contains a `.bnddir` file, then the basename of that file will be used as the binding directory name. If you do not have a `.bnddir` file, then the binding directory will be named `APP` automatically.

## Changing compile options

When you use `so -bf make`, Source Orbit will generate a makefile that contains the compile commands for each object. The compile commands can come from two places

### Setting up generic compiles (`actions.json`)

The `actions.json` standard determines how files that match specific extensions are compiled. The file is typically created by a user and is located in `.vscode/actions.json`. The `actions.json` standard comes from Code for IBM i and is now used by Source Orbit.

Check out how to use the `actions.json` file in the [Code for IBM i documentation](https://codefori.github.io/docs/developing/local/actions/).

#### Multiple levels of actions

The `.vscode/actions.json` is the default location for the `actions.json` file. However, you can also create a `actions.json` file in a specific directory, which will be merged with the default one. This allows you to have directory specific actions, while still using the defaults.

```
.vscode:
  actions.json      ## Actions defined here apply to the entire project

qddssrc:
  actions.json      ## Actions defined here only apply to qddssrc
  department.table
  employee.table
  emps.dspf
  nemp.dspf

qrpglesrc:
  depts.pgm.sqlrpgle
  empdet.sqlrpgle
  employees.pgm.sqlrpgle
  mypgm.pgm.rpgle
  newemp.pgm.sqlrpgle
```

### Changing project wide compiles (`iproj.json`)

The `iproj.json` can contain a `compiles` property to customize how each extension type gets compiled. To generate the defaults, you can use `so --init` to create or update the `iproj.json` file. When you define an extension in the `compiles` property, the properties of it will override the `so` defaults.

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
	/** `preCommands` do not respect the library list and is run before 'command' */
	preCommands?: string[]
	/** `command` does respect the library list */
	command?: string;
	/** Used if the commands are built up from source. Usually means `command` and `commands` is blank */
	commandSource?: boolean;
	/** `postCommands` do not respect the library list and is run after 'command' */
	postCommands?: string[]
};
```

#### Example: build object from commands

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
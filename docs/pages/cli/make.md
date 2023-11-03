This page describes information about the `makefile` that is generated for a project when using `-bf make`.

This section only applies if you use `-bf make`.

## `iproj.json` properties

Source Orbit makes use of a few properties that you can put inside your `iproj.json` file.

| Property       | Type                                                                                                                             |
| -------------- | -------------------------------------------------------------------------------------------------------------------------------- |
| `binders`      | Array of strings. Use this if you want to use export functions/procedures from other service programs outside of your repository |
| `includePaths` | Array of strings. Use this if you want to include headers that can be found in the IFS at compile time. **You do not need to list directories that are part of the repository.** |

## Binding handled automatically

If you are making use of service programs, Source Orbit will automatically maintain this for you. Your service programs will be added to the binding directory automatically and programs automatically include them. **No need to use the `BNDDIR` header** in your source code. Be careful, because changing the custom compile commands from the default may break this functionality. **You should still create binder source for all your service programs** in order for Source Orbit to create them automatically.

## Changing compile options

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
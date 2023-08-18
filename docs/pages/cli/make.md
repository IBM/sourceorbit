This page describes information about the `makefile` that is generated for a project when using `-bf make`.

This section only applies if you use `-bf make`.

## Binding handled automatically

If you are making use of service programs, Source Orbit will automatically maintain this for you. Service programs will be added to the binding directory automatically and programs automatically include them. No need to use the `BNDDIR` header. Be careful, because changing the custom compile commands from the default may break this functionality.

## Changing compile options

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
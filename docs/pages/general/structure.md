Like most IBM i projects, including those residing in `QSYS`, we recommend you split up your source code using directories. Except, when using git, you can have multiple levels of directories.

## Directory names

While projects can continue to use the traditional `QXSRC` source names (`QRPGLESRC`, `QCLLESRC`, etc), you can now use different, simpler names.

## Lowercase file names

It is recommend to always use lowercase file names. Generally, it is easier on the eyes and follows the standard of most other environments. You could also consider using camelCase.

* `ord500.pgm.sqlrpgle`
* `ord600.pgm.cbble`
* `qrpglesrc/faq500.rpgle`

## Valid extensions

The extensions used for your source can generally follow suit from member attributes (e.g. `.rpgle`, `.sqlrpgle`, `.cbble`, `.clle`, etc).

But, it is encouraged to use an additional extension to identify that your source is a program or a module in addition to the regular extension.

* `sale10.rpgle` indicates a module (which could become a service program, or part of a multi-module program)
* `ord600.pgm.cbble` indicates that is source should become a program

## Includes and copybooks

It is recommended that all includes (also referred to as copybooks or headers) for RPGLE and COBOL use the extension of `.xxxinc`.

For example:

* `ordsrv.rpgleinc` is a RPGLE include.
* `pwrdta.cbbleinc` is a COBOL include.

For C and C++, you should continue to use the standard `.h` for header files.

## Include statements (RPGLE)

When using `/COPY` and `/INCLUDE` in RPGLE with a local project, the path should always be relative to the project directory and not relative to the active file. Generally, the more explicit you can be, the easier it will be the maintain long term.

* Works: `/copy 'faq500.rpgleinc'`,
* **Improved**: `/copy 'qrpgleref/faq500.rpgleinc'`

While it is possible to use `INCDIR` and then not provide a directory on the include statement, when reading the code, it is a lot clearer where the file is coming from.

If you want your local project to resolve files on the IFS, make sure you specify your 'include directories' 

* by using the `INCDIR` parameter available on most ILE compilers,
* or inside the [`iproj.json` file with the `includePath` property](https://ibm.github.io/vscode-ibmi-projectexplorer/#/pages/ibm-i-projects/iproj-json?id=includepath) to the paths where the compiler should look up (this is supported by ibmi-bob)

## Example project structure

Check out the [company_system](https://github.com/worksofliam/company_system) repository for a good example project which follows these rules.
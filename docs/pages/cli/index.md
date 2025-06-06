# Overview

[Source Orbit](https://www.npmjs.com/package/@ibm/sourceorbit) is available as a CLI tool. It can be installed through npm onto most platforms, including IBM i.

> [!TIP]
> Use the `so --help` command anytime you would like to see the available commands and options.

## General usage

Source Orbit will scan the sources in the current working directory (CWD) and can do a few things:

* Show object dependencies with `-l`
* Generate a different file formats based on the dependency tree to build the project
* Rename files easily (`.rpgle`/`.clle`/`.clp` -> `.pgm.rpgle`/`.pgm.clle`/`.clle`) with `-ar`
* Fix RPGLE includes if the source is found locally (useful for converting from member include style) with `-fi`
* List detailed information with `--verbose`
* Want to only scan a specific source? Use the `-s` option: `so -s qrpglesrc/employees.pgm.sqlrpgle`

```sh
cd myibmiproject
so -h
so -bf make && gmake BIN_LIB=libname
```

### Dependency file generation

Source Orbit CLI can generate different file formats using the `-bf <type>` parameter.

Types available:

* `json`: Generate all dependency info as JSON
* `bob`: Generate the required `Rules.mk` files for bob
   *  See the documentation [here](https://ibm.github.io/ibmi-bob/#/prepare-the-project/rules.mk) on defining targets using Rules.mk in the Bob documentation.
* `make`: Generates a single makefile with the targets and rules
   *  [See more here](./pages/cli/make.md) for our make support.
* `imd`: Generate analysis reports for branches
   * This is particularly useful for pull-requests. It is possible have a pipeline that runs on a push to a branch/PR to generate dependency information.
   * [See more here](./pages/cli/gha.md) about usage with GitHub Actions.

```sh
so -bf json
so -bf imd -f `git diff --name-only origin/main origin/${GITHUB_HEAD_REF}`
```

### Specific files

* You can use `-f` to make Source Orbit to only care about specific sources. This still scans the entire project, so will know about object parents.
* Using `-s` means only a specific files and children of that file/object. This will not scan the entire project. **This option is deprecated and will likely be removed.**

### Cleanup capabilities

The ability to cleanup your source code is usually a one-and-done situation. After you've migrated your source code into git, you may find that a majority of your source extensions are incorrect (like missing the `.pgm` attribute on your source). There are currently two methods of cleanup available:

* `-ar` for **auto-rename**. This fixes most extensions for your repository. For example, adds the `.pgm` attribute where possible, changes RPGLE headers to use `.rpgleinc` and fixes SQL sources to use right extension based on the `CREATE` statement inside of it.
* `-fi` for **fix includes**. This will scan all RPGLE source code and change the include statements to use the unix style path if the mapped source member can be found in the current working directory.

You can also use `--verbose` before using those parameters to see impact it will have before running them. To follow up to that, consider using git before using this parameters.

```sh
cd myrpgrepo

# Preview warnings and infos
so --verbose

# Fix names
so -ar

# Preview warnings and infos. Will be different since file names changed
so --verbose

# Fix names
so -fi
```

### Branch library

When building in pipelines, sometimes you need a deterministic library name for a branch to build in. Use `-bl` (branch library) for to get one:

```sh
# so -bl <branch/name>

$ so -bl feature/5353_new_thing
VS6541B2A1

$ export BIN_LIB=$(so -bl feature/5353_new_thing)
$ echo $BIN_LIB                                 
VS6541B2A1
```
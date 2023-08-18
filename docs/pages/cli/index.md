Source Orbit is available as a CLI tool. It can be installed through npm onto most platforms, including IBM i.

## General usage

Source Orbit will scan the sources in the current working directory (CWD) and can do a few things:

* Show object dependencies with `-l`
* Generate a different file formats based on the dependency tree to build the project
* Rename files easily (`.rpgle`/`.clle`/`.clp` -> `.pgm.rpgle`/`.pgm.clle`/`.clle`) with `-ar`
* Fix RPGLE includes if the source is found locally (useful for converting from member include style) with `-fi`
* List all project objects with `--verbose`

```sh
cd myibmiproject
so -h
so -bf make && gmake BIN_LIB=libname
```

## Dependency file generation

Source Orbit CLI can generate different file formats using the `-bf <type>` parameter.

Types available:

* `json` Generate all dependency info as JSON
* `bob` to generate the required `Rules.mk` files
* `make` generates a single makefile with the targets and rules.
   *  [See more here](./pages/cli/make) for our make support.
* `imd` can be used to generate analysis reports for branches
   * This is particularly useful for pull-requests. It is possible have a pipeline that runs on a push to a branch/PR to generate dependency information.
   * See an [example report here](https://github.com/worksofliam/ibmi-company_system-rmake/actions/runs/5765430282). See about [GitHub Actions](./gha.md) here.

```
so -bf imd -l `git diff --name-only origin/main origin/${GITHUB_HEAD_REF}`
```

## Installation

We recommend Node.js 18+.

### Most platforms

1. `npm i -g @IBM/source-orbit`
2. Use `so`

### IBM i

1. Install Node.js via `yum` and/or use `update-alternatives` to set the Node.js version.
   * `yum install nodejsxx`
   * `update-alternatives --set node /QOpenSys/pkgs/lib/nodejs18/bin/node`
2. Install Source Orbit globally onto the IBM i
   * `npm i -g @IBM/source-orbit`
3. Find where global CLI tools get installed
   * Start with `npm list -g --depth 0` to find globally installed packages
4. Update the `PATH` environment variable to include the `npm` binary directory for installed CLI packages
   * `PATH=/QOpenSys/pkgs/lib/nodejs18/bin:$PATH`
	 * put in `.bash_profile` for CLI usage, put in `.bashrc` for Code for IBM i usage 
5. Use `so`
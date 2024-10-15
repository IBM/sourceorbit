# Source Orbit

<img src="https://github.com/IBM/sourceorbit/blob/main/vs/icon.png?raw=true" align="right" width="256" height="256">

Source Orbit is a dependency management tool. As IBM i developers start using Git for their RPGLE, CL, DDS and SQL, we want to provide them with excellent tools to help them understand their source code. Source orbit is available for use as both a VS Code extension and CLI tool.

* 💻 [Install from VS Code Marketplace](https://marketplace.visualstudio.com/items?itemName=IBM.vscode-sourceorbit)
* 📦 [Download from Open VSX Registry](https://open-vsx.org/extension/IBM/vscode-sourceorbit)
* ⚡[Install CLI from NPM](https://www.npmjs.com/package/@ibm/sourceorbit)
* 📖 [View Documentation](https://ibm.github.io/sourceorbit/#/) 
* 🔎 [See Releases](https://github.com/IBM/sourceorbit/releases)

[![Extension Version](https://img.shields.io/visual-studio-marketplace/v/IBM.vscode-sourceorbit?label=Extension)](https://marketplace.visualstudio.com/items?itemName=IBM.vscode-sourceorbit)
[![Extension Installs](https://img.shields.io/visual-studio-marketplace/i/IBM.vscode-sourceorbit)](https://marketplace.visualstudio.com/items?itemName=IBM.vscode-sourceorbit)

[![NPM Version](https://img.shields.io/npm/v/@ibm/sourceorbit.svg?label=CLI)](https://www.npmjs.com/package/@ibm/sourceorbit)
[![NPM Downloads](https://img.shields.io/npm/dm/@ibm/sourceorbit.svg)](https://www.npmjs.com/package/@ibm/sourceorbit)

## General Usage

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

## Getting Started

Check out these pages to dive in:

1. [As a CLI](https://ibm.github.io/sourceorbit/#/./pages/cli/index): Run Source Orbit as part of an automated pipeline, or anywhere really!
2. [As a VS Code Extension](https://ibm.github.io/sourceorbit/#/./pages/extension/index): Leverage Source Orbit as you develop applications in VS Code
3. [Project Structure](https://ibm.github.io/sourceorbit/#/./pages/general/structure): Learn how to structure your code when stored in git
4. [Source Code Rules](https://ibm.github.io/sourceorbit/#/./pages/general/rules): Learn what rules to abide by when using Source Orbit
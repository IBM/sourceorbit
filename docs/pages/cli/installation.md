# Installation

This page describes how you can install the [@ibm/sourceorbit](https://www.npmjs.com/package/@ibm/sourceorbit) CLI tool from NPM.

## Most Platforms

1. Install Source Orbit globally
```sh
npm i -g @ibm/sourceorbit
```

2. Use `so`

> [!TIP]
> We recommend Node.js 18+.

## IBM i

1. Install Node.js via `yum` and/or use `update-alternatives` to set the Node.js version.
```sh
yum install nodejsxx
```
```sh
update-alternatives --set node /QOpenSys/pkgs/lib/nodejs18/bin/node
```

2. Install Source Orbit globally on to the IBM i
```sh
npm i -g @ibm/sourceorbit
```

3. Update the `PATH` environment variable to include the `npm` binary directory for installed CLI packages
    * `PATH=/QOpenSys/pkgs/lib/nodejs18/bin:$PATH`
    * Put in `.bash_profile` for CLI usage, put in `.bashrc` for Code for IBM i usage

4. Use `so`
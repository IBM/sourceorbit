
## What is Source Orbit

Source Orbit is a dependency management system. As IBM i developers start using git for the RPGLE, CL, DDS and SQL, we want to provide them with excellent tools to help them understand their source code. Source Orbit:

1. scans all applicable source code to build a dependency tree
2. can show how objects would be affected as developers write code (inside of Visual Studio Code)
3. can generate JSON, or build scripts, to automatically build your application changes
4. generates reports for branches being worked on so project owners can see their application in real time.

## Start using Source Orbit

Source Orbit is available in two flavours:

1. [As a CLI](./pages/cli/index.md): run Source Orbit as part of an automated pipeline, or anywhere really!
2. Inside Visual Studio Code: your devs should know how their changes will affect other objects. This will let them see it in real time
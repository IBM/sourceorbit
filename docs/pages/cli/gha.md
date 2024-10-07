# GitHub Actions

Below are two workflow examples that use [Source Orbit](https://www.npmjs.com/package/@ibm/sourceorbit) and [IBM i CI](https://www.npmjs.com/package/@ibm/ibmi-ci). They both make use of Environments which hold the connection details to the IBM i being deployed to.

Both workflows the library with the full build / latest objects is `CMPSYS`.

> [!NOTE]
> Refer to the workflow history of the [ibmi-company_system](https://github.com/IBM/ibmi-company_system/actions) project for example output of both workflows.

## Main branch change

```yaml
name: Source Orbit Impact Report and Deploy

on:
  push:
    branches:
      - main

jobs:
  ibmi-build:
    environment: REMOTEIBMI
    runs-on: ubuntu-latest
    permissions:
      packages: read
      contents: read
    steps:
      - uses: actions/checkout@v4
        with:
          fetch-depth: 0
          
      - uses: actions/setup-node@v3
        with:
          node-version: 18
        
      - run: npm i -g @ibm/sourceorbit
      - run: npm i -g @ibm/ibmi-ci

      - name: Generate makefile
        run: so -bf make

      - name: Deploy to IBM i
        run: | 
          ici \
            --cmd "mkdir -p './builds/ics_${GITHUB_HEAD_REF}'" \
            --rcwd "./builds/ics_${GITHUB_HEAD_REF}" \
            --push "." \
            --cmd "/QOpenSys/pkgs/bin/gmake BIN_LIB=CMPSYS"
        env:
          IBMI_HOST: ${{ secrets.IBMI_HOST }}
          IBMI_USER: ${{ secrets.IBMI_USER }}
          IBMI_PASSWORD: ${{ secrets.IBMI_PASSWORD }}
          IBMI_SSH_PORT: ${{ secrets.IBMI_SSH_PORT }}
```

## PR creation

This workflow also shows an example of creating an impact report for a PR and posting a comment to the PR with a link to it.

It is also using `so -bl ${GITHUB_HEAD_REF}` (`&BRANCHLIB` inside of Code for IBM i `.env`) to deploy to a library unique to this branch.

```yaml
name: Source Orbit Impact Report

on:
  pull_request: 
    types: [opened]

jobs:
  so-impact:
    environment: REMOTEIBMI
    runs-on: ubuntu-latest
    permissions:
      issues: write
      pull-requests: write
      contents: read
    steps:
      - uses: actions/checkout@v4
        with:
          fetch-depth: 0
          
      - uses: actions/setup-node@v3
        with:
          node-version: 18
        
      - run: npm i -g @ibm/sourceorbit
      - run: npm i -g @ibm/ibmi-ci

      - name: Generate impact information
        run: so -bf imd -l `git diff --name-only origin/main origin/${GITHUB_HEAD_REF}`

      - name: Adding markdown
        run: cat impact.md >> $GITHUB_STEP_SUMMARY

      - name: Post comment
        uses: actions/github-script@v5
        with:
          script: |
            github.rest.issues.createComment({
              issue_number: context.issue.number,
              owner: context.repo.owner,
              repo: context.repo.repo,
              body: '👋  A new change report is available based on this PR being created.\n\n[See summary here.](https://github.com/' + context.repo.owner + '/' + context.repo.repo + '/actions/runs/${{ github.run_id }})'
            })

      - name: Generate makefile
        run: so -bf make -l `git diff --name-only origin/main origin/${GITHUB_HEAD_REF}`

      - name: Deploy to IBM i
        run: | 
          ici \
            --cmd "mkdir -p './builds/ics_${GITHUB_HEAD_REF}'" \
            --rcwd "./builds/ics_${GITHUB_HEAD_REF}" \
            --push "." \
            --cmd "/QOpenSys/pkgs/bin/gmake LIBL='CMPSYS' BIN_LIB=$(so -bl ${GITHUB_HEAD_REF})"
        env:
          IBMI_HOST: ${{ secrets.IBMI_HOST }}
          IBMI_USER: ${{ secrets.IBMI_USER }}
          IBMI_PASSWORD: ${{ secrets.IBMI_PASSWORD }}
          IBMI_SSH_PORT: ${{ secrets.IBMI_SSH_PORT }}
```
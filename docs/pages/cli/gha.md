This GitHub Action will run Source Orbit when a PR is created or updated to generate a report based on the changes in the PR. [See an example here](https://github.com/worksofliam/ibmi-company_system-rmake/actions/runs/5765430282)!

```yaml
name: Impact analysis

on: pull_request

jobs:
  build:
    name: Code scan
    runs-on: ubuntu-latest

    strategy:
      matrix:
        node-version: [18.x]

    steps:
    - uses: actions/checkout@v3
      with:
        fetch-depth: 0
    - name: Create dummy package-lock
      run: echo "{}" > package-lock.json
    - name: Use Node.js ${{ matrix.node-version }}
      uses: actions/setup-node@v3
      with:
        node-version: ${{ matrix.node-version }}
        cache: 'npm'
        
    - run: npm i -g @IBM/sourceorbit
    - run: so -bf imd -l `git diff --name-only origin/main origin/${GITHUB_HEAD_REF}`
    - name: Adding markdown
      run: cat impact.md >> $GITHUB_STEP_SUMMARY
```
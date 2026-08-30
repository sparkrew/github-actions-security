# Scanner commands

This file lists the command used to run each scanner in the study.

| Scanner | Command |
| --- | --- |
| `actionlint` | `./actionlint .github/workflows/<workflow-file>.yml` |
| `frizbee` | `./frizbee actions` |
| `ggshield` | `./ggshield secret scan --all-secrets path .github/workflows/ --recursive` |
| `pinny` | `./pinny actions pin` |
| `poutine` | `./poutine analyze_local .` |
| `scharf` | `./scharf audit .` |
| `scorecard` | `./scorecard --local=<TEMP_REPO_DIR> --show-details` |
| `semgrep` | `semgrep --config p/github-actions .github/workflows/<workflow-file>.yml` |
| `zizmor` | `./zizmor .github/workflows/<workflow-file>.yml` |
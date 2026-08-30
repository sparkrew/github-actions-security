# Maintainer validation

This document records the template used to contact scanner maintainers for validating the rule-to-weakness mapping used in the paper.

We contacted the maintainers of the studied GitHub Actions workflow security scanners and asked them to review:

- the mapping between their scanner rules and our weakness classes;
- the definition of the 10 general weakness classes.



## Email template

The following template was sent to each scanner's maintainers, with `<scanner>` replaced by the tool name.

---

Dear \<scanner\> maintainer,

My name is Madjda Fares. I am a PhD student in software engineering at Université de Montréal. Together with my colleagues Yogya Gamage and Benoit Baudry, we are conducting a study comparing security scanners for GitHub Actions workflows. The first version of our paper is available on arXiv: [Unpacking Security Scanners for GitHub Actions Workflows](https://arxiv.org/abs/2601.14455).

In order to compare the features of different scanners, we have come up with a set of 10 general weaknesses. Then, we have mapped the specific rules of each scanner to these general weaknesses.

We have mapped the rules of \<scanner\> as follows: [`<scanner>.md`](./<scanner>.md)

We also provide a detailed description of the 10 weaknesses: [`weaknesses.md`](./weaknesses.md)

We would be grateful if you could let us know whether you agree with our classification of \<scanner\>'s rules and if you can provide feedback on the definition of the general weaknesses.

Thank you.

Best regards,
Madjda Fares, on behalf of Yogya Gamage and Benoit Baudry

---


## Weakness classes

The 10 weakness classes used in the study:

1. Artifact Integrity Weakness (AIW)
2. Control Flow Weakness (CFW)
3. Excessive Permission Weakness (EPW)
4. GitHub Runner Compatibility Weakness (GRCW)
5. Hardening Gap Weakness (HGW)
6. Injection Weakness (IW)
7. Known Vulnerable Component Weakness (KVCW)
8. Privileged Trigger Weakness (PTW)
9. Secrets Exposure Weakness (SEW)
10. Unpinned Dependency Weakness (UDW)

Full definitions are in [`weaknesses.md`](./weaknesses.md).

## Scanner-specific validation files

Each scanner's rule-to-weakness mapping is maintained as a separate file in this directory.

- `actionlint`: rule mapping in [`actionlint.md`](./actionlint.md). Command: `./actionlint .github/workflows/<workflow-file>.yml`
- `frizbee`: rule mapping in [`frizbee.md`](./frizbee.md). Command: `./frizbee actions`
- `ggshield`: rule mapping in [`ggshield.md`](./ggshield.md). Command: `./ggshield secret scan --all-secrets path .github/workflows/ --recursive`
- `pinny`: rule mapping in [`pinny.md`](./pinny.md). Command: `./pinny actions pin`
- `poutine`: rule mapping in [`poutine.md`](./poutine.md). Command: `./poutine analyze_local .`
- `scharf`: rule mapping in [`scharf.md`](./scharf.md). Command: `./scharf audit .`
- `scorecard`: rule mapping in [`scorecard.md`](./scorecard.md). Command: `./scorecard --local=<TEMP_REPO_DIR> --show-details`
- `semgrep`: rule mapping in [`semgrep.md`](./semgrep.md). Command: `semgrep --config p/github-actions .github/workflows/<workflow-file>.yml`
- `zizmor`: rule mapping in [`zizmor.md`](./zizmor.md). Command: `./zizmor .github/workflows/<workflow-file>.yml`

The consolidated rule mapping across all scanners is in [`rules_mapping.csv`](./rules_mapping.csv).

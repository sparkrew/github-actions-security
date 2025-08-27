# GitHub Actions Security Analysis

## Overview
This repository hosts a systematic, comparative study of security risks in GitHub Actions (GHA) workflows. It contains curated datasets of real workflow files, a normalized mapping of tool capabilities to issue categories, and scripts and notebooks used to collect, analyze, and summarize results.

## Repository Structure
- **`dataset/`** and **`datasets/`**: raw GHA workflow files used as data, kept here so they are not executed by this repository. Paths mirror the original repos, for example `datasets/<owner>/<repo>/.github/workflows/<file>.yml`.
- **`capabilities/`**: rule and capability metadata.
  - `group_tool_capabilities.csv`: grouped high level capabilities per tool.
  - `rules_mapping.csv`: mapping from tool rule identifiers to the study taxonomy.
- **`tools/`**: scripts or wrappers for running scanners and utilities used in the study.
- **`scripts/`**: Jupyter notebooks that orchestrate collection and analysis.
  - `fetch_workflows.ipynb`, `run_tools.ipynb`, `results.ipynb`.
- **`tools_output/`**: selected outputs only.
  - `*/workflow_with_issues/`: per tool, only workflows where issues were found.
- **`biblio.md`**: annotated bibliography and links.
- **`tools.csv`**: list of scanners considered in the study.

> Safety note: workflow files are treated as data, not automation. They live under `dataset/` or `datasets/` so that GitHub does not run them for this repository.

## Study Objective
Identify and compare security risks in GitHub Actions workflows, with focus on:
- Common misconfigurations and weaknesses in CI contexts (privilege scope, untrusted inputs, caches, dependency and action trust, secret handling).
- Practical mitigation strategies and recommended defaults.
- A capability based comparison of open source scanners for GHA.

## Quick Start
```bash
git clone https://github.com/Madjda32-del/github-actions-security.git
cd github-actions-security
# Optional environment
# python -m venv .venv && source .venv/bin/activate
# pip install -r requirements.txt
# Open notebooks in your environment:
# jupyter lab scripts/fetch_workflows.ipynb

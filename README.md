# GitHub Actions Security

## Overview

This repository contains the reproducibility package for the paper
[Unpacking Security Scanners for GitHub Actions Workflows](https://arxiv.org/abs/2601.14455).

It includes the dataset, scanner outputs, processing scripts, normalized results, and artifacts used to compare GitHub Actions workflow security scanners.

## Repository structure

- `dataset/` — workflow metadata and dataset manifests (`workflow_list.csv`, `workflow_metadata.csv`).
- `workflows/` — collected GitHub Actions workflow files used as data. Paths mirror the original repositories. Stored here so GitHub does not execute them.
- `scanners/` — scanner binaries and local installations used in the study (one subdirectory per tool).
- `scanners_output/` — raw output produced by each scanner (one subdirectory per tool).
- `scanners_under_study.csv` — full list of scanners considered in the study with repository and source links.
- `scripts/` — Jupyter notebooks for data collection and analysis (`fetch_workflows.ipynb`, `run_tools.ipynb`, `results.ipynb`, `execution_time.ipynb`).
- `results/` — summary CSV files and execution time measurements used in the paper (`coverage_matrix.csv`, `detection_volume_matrix.csv`, `tools_findings_summary.csv`, `execution_time/`).
- `weakness/` — weakness taxonomy, per-scanner rule-to-weakness mappings, and maintainer validation documentation.
- `biblio.md` — annotated bibliography of related work.

## Reproducing the results

The main notebooks are in `scripts/`.

Run them in this order:

1. `fetch_workflows.ipynb` collects the workflow dataset.
2. `run_tools.ipynb` runs the scanners on the collected workflows.
3. `results.ipynb` normalizes scanner outputs and generates the detection matrices.
4. `execution_time.ipynb` processes runtime measurements.

Generated outputs are stored in `results/`.

## Notes

The collected workflows are stored under `workflows/` instead of `.github/workflows/` to prevent GitHub from executing them in this repository.

Scanner outputs are kept raw in `scanners_output/`; processed and normalized results are stored in `results/normalized_workflows/`.
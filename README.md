# GitHub Actions Security

## Overview

This repository contains the reproducibility package for the paper
[Unpacking Security Scanners for GitHub Actions Workflows](https://arxiv.org/abs/2601.14455).

It includes scanner outputs, processing scripts, normalized results, and artifacts used to compare GitHub Actions workflow security scanners.

The 2,722 collected workflows are stored separately:

Dataset: https://doi.org/10.5281/zenodo.23129390

## Repository structure

- `dataset/` — workflow metadata and dataset manifests (`workflow_list.csv`, `workflow_metadata.csv`, `workflow_diversity_features.csv`).
- `scanners/` — scanner binaries and local installations used in the study (one subdirectory per tool).
- `scanners_output/` — raw output produced by each scanner (one subdirectory per tool).
- `scanners_under_study.csv` — full list of scanners considered in the study with repository and source links.
- `scripts/` — Jupyter notebooks for data collection and analysis (`fetch_workflows.ipynb`, `run_tools.ipynb`, `results.ipynb`, `execution_time.ipynb`, `accuracy_sampling.ipynb`, `accuracy.ipynb`).
- `ground-truth/` — manual labels and per-scanner labels for the 54-workflow accuracy sample (`manual_label.csv`, `tools_label/`).
- `results/` — summary CSV files, execution time measurements, and accuracy metrics (`coverage_matrix.csv`, `detection_volume_matrix.csv`, `tools_findings_summary.csv`, `accuracy.csv`, `execution_time/`).
- `weakness/` — weakness taxonomy, per-scanner rule-to-weakness mappings, and maintainer validation documentation.
- `capabilities/rules_map.csv` — rule-to-weakness mapping used by the analysis notebooks.

## Reproducing the results

Unpack the Zenodo archive and set `DATASET_DIR` in the first cell of each notebook that needs the workflows. The archive contains `workflows/`, `normalized_workflows/`, and `54_workflows_to_label/`.

The main notebooks are in `scripts/`.

Run them in this order:

1. `fetch_workflows.ipynb` collects the workflow dataset.
2. `run_tools.ipynb` runs the scanners on the collected workflows.
3. `results.ipynb` normalizes scanner outputs and generates the detection matrices.
4. `execution_time.ipynb` processes runtime measurements.
5. `accuracy_sampling.ipynb` documents the 54-workflow sample.
6. `accuracy.ipynb` compares `ground-truth/tools_label/` with `ground-truth/manual_label.csv` and writes `results/accuracy.csv`.

Generated outputs are stored in `results/` and `analyze/`.

## Notes

The collected workflows are not stored in this GitHub repository, so GitHub does not execute them here.

Scanner outputs are kept raw in `scanners_output/`.

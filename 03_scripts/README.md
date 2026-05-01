# Scripts

This folder contains the script files underlying the published paper on ICT-related, contextual, and behavioural predictors of student achievement in Thailand using PISA 2022.

## Subfolders

- `01_preparation/`  
  Selected preparation scripts used to produce the final analysis-ready dataset for the published random forest study.

- `02_random_forests/`  
  The implemented random forest scripts used in the paper across global, in-school, out-of-school, PISA-based, and MLFTAU-based model specifications.

- `03_synthesis/`  
  The synthesis script used to consolidate random forest outputs and generate the higher-level synthesis files underpinning the paper’s main interpretive claims.
  
- `04_validation/`
  Validation scripts supporting the manuscript’s predictive performance checks, rank-stability checks, sensitivity checks, correlation diagnostics, and validation summary tables.

## Important note

The scripts included here are the actual scripts used in the published workflow, shared with only minimal safe edits for public release. These edits are limited to improving headers, removing sensitive comments where necessary, and replacing private local paths with clearly marked placeholder paths. The implemented analytical logic has not been rewritten into new untested public-facing scripts.

These materials are provided to support transparency and auditability for the published paper. They should not be interpreted as a fully self-contained end-to-end reproduction package.
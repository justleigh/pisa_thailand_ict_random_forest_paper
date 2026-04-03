# PISA 2022 Thailand ICT Random Forest Paper Repository

This repository provides public-facing materials supporting the paper:

**Understanding ICT-Related Drivers of Student Achievement in Thailand: A Cross-Framework Random Forest Analysis of PISA 2022**

## Repository purpose

This repository is intended to support **transparency and auditability** for the published paper. It contains selected scripts, public-facing metadata, and selected output files directly relevant to the article’s reported findings.

The repository is **not** a fully self-contained end-to-end reproduction package. Some private intermediate datasets, detailed framework-mapping resources, and broader internal workflow materials used in the larger PhD project are not publicly released here.

## What the paper does

The paper examines how ICT-related, contextual, and behavioural factors are associated with student achievement in Thailand using the Thailand subset of **PISA 2022**. The analysis uses a **cross-framework predictive modelling design** built around random forest models and a higher-level synthesis of variable-importance patterns across:

- global models
- in-school models
- out-of-school models
- PISA-based framework partitions
- MLFTAU-based framework partitions

The paper interprets ICT-related predictors as contextually embedded within broader learning environments rather than as isolated drivers of achievement.

## What is included in this repository

### `01_data_access/`
Documentation describing the input data requirements for the public materials.

### `02_metadata_public/`
A reduced public-facing inventory of variables used in the published analysis.

### `03_scripts/`
The actual scripts used in the published workflow, shared with only minimal safe edits for public release.

This includes:
- selected preparation scripts
- random forest scripts
- the synthesis script used to consolidate the random forest outputs

The analytical logic of these scripts has **not** been rewritten into new untested public-facing versions.

### `04_outputs_public/`
Selected paper-facing outputs directly relevant to the article.

This includes:
- three global top-20 random forest tables
- four key synthesis outputs underpinning the paper’s main interpretive claims

### `05_docs/`
Supporting documentation describing:
- the scope of the repository
- the broad analytical workflow underlying the paper

### `06_paper/`
Brief manuscript-related information, including the paper title and abstract.

## What is not included

This repository does **not** include:

- the full private variable-mapping table
- detailed framework assignment files
- private cleaned or imputed datasets
- private intermediate data products
- internal tracking files or draft materials
- the broader inferential modelling pipeline planned for later stages of the PhD project

## Reproducibility note

This repository should be understood primarily as a **transparency resource** rather than as a turnkey reproducibility archive. The included scripts reflect the implemented workflow used in the study, but some of them depend on intermediate data objects, local adaptations, or project-specific metadata resources that are not publicly released. As a result, users seeking to adapt or rerun the workflow may need to modify paths and supply their own locally available inputs.

## Data source

The published analysis is based on the Thailand subset of **PISA 2022**. Raw PISA data are not redistributed in this repository. Users wishing to inspect or adapt the workflow should obtain the relevant source data from the appropriate official OECD/PISA source.

## Citation

A full paper citation will be added here once publication details are available.

If you use this repository, please cite both:
1. the published paper, once available
2. this repository, if a repository citation or DOI is later added

## Collaboration note

This repository is a public-facing paper-specific subset of a broader research programme on ICT and educational outcomes using PISA data. The wider project includes a substantial variable-mapping and workflow design process that is not fully released here.

Scholars interested in applying a similar cross-framework analytical approach in other national contexts are welcome to get in touch. Possible areas for collaboration include:
- country-specific replication studies
- cross-country comparison of ICT-related predictors of student achievement
- extension of the workflow to topics such as digital inequality, student well-being, self-efficacy, and learning during COVID-19 school closures

Please contact Leigh Pearson if you are interested in discussing potential collaboration.

## Contact / status

This repository is a paper-specific public subset of a broader PhD research programme on ICT and educational outcomes in Thailand.
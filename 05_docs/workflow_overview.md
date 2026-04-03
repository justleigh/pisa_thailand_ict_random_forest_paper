# Workflow overview

This document summarises the analytical workflow underlying the published paper on ICT-related, contextual, and behavioural predictors of student achievement in Thailand using PISA 2022.

## 1. Data source

The published analysis is based on the Thailand subset of PISA 2022. The study uses cognitive outcomes in reading, mathematics, and science together with contextual information drawn from the student questionnaire, ICT familiarity questionnaire, and school questionnaire.

The repository does not redistribute the original PISA data. Users wishing to inspect or adapt the workflow should obtain the relevant source data from the appropriate official OECD/PISA source.

## 2. Data preparation

The broader project involved extensive data cleaning, transformation, and preparation prior to modelling. For the purposes of this public repository, selected preparation scripts are included to document the main stages that led to the analysis-ready dataset used in the published paper.

These preparation stages include:

- cleaning of student questionnaire variables
- cleaning of ICT questionnaire variables
- cleaning of student and ICT derived variables
- preparation of school questionnaire and school derived-variable inputs
- final imputation and assembly of the analysis dataset used for random forest modelling

The preparation scripts included in this repository reflect the implemented workflow used in the published analysis, but the full private preparation infrastructure is not publicly released.

## 3. Variable organisation

The published paper organised predictors using three complementary analytical structures:

- the PISA Questionnaire Framework
- the PISA ICT Framework
- the adapted Multi-Level Framework of Technology Acceptance and Use (MLFTAU)

Variables were also classified by broad learning context in order to support global, in-school, and out-of-school modelling. A reduced public-facing variable inventory is included in the repository to support transparency regarding the variables used in the paper.

## 4. Random forest modelling

The article uses random forest models as a predictive screening approach rather than as a causal modelling strategy. Models were estimated separately for reading, mathematics, and science and were organised across multiple analytical dimensions, including:

- global models
- in-school models
- out-of-school models
- PISA-based model partitions
- MLFTAU-based model partitions
- domain-level and construct-level specifications

The scripts included in the repository reflect the modelling workflow used in the paper.

## 5. Synthesis

The published article reports a selective synthesis from a broader modelling architecture. Rather than presenting every model output in full, the paper focuses on three recurring forms of variation in predictor salience:

- convergence across model specifications
- context shifts between in-school and out-of-school learning environments
- framework-sensitive variation across analytical frameworks

Selected synthesis outputs are included in the repository because they directly underpin the paper’s main reported patterns.

## 6. What the public materials support

The public materials are intended to support:

- inspection of the implemented workflow behind the published paper
- understanding of the preparation, modelling, and synthesis stages
- review of selected outputs underpinning the article’s main claims
- auditability of the logic connecting scripts, outputs, and published findings

The repository should therefore be read as a paper-specific transparency resource built around the published random forest study.
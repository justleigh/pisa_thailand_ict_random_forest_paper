# --------------------------------------
# Random Forest Validation Script 01:
# Predictive Performance Checks for Global and Selected Framework RF Models
# --------------------------------------
#
# Purpose:
# This standalone validation script assesses the predictive performance of the
# main random forest (RF) model families used in the exploratory RF analysis.
# It includes both the global baseline models and a purposively selected subset
# of high-salience framework-based domain and construct slices.
#
# The script evaluates nine model families:
#
#   A. Global baseline validation models:
#      1. global_all_predictors
#      2. global_in_school
#      3. global_out_of_school
#
#   B. Selected domain-level framework validation models:
#      4. pisa_inschool_process_domain
#      5. pisa_outschool_process_domain
#      6. mlftau_inschool_individual_domain
#      7. mlftau_inschool_organisation_school_domain
#
#   C. Selected construct-level spot checks:
#      8. mlftau_inschool_individual_user_attributes_construct
#      9. pisa_outschool_process_self_directed_ict_learning_construct
#
# Rationale for selected framework models:
# The framework-level checks were selected because they represent the main
# interpretive pillars of the study:
#   - in-school learning processes;
#   - out-of-school ICT use and self-directed digital learning;
#   - individual learner dispositions and user attributes;
#   - school-level facilitating conditions, resources, and organisational support.
#
# These checks are not intended to exhaustively validate every framework domain
# or construct slice. Instead, they assess whether the principal framework-based
# evidence used in the manuscript retains sufficient predictive signal for
# exploratory interpretation.
#
# For each model family, the script runs RF models for:
#   - Mathematics: PV1MATH to PV10MATH
#   - Reading:     PV1READ to PV10READ
#   - Science:     PV1SCIE to PV10SCIE
#
# This produces:
#   9 model families × 3 outcomes × 10 plausible values = 270 RF models
#
# The script mirrors the main RF modelling pipeline by:
#   - using the same cleaned dataset:
#       data/private/processed/2022/pisa2022_cleaned_19_composites_added.csv
#   - using the same variable mapping table:
#       data/private/metadata/2022/pisa2022_variable_mapping_table_final.csv
#   - excluding students with two or more response-quality flags;
#   - excluding COVID / Global Crises variables;
#   - excluding input variables unless they are derived/composite variables;
#   - excluding PVs, weights, WLEs, identifiers, sampling/admin variables,
#     and test-effort variables as predictors;
#   - using W_FSTUWT for weighted bootstrap resampling;
#   - sampling 5,000 cases with replacement for each plausible-value model;
#   - using the original RF specification:
#       randomForest(..., importance = TRUE)
#       ntree = default randomForest value, i.e. 500
#       mtry = default randomForest value.
#
# Outputs:
#
#   1. PV-level performance diagnostics:
#      output/private/exploratory_data_analysis/2022/random_forests/
#      rf_validation_outputs/performance_checks/
#      rf_validation_performance_by_pv.csv
#
#   2. Outcome-level summary table:
#      output/private/exploratory_data_analysis/2022/random_forests/
#      rf_validation_outputs/performance_checks/
#      rf_validation_performance_summary_by_outcome.csv
#
#   3. Model-family-level summary table:
#      output/private/exploratory_data_analysis/2022/random_forests/
#      rf_validation_outputs/performance_checks/
#      rf_validation_performance_summary_by_model_family.csv
#
#   4. Validation-level summary table:
#      output/private/exploratory_data_analysis/2022/random_forests/
#      rf_validation_outputs/performance_checks/
#      rf_validation_performance_summary_by_validation_level.csv
#
# Key metrics:
#   - OOB MSE
#   - OOB RMSE
#   - OOB R²
#   - number of predictors
#   - number of available cases before sampling
#   - number of sampled cases
#   - ntree
#   - mtry used
#
# Interpretation:
# These diagnostics are intended to show whether the global and selected
# framework-based RF models contain sufficient predictive signal to justify using
# variable-importance rankings as exploratory screening evidence. They should not
# be interpreted as design-based population estimates, confirmatory hypothesis
# tests, effect sizes, or causal evidence.
#
# Methodological note:
# OOB estimates provide internal validation for the bootstrap RF models. They do
# not replace PISA replicate-weight inference and do not fully account for the
# complex survey design. The validation results should therefore be interpreted
# as predictive diagnostics for exploratory modelling rather than as population-
# level inferential statistics.
#
# --------------------------------------

# --------------------------------------
# 1. Load libraries
# --------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(randomForest)
  library(tibble)
  library(stringr)
  library(purrr)
  library(glue)
  library(fs)
})


# --------------------------------------
# 2. Manual parameters
# --------------------------------------

input_data <- "data/private/processed/2022/pisa2022_cleaned_19_composites_added.csv"
mapping_table_path <- "data/private/metadata/2022/pisa2022_variable_mapping_table_final.csv"

output_dir <- "output/private/exploratory_data_analysis/2022/random_forests/rf_validation_outputs/performance_checks"

dir_create(output_dir)

set.seed(1234)

outcomes <- c("MATH", "READ", "SCIE")
weight_var <- "W_FSTUWT"
sample_size_per_pv <- 5000


# --------------------------------------
# 3. Load data and mapping table
# --------------------------------------

df <- read_csv(input_data, show_col_types = FALSE)
mapping <- read_csv(mapping_table_path, show_col_types = FALSE)

cat("✅ Data loaded:", nrow(df), "rows and", ncol(df), "columns.\n")
cat("✅ Mapping table loaded:", nrow(mapping), "rows.\n")


# --------------------------------------
# 4. Exclude students with two or more quality flags
# --------------------------------------

flag_vars <- c(
  "perception_straight_line_flag",
  "feeling_straight_line_flag",
  "persistence_straight_line_flag",
  "math_class_periods_flag",
  "total_class_periods_flag"
)

missing_flags <- setdiff(flag_vars, colnames(df))

if (length(missing_flags) > 0) {
  stop("Missing flag columns: ", paste(missing_flags, collapse = ", "))
}

n_before_flags <- nrow(df)

df <- df %>%
  mutate(
    n_flags = rowSums(
      across(all_of(flag_vars), ~ . == "Flagged" & !is.na(.))
    )
  ) %>%
  filter(n_flags < 2) %>%
  select(-n_flags)

n_after_flags <- nrow(df)

cat("⚠️ Students removed due to ≥2 flags:", n_before_flags - n_after_flags, "\n")
cat("✅ Remaining sample size after flag exclusion:", n_after_flags, "\n")


# --------------------------------------
# 5. Define exclusion patterns
# --------------------------------------

exclude_patterns <- c(
  "^PV\\d",
  "^W_",
  "WVARSTRR",
  "SENWT",
  "VER_DAT",
  "^country_id$",
  "^school_id$",
  "^student_id$",
  "^assessment_cycle$",
  "^sampling_stratum$",
  "^subnational_region$",
  "^administration_mode$",
  "^st_effort_invested_pisa$",
  "^st_effort_invested_marks_pisa$",
  "^st_effort_accurate_pisa$",
  "^stdv_test_effort_actual$",
  "^stdv_test_effort_hypothetical$"
)

exclude_regex <- paste(exclude_patterns, collapse = "|")


# --------------------------------------
# 6. Validate required columns
# --------------------------------------

required_mapping_cols <- c(
  "renamed_variable",
  "pisa_construct",
  "input_variable",
  "derived_variable",
  "status",
  "broad_learning_context",
  "pisa_domain",
  "mlftau_domain",
  "mlftau_construct",
  "pisa_ict_home_domain",
  "pisa_ict_home_construct"
)

missing_mapping_cols <- setdiff(required_mapping_cols, names(mapping))

if (length(missing_mapping_cols) > 0) {
  stop(
    "Mapping table is missing required columns: ",
    paste(missing_mapping_cols, collapse = ", ")
  )
}

if (!weight_var %in% names(df)) {
  stop("Weight variable not found in dataset: ", weight_var)
}


# --------------------------------------
# 7. Predictor-selection functions
# --------------------------------------

select_global_all_predictors <- function(mapping, df) {
  mapping %>%
    filter(
      !str_detect(pisa_construct, regex("\\bGlobal Crises\\b", ignore_case = TRUE)),
      input_variable == "No" | derived_variable == "Yes",
      status == "Included"
    ) %>%
    pull(renamed_variable) %>%
    intersect(names(df)) %>%
    discard(~ str_detect(.x, exclude_regex))
}

select_global_in_school_predictors <- function(mapping, df) {
  mapping %>%
    filter(
      !str_detect(pisa_construct, regex("\\bGlobal Crises\\b", ignore_case = TRUE)),
      input_variable == "No" | derived_variable == "Yes",
      status == "Included",
      broad_learning_context %in% c("In School", "In and Out")
    ) %>%
    pull(renamed_variable) %>%
    intersect(names(df)) %>%
    discard(~ str_detect(.x, exclude_regex))
}

select_global_out_of_school_predictors <- function(mapping, df) {
  mapping %>%
    filter(
      !str_detect(pisa_construct, regex("\\bGlobal Crises\\b", ignore_case = TRUE)),
      input_variable == "No" | derived_variable == "Yes",
      status == "Included",
      broad_learning_context %in% c("Out of School", "In and Out")
    ) %>%
    pull(renamed_variable) %>%
    intersect(names(df)) %>%
    discard(~ str_detect(.x, exclude_regex))
}

# --------------------------------------
# Framework-slice predictor-selection helpers
# --------------------------------------

has_label <- function(x, target_label) {
  str_detect(
    string = coalesce(as.character(x), ""),
    pattern = regex(paste0("(^|;)\\s*", target_label, "\\s*(;|$)"), ignore_case = TRUE)
  )
}

select_framework_slice_predictors <- function(
    mapping,
    df,
    learning_contexts,
    domain_column,
    domain_label,
    construct_column = NULL,
    construct_label = NULL
) {
  
  mapping_filtered <- mapping %>%
    filter(
      !str_detect(pisa_construct, regex("\\bGlobal Crises\\b", ignore_case = TRUE)),
      input_variable == "No" | derived_variable == "Yes",
      status == "Included",
      broad_learning_context %in% learning_contexts,
      has_label(.data[[domain_column]], domain_label)
    )
  
  if (!is.null(construct_column) && !is.null(construct_label)) {
    mapping_filtered <- mapping_filtered %>%
      filter(has_label(.data[[construct_column]], construct_label))
  }
  
  mapping_filtered %>%
    pull(renamed_variable) %>%
    intersect(names(df)) %>%
    discard(~ str_detect(.x, exclude_regex))
}

# --- Selected PISA domain-level framework slices ---

select_pisa_inschool_process_domain_predictors <- function(mapping, df) {
  select_framework_slice_predictors(
    mapping = mapping,
    df = df,
    learning_contexts = c("In School", "In and Out"),
    domain_column = "pisa_domain",
    domain_label = "Process"
  )
}

select_pisa_outschool_process_domain_predictors <- function(mapping, df) {
  select_framework_slice_predictors(
    mapping = mapping,
    df = df,
    learning_contexts = c("Out of School", "In and Out"),
    domain_column = "pisa_domain",
    domain_label = "Process"
  )
}

# --- Selected MLFTAU domain-level framework slices ---

select_mlftau_inschool_individual_domain_predictors <- function(mapping, df) {
  select_framework_slice_predictors(
    mapping = mapping,
    df = df,
    learning_contexts = c("In School", "In and Out"),
    domain_column = "mlftau_domain",
    domain_label = "Individual"
  )
}

select_mlftau_inschool_organisation_school_domain_predictors <- function(mapping, df) {
  select_framework_slice_predictors(
    mapping = mapping,
    df = df,
    learning_contexts = c("In School", "In and Out"),
    domain_column = "mlftau_domain",
    domain_label = "Organisation/School"
  )
}

# --- Selected construct-level spot checks ---

select_mlftau_inschool_individual_user_attributes_construct_predictors <- function(mapping, df) {
  select_framework_slice_predictors(
    mapping = mapping,
    df = df,
    learning_contexts = c("In School", "In and Out"),
    domain_column = "mlftau_domain",
    domain_label = "Individual",
    construct_column = "mlftau_construct",
    construct_label = "User Attributes"
  )
}

select_pisa_outschool_process_self_directed_ict_learning_construct_predictors <- function(mapping, df) {
  mapping %>%
    filter(
      !str_detect(pisa_construct, regex("\\bGlobal Crises\\b", ignore_case = TRUE)),
      input_variable == "No" | derived_variable == "Yes",
      status == "Included",
      broad_learning_context %in% c("Out of School", "In and Out"),
      has_label(pisa_ict_home_domain, "Input") | has_label(pisa_ict_home_domain, "Process"),
      has_label(pisa_ict_home_construct, "Students' Use of ICT for Self-Directed Learning")
    ) %>%
    pull(renamed_variable) %>%
    intersect(names(df)) %>%
    discard(~ str_detect(.x, exclude_regex))
}

# --------------------------------------
# 8. Build model-family configuration
# --------------------------------------

model_families <- list(
  
  # --------------------------------------
  # A. Global baseline validation models
  # --------------------------------------
  
  global_all_predictors = list(
    model_family = "global_all_predictors",
    validation_level = "global_baseline",
    framework_group = "Global",
    framework_context = "All",
    granularity = "None",
    domain_label = "All predictors",
    construct_label = NA_character_,
    predictor_function = select_global_all_predictors,
    subset_rule = "All Included non-input predictors and derived variables, excluding Global Crises variables"
  ),
  
  global_in_school = list(
    model_family = "global_in_school",
    validation_level = "global_baseline",
    framework_group = "Global",
    framework_context = "InSchool",
    granularity = "None",
    domain_label = "In School and In and Out predictors",
    construct_label = NA_character_,
    predictor_function = select_global_in_school_predictors,
    subset_rule = "Included non-input predictors and derived variables with broad_learning_context equal to In School or In and Out"
  ),
  
  global_out_of_school = list(
    model_family = "global_out_of_school",
    validation_level = "global_baseline",
    framework_group = "Global",
    framework_context = "OutOfSchool",
    granularity = "None",
    domain_label = "Out of School and In and Out predictors",
    construct_label = NA_character_,
    predictor_function = select_global_out_of_school_predictors,
    subset_rule = "Included non-input predictors and derived variables with broad_learning_context equal to Out of School or In and Out"
  ),
  
  # --------------------------------------
  # B. Selected domain-level framework validation models
  # --------------------------------------
  
  pisa_inschool_process_domain = list(
    model_family = "pisa_inschool_process_domain",
    validation_level = "framework_domain",
    framework_group = "PISA_Contextual",
    framework_context = "InSchool",
    granularity = "Domain",
    domain_label = "Process",
    construct_label = NA_character_,
    predictor_function = select_pisa_inschool_process_domain_predictors,
    subset_rule = "PISA domain Process; broad_learning_context equal to In School or In and Out"
  ),
  
  pisa_outschool_process_domain = list(
    model_family = "pisa_outschool_process_domain",
    validation_level = "framework_domain",
    framework_group = "PISA_Contextual",
    framework_context = "OutOfSchool",
    granularity = "Domain",
    domain_label = "Process",
    construct_label = NA_character_,
    predictor_function = select_pisa_outschool_process_domain_predictors,
    subset_rule = "PISA domain Process; broad_learning_context equal to Out of School or In and Out"
  ),
  
  mlftau_inschool_individual_domain = list(
    model_family = "mlftau_inschool_individual_domain",
    validation_level = "framework_domain",
    framework_group = "MLFTAU",
    framework_context = "InSchool",
    granularity = "Domain",
    domain_label = "Individual",
    construct_label = NA_character_,
    predictor_function = select_mlftau_inschool_individual_domain_predictors,
    subset_rule = "MLFTAU domain Individual; broad_learning_context equal to In School or In and Out"
  ),
  
  mlftau_inschool_organisation_school_domain = list(
    model_family = "mlftau_inschool_organisation_school_domain",
    validation_level = "framework_domain",
    framework_group = "MLFTAU",
    framework_context = "InSchool",
    granularity = "Domain",
    domain_label = "Organisation/School",
    construct_label = NA_character_,
    predictor_function = select_mlftau_inschool_organisation_school_domain_predictors,
    subset_rule = "MLFTAU domain Organisation/School; broad_learning_context equal to In School or In and Out"
  ),
  
  # --------------------------------------
  # C. Selected construct-level spot checks
  # --------------------------------------
  
  mlftau_inschool_individual_user_attributes_construct = list(
    model_family = "mlftau_inschool_individual_user_attributes_construct",
    validation_level = "framework_construct_spot_check",
    framework_group = "MLFTAU",
    framework_context = "InSchool",
    granularity = "Construct",
    domain_label = "Individual",
    construct_label = "User Attributes",
    predictor_function = select_mlftau_inschool_individual_user_attributes_construct_predictors,
    subset_rule = "MLFTAU domain Individual and construct User Attributes; broad_learning_context equal to In School or In and Out"
  ),
  
  pisa_outschool_process_self_directed_ict_learning_construct = list(
    model_family = "pisa_outschool_process_self_directed_ict_learning_construct",
    validation_level = "framework_construct_spot_check",
    framework_group = "PISA_ICT_Home",
    framework_context = "OutOfSchool",
    granularity = "Construct",
    domain_label = "Input; Process",
    construct_label = "Students' Use of ICT for Self-Directed Learning",
    predictor_function = select_pisa_outschool_process_self_directed_ict_learning_construct_predictors,
    subset_rule = "PISA ICT home construct Students' Use of ICT for Self-Directed Learning; pisa_ict_home_domain equal to Input or Process; broad_learning_context equal to Out of School or In and Out"
  )
)

# --------------------------------------
# 9. Function to run one RF validation model
# --------------------------------------

run_rf_performance_check <- function(
    model_family,
    validation_level,
    framework_group,
    framework_context,
    granularity,
    domain_label,
    construct_label,
    subset_rule,
    pv_name,
    outcome,
    df,
    predictors,
    weight_var,
    sample_size_per_pv
) {
  
  selected_columns <- c(pv_name, predictors, weight_var)
  
  missing_selected_columns <- setdiff(selected_columns, names(df))
  
  if (length(missing_selected_columns) > 0) {
    stop(
      "Missing selected columns for ",
      model_family, " / ", pv_name, ": ",
      paste(missing_selected_columns, collapse = ", ")
    )
  }
  
  df_model <- df %>%
    select(all_of(selected_columns)) %>%
    filter(
      !is.na(.data[[pv_name]]),
      !is.na(.data[[weight_var]])
    )
  
  if (nrow(df_model) == 0) {
    stop("No valid rows available for ", model_family, " / ", pv_name)
  }
  
  df_sampled <- df_model %>%
    slice_sample(
      n = sample_size_per_pv,
      weight_by = !!sym(weight_var),
      replace = TRUE
    )
  
  rf_formula <- as.formula(paste(pv_name, "~ ."))
  
  rf_model <- randomForest(
    formula = rf_formula,
    data = df_sampled %>% select(-all_of(weight_var)),
    importance = TRUE
  )
  
  # OOB performance from randomForest regression object
  oob_mse <- tail(rf_model$mse, 1)
  oob_rmse <- sqrt(oob_mse)
  
  outcome_variance <- var(df_sampled[[pv_name]], na.rm = TRUE)
  
  oob_r2 <- ifelse(
    is.finite(outcome_variance) && outcome_variance > 0,
    1 - (oob_mse / outcome_variance),
    NA_real_
  )
  
  tibble(
    model_family = model_family,
    validation_level = validation_level,
    framework_group = framework_group,
    framework_context = framework_context,
    granularity = granularity,
    domain_label = domain_label,
    construct_label = construct_label,
    subset_rule = subset_rule,
    outcome = outcome,
    pv = pv_name,
    n_predictors = length(predictors),
    n_available_before_sampling = nrow(df_model),
    n_sampled = nrow(df_sampled),
    ntree = rf_model$ntree,
    mtry_used = rf_model$mtry,
    oob_mse = as.numeric(oob_mse),
    oob_rmse = as.numeric(oob_rmse),
    outcome_variance = as.numeric(outcome_variance),
    oob_r2 = as.numeric(oob_r2)
  )
}


# --------------------------------------
# 10. Main validation loop
# --------------------------------------

all_performance_results <- list()

counter <- 1

for (family_name in names(model_families)) {
  
  family_config <- model_families[[family_name]]
  
  model_family <- family_config$model_family
  subset_rule <- family_config$subset_rule
  predictor_function <- family_config$predictor_function
  validation_level <- family_config$validation_level
  framework_group <- family_config$framework_group
  framework_context <- family_config$framework_context
  granularity <- family_config$granularity
  domain_label <- family_config$domain_label
  construct_label <- family_config$construct_label
  
  cat("\n========================================\n")
  cat("🎯 Running performance checks for:", model_family, "\n")
  cat("========================================\n")
  
  predictors <- predictor_function(mapping, df)
  
  cat("📦 Predictors selected:", length(predictors), "\n")
  cat("🔎 Validation level:", validation_level, "\n")
  cat("🔎 Framework/context/granularity:", framework_group, "/", framework_context, "/", granularity, "\n")
  cat("🔎 Domain:", domain_label, "| Construct:", construct_label, "\n")
  
  if (length(predictors) == 0) {
    stop("No predictors selected for model family: ", model_family)
  }
  
  for (outcome in outcomes) {
    
    cat("\n➡️ Outcome:", outcome, "\n")
    
    pvs <- paste0("PV", 1:10, outcome)
    
    missing_pvs <- setdiff(pvs, names(df))
    
    if (length(missing_pvs) > 0) {
      stop(
        "Missing plausible value columns for outcome ",
        outcome, ": ",
        paste(missing_pvs, collapse = ", ")
      )
    }
    
    for (pv_name in pvs) {
      
      cat("📊 Running RF performance check:", model_family, "/", pv_name, "\n")
      
      result <- run_rf_performance_check(
        model_family = model_family,
        validation_level = validation_level,
        framework_group = framework_group,
        framework_context = framework_context,
        granularity = granularity,
        domain_label = domain_label,
        construct_label = construct_label,
        subset_rule = subset_rule,
        pv_name = pv_name,
        outcome = outcome,
        df = df,
        predictors = predictors,
        weight_var = weight_var,
        sample_size_per_pv = sample_size_per_pv
      )
      
      all_performance_results[[counter]] <- result
      counter <- counter + 1
    }
  }
}


# --------------------------------------
# 11. Combine and save PV-level results
# --------------------------------------

performance_by_pv <- bind_rows(all_performance_results) %>%
  arrange(model_family, outcome, pv)

performance_by_pv_file <- file.path(
  output_dir,
  "rf_validation_performance_by_pv.csv"
)

write_csv(performance_by_pv, performance_by_pv_file)

cat("\n✅ PV-level performance results saved to:\n", performance_by_pv_file, "\n")


# --------------------------------------
# 12. Create and save outcome-level summary
# --------------------------------------

performance_summary_by_outcome <- performance_by_pv %>%
  group_by(
    model_family,
    validation_level,
    framework_group,
    framework_context,
    granularity,
    domain_label,
    construct_label,
    outcome
  ) %>%
  summarise(
    n_pv_models = n(),
    mean_n_predictors = mean(n_predictors, na.rm = TRUE),
    mean_n_available_before_sampling = mean(n_available_before_sampling, na.rm = TRUE),
    mean_n_sampled = mean(n_sampled, na.rm = TRUE),
    mean_ntree = mean(ntree, na.rm = TRUE),
    mean_mtry_used = mean(mtry_used, na.rm = TRUE),
    mean_oob_mse = mean(oob_mse, na.rm = TRUE),
    sd_oob_mse = sd(oob_mse, na.rm = TRUE),
    mean_oob_rmse = mean(oob_rmse, na.rm = TRUE),
    sd_oob_rmse = sd(oob_rmse, na.rm = TRUE),
    mean_outcome_variance = mean(outcome_variance, na.rm = TRUE),
    mean_oob_r2 = mean(oob_r2, na.rm = TRUE),
    sd_oob_r2 = sd(oob_r2, na.rm = TRUE),
    min_oob_r2 = min(oob_r2, na.rm = TRUE),
    max_oob_r2 = max(oob_r2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(model_family, outcome)

performance_summary_by_outcome_file <- file.path(
  output_dir,
  "rf_validation_performance_summary_by_outcome.csv"
)

write_csv(performance_summary_by_outcome, performance_summary_by_outcome_file)

cat("\n✅ Outcome-level performance summary saved to:\n", performance_summary_by_outcome_file, "\n")


# --------------------------------------
# 13. Create and save model-family-level summary
# --------------------------------------

performance_summary_by_model_family <- performance_by_pv %>%
  group_by(
    model_family,
    validation_level,
    framework_group,
    framework_context,
    granularity,
    domain_label,
    construct_label
  ) %>%
  summarise(
    n_pv_models = n(),
    outcomes_covered = paste(sort(unique(outcome)), collapse = "; "),
    mean_n_predictors = mean(n_predictors, na.rm = TRUE),
    mean_n_available_before_sampling = mean(n_available_before_sampling, na.rm = TRUE),
    mean_n_sampled = mean(n_sampled, na.rm = TRUE),
    mean_ntree = mean(ntree, na.rm = TRUE),
    mean_mtry_used = mean(mtry_used, na.rm = TRUE),
    mean_oob_mse = mean(oob_mse, na.rm = TRUE),
    sd_oob_mse = sd(oob_mse, na.rm = TRUE),
    mean_oob_rmse = mean(oob_rmse, na.rm = TRUE),
    sd_oob_rmse = sd(oob_rmse, na.rm = TRUE),
    mean_outcome_variance = mean(outcome_variance, na.rm = TRUE),
    mean_oob_r2 = mean(oob_r2, na.rm = TRUE),
    sd_oob_r2 = sd(oob_r2, na.rm = TRUE),
    min_oob_r2 = min(oob_r2, na.rm = TRUE),
    max_oob_r2 = max(oob_r2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(model_family)

performance_summary_by_model_family_file <- file.path(
  output_dir,
  "rf_validation_performance_summary_by_model_family.csv"
)

write_csv(performance_summary_by_model_family, performance_summary_by_model_family_file)

cat("\n✅ Model-family-level performance summary saved to:\n", performance_summary_by_model_family_file, "\n")

# --------------------------------------
# 13b. Create and save validation-level summary
# --------------------------------------

performance_summary_by_validation_level <- performance_by_pv %>%
  group_by(validation_level) %>%
  summarise(
    n_model_families = n_distinct(model_family),
    n_pv_models = n(),
    model_families_included = paste(sort(unique(model_family)), collapse = "; "),
    outcomes_covered = paste(sort(unique(outcome)), collapse = "; "),
    mean_n_predictors = mean(n_predictors, na.rm = TRUE),
    mean_n_available_before_sampling = mean(n_available_before_sampling, na.rm = TRUE),
    mean_n_sampled = mean(n_sampled, na.rm = TRUE),
    mean_ntree = mean(ntree, na.rm = TRUE),
    mean_mtry_used = mean(mtry_used, na.rm = TRUE),
    mean_oob_mse = mean(oob_mse, na.rm = TRUE),
    sd_oob_mse = sd(oob_mse, na.rm = TRUE),
    mean_oob_rmse = mean(oob_rmse, na.rm = TRUE),
    sd_oob_rmse = sd(oob_rmse, na.rm = TRUE),
    mean_outcome_variance = mean(outcome_variance, na.rm = TRUE),
    mean_oob_r2 = mean(oob_r2, na.rm = TRUE),
    sd_oob_r2 = sd(oob_r2, na.rm = TRUE),
    min_oob_r2 = min(oob_r2, na.rm = TRUE),
    max_oob_r2 = max(oob_r2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(validation_level)

performance_summary_by_validation_level_file <- file.path(
  output_dir,
  "rf_validation_performance_summary_by_validation_level.csv"
)

write_csv(performance_summary_by_validation_level, performance_summary_by_validation_level_file)

cat("\n✅ Validation-level performance summary saved to:\n", performance_summary_by_validation_level_file, "\n")


# --------------------------------------
# 14. Console preview
# --------------------------------------

cat("\n========================================\n")
cat("Preview: Outcome-level performance summary\n")
cat("========================================\n")
print(performance_summary_by_outcome)

cat("\n========================================\n")
cat("Preview: Model-family-level performance summary\n")
cat("========================================\n")
print(performance_summary_by_model_family)

cat("\n🎉 Random forest performance validation complete.\n")
# --------------------------------------
# Random Forest Validation Script 03:
# Sensitivity Checks for RF Specification and Importance Metric
# --------------------------------------
#
# Purpose:
# This standalone validation script assesses whether the variable-importance
# rankings used in the exploratory random forest (RF) analysis are sensitive to:
#
#   1. Importance metric:
#      - IncNodePurity
#      - permutation importance (%IncMSE)
#
#   2. RF specification:
#      - baseline default randomForest specification:
#          ntree = 500, mtry = default
#      - increased number of trees:
#          ntree = 1000, mtry = default
#      - alternative mtry:
#          ntree = 500, mtry = floor(sqrt(number of predictors))
#
# This script mirrors the model-family structure used in:
#   01_rf_validation_performance_checks.R
#   02_rf_validation_rank_stability.R
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
# To keep the sensitivity checks computationally feasible, the script uses a
# representative subset of plausible values:
#   - PV1
#   - PV5
#   - PV10
#
# across:
#   - Mathematics
#   - Reading
#   - Science
#
# This produces:
#   9 model families × 3 outcomes × 3 plausible values × 3 RF specifications
#   = 243 RF models
#
# The sensitivity checks are not intended to replace the main RF analysis.
# They assess whether the main ranking patterns are robust to plausible
# alternative RF settings and to an alternative importance metric.
#
# Outputs:
#
#   1. PV-level variable-importance rankings for each sensitivity specification:
#      rf_sensitivity_importance_by_model.csv
#
#   2. Pairwise sensitivity-comparison diagnostics:
#      rf_sensitivity_pairwise_comparisons.csv
#
#   3. Compact top-20 comparison table for reporting:
#      rf_sensitivity_top20_comparison_for_reporting.csv
#
#   4. Outcome-level sensitivity summary:
#      rf_sensitivity_summary_by_outcome.csv
#
#   5. Model-family-level sensitivity summary:
#      rf_sensitivity_summary_by_model_family.csv
#
#   6. Validation-level sensitivity summary:
#      rf_sensitivity_summary_by_validation_level.csv
#
#   7. Compact manuscript sensitivity summary:
#      rf_sensitivity_summary_for_manuscript.csv
#
# Key metrics:
#   - Spearman rank correlation between baseline and sensitivity rankings
#   - Pearson correlation between normalized importance scores
#   - Top-5, top-10, and top-20 overlap
#   - Top-5, top-10, and top-20 Jaccard similarity
#   - Flags for small slices where top-10 or top-20 overlap is less informative
#
# Interpretation:
# High similarity between the baseline IncNodePurity rankings and the sensitivity
# rankings strengthens confidence that the reported variable-importance patterns
# are not artefacts of a single RF specification or a single importance metric.
# These checks do not imply causality, effect size, or design-based population
# inference.
#
# Methodological note:
# Sensitivity checks are conducted on PV1, PV5, and PV10 for each achievement
# domain because the full rank-stability validation already evaluates ranking
# consistency across all ten plausible values.
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
  library(tidyr)
})


# --------------------------------------
# 2. Manual parameters
# --------------------------------------

input_data <- "data/private/processed/2022/pisa2022_cleaned_19_composites_added.csv"
mapping_table_path <- "data/private/metadata/2022/pisa2022_variable_mapping_table_final.csv"

output_dir <- "output/private/exploratory_data_analysis/2022/random_forests/rf_validation_outputs/sensitivity_checks"

dir_create(output_dir)

set.seed(1234)

outcomes <- c("MATH", "READ", "SCIE")
weight_var <- "W_FSTUWT"
sample_size_per_pv <- 5000

# Representative PVs for sensitivity checks.
# Rank-stability validation already evaluates all ten PVs.
sensitivity_pv_indices <- c(1, 5, 10)

top_k_values <- c(5, 10, 20)


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
# 9. RF specification helpers
# --------------------------------------

make_rf_specs <- function(n_predictors) {
  sqrt_mtry <- max(1L, floor(sqrt(n_predictors)))
  
  list(
    baseline_default = list(
      spec_id = "baseline_default",
      spec_label = "ntree500_default_mtry",
      ntree = 500L,
      mtry = NULL
    ),
    ntree_1000 = list(
      spec_id = "ntree_1000",
      spec_label = "ntree1000_default_mtry",
      ntree = 1000L,
      mtry = NULL
    ),
    mtry_sqrt = list(
      spec_id = "mtry_sqrt",
      spec_label = "ntree500_mtry_sqrt_p",
      ntree = 500L,
      mtry = sqrt_mtry
    )
  )
}


# --------------------------------------
# 10. Prepare one sampled dataset for a model-family/outcome/PV combination
# --------------------------------------

prepare_sampled_data <- function(
    model_family,
    pv_name,
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
  
  list(
    df_model = df_model,
    df_sampled = df_sampled
  )
}


# --------------------------------------
# 11. Run one RF sensitivity model
# --------------------------------------

run_rf_sensitivity_model <- function(
    model_family,
    validation_level,
    framework_group,
    framework_context,
    granularity,
    domain_label,
    construct_label,
    subset_rule,
    outcome,
    pv_name,
    spec_id,
    spec_label,
    ntree,
    mtry,
    df_model,
    df_sampled,
    predictors,
    weight_var
) {
  
  rf_formula <- as.formula(paste(pv_name, "~ ."))
  
  rf_data <- df_sampled %>%
    select(-all_of(weight_var))
  
  # Set deterministic model-level seed for reproducibility
  seed_string <- paste(model_family, outcome, pv_name, spec_id, sep = "_")
  seed_value <- sum(utf8ToInt(seed_string)) %% .Machine$integer.max
  set.seed(seed_value)
  
  if (is.null(mtry)) {
    rf_model <- randomForest(
      formula = rf_formula,
      data = rf_data,
      ntree = ntree,
      importance = TRUE
    )
  } else {
    rf_model <- randomForest(
      formula = rf_formula,
      data = rf_data,
      ntree = ntree,
      mtry = mtry,
      importance = TRUE
    )
  }
  
  imp_mat <- importance(rf_model)
  
  if (!"IncNodePurity" %in% colnames(imp_mat)) {
    stop(
      "IncNodePurity column not found for ",
      model_family, " / ", pv_name, " / ", spec_id
    )
  }
  
  if (!"%IncMSE" %in% colnames(imp_mat)) {
    stop(
      "Permutation importance column %IncMSE not found for ",
      model_family, " / ", pv_name, " / ", spec_id,
      ". Check that randomForest was run with importance = TRUE."
    )
  }
  
  node_purity <- as.numeric(imp_mat[, "IncNodePurity"])
  permutation_importance <- as.numeric(imp_mat[, "%IncMSE"])
  
  importance_long <- tibble(
    variable = rownames(imp_mat),
    inc_node_purity = node_purity,
    permutation_importance = permutation_importance
  ) %>%
    pivot_longer(
      cols = c(inc_node_purity, permutation_importance),
      names_to = "importance_metric",
      values_to = "importance_value"
    ) %>%
    mutate(
      importance_metric = recode(
        importance_metric,
        inc_node_purity = "IncNodePurity",
        permutation_importance = "Permutation_%IncMSE"
      )
    ) %>%
    group_by(importance_metric) %>%
    mutate(
      max_importance = suppressWarnings(max(importance_value, na.rm = TRUE)),
      safe_denom = if_else(
        is.finite(max_importance) & max_importance > 0,
        max_importance,
        NA_real_
      ),
      norm_importance = importance_value / safe_denom
    ) %>%
    ungroup() %>%
    select(-max_importance, -safe_denom) %>%
    group_by(importance_metric) %>%
    arrange(desc(importance_value), variable, .by_group = TRUE) %>%
    mutate(
      rank_in_model = row_number(),
      top5 = rank_in_model <= min(5L, length(predictors)),
      top10 = rank_in_model <= min(10L, length(predictors)),
      top20 = rank_in_model <= min(20L, length(predictors)),
      top10_is_full_slice = length(predictors) <= 10L,
      top20_is_full_slice = length(predictors) <= 20L
    ) %>%
    ungroup()
  
  importance_long %>%
    transmute(
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
      spec_id = spec_id,
      spec_label = spec_label,
      ntree_requested = ntree,
      mtry_requested = ifelse(is.null(mtry), NA_integer_, as.integer(mtry)),
      ntree_used = rf_model$ntree,
      mtry_used = rf_model$mtry,
      importance_metric = importance_metric,
      variable = variable,
      n_predictors = length(predictors),
      n_available_before_sampling = nrow(df_model),
      n_sampled = nrow(df_sampled),
      importance_value = importance_value,
      norm_importance = norm_importance,
      rank_in_model = rank_in_model,
      top5 = top5,
      top10 = top10,
      top20 = top20,
      top10_is_full_slice = top10_is_full_slice,
      top20_is_full_slice = top20_is_full_slice
    )
}


# --------------------------------------
# 12. Main sensitivity loop
# --------------------------------------

all_sensitivity_importance <- list()

counter <- 1

for (family_name in names(model_families)) {
  
  family_config <- model_families[[family_name]]
  
  model_family <- family_config$model_family
  validation_level <- family_config$validation_level
  framework_group <- family_config$framework_group
  framework_context <- family_config$framework_context
  granularity <- family_config$granularity
  domain_label <- family_config$domain_label
  construct_label <- family_config$construct_label
  subset_rule <- family_config$subset_rule
  predictor_function <- family_config$predictor_function
  
  cat("\n========================================\n")
  cat("🎯 Running sensitivity checks for:", model_family, "\n")
  cat("========================================\n")
  
  predictors <- predictor_function(mapping, df)
  
  cat("📦 Predictors selected:", length(predictors), "\n")
  cat("🔎 Validation level:", validation_level, "\n")
  cat("🔎 Framework/context/granularity:", framework_group, "/", framework_context, "/", granularity, "\n")
  cat("🔎 Domain:", domain_label, "| Construct:", construct_label, "\n")
  
  if (length(predictors) == 0) {
    stop("No predictors selected for model family: ", model_family)
  }
  
  rf_specs <- make_rf_specs(length(predictors))
  
  for (outcome in outcomes) {
    
    cat("\n➡️ Outcome:", outcome, "\n")
    
    pvs <- paste0("PV", sensitivity_pv_indices, outcome)
    
    missing_pvs <- setdiff(pvs, names(df))
    
    if (length(missing_pvs) > 0) {
      stop(
        "Missing plausible value columns for outcome ",
        outcome, ": ",
        paste(missing_pvs, collapse = ", ")
      )
    }
    
    for (pv_name in pvs) {
      
      cat("📊 Preparing shared weighted bootstrap sample:", model_family, "/", pv_name, "\n")
      
      sampled_data <- prepare_sampled_data(
        model_family = model_family,
        pv_name = pv_name,
        df = df,
        predictors = predictors,
        weight_var = weight_var,
        sample_size_per_pv = sample_size_per_pv
      )
      
      for (spec_name in names(rf_specs)) {
        
        spec <- rf_specs[[spec_name]]
        
        cat("   🔁 Running sensitivity spec:", spec$spec_id, "/", pv_name, "\n")
        
        result <- run_rf_sensitivity_model(
          model_family = model_family,
          validation_level = validation_level,
          framework_group = framework_group,
          framework_context = framework_context,
          granularity = granularity,
          domain_label = domain_label,
          construct_label = construct_label,
          subset_rule = subset_rule,
          outcome = outcome,
          pv_name = pv_name,
          spec_id = spec$spec_id,
          spec_label = spec$spec_label,
          ntree = spec$ntree,
          mtry = spec$mtry,
          df_model = sampled_data$df_model,
          df_sampled = sampled_data$df_sampled,
          predictors = predictors,
          weight_var = weight_var
        )
        
        all_sensitivity_importance[[counter]] <- result
        counter <- counter + 1
      }
    }
  }
}


# --------------------------------------
# 13. Combine and save sensitivity importance results
# --------------------------------------

sensitivity_importance_by_model <- bind_rows(all_sensitivity_importance) %>%
  arrange(
    validation_level,
    framework_group,
    framework_context,
    granularity,
    model_family,
    outcome,
    pv,
    spec_id,
    importance_metric,
    rank_in_model
  )

sensitivity_importance_by_model_file <- file.path(
  output_dir,
  "rf_sensitivity_importance_by_model.csv"
)

write_csv(sensitivity_importance_by_model, sensitivity_importance_by_model_file)

cat("\n✅ Sensitivity importance rankings saved to:\n", sensitivity_importance_by_model_file, "\n")


# --------------------------------------
# 14. Pairwise sensitivity comparison helpers
# --------------------------------------

calculate_topk_overlap <- function(joined, rank_a_col, rank_b_col, k) {
  k_eff <- min(k, nrow(joined))
  
  top_a <- joined %>%
    filter(.data[[rank_a_col]] <= k_eff) %>%
    pull(variable)
  
  top_b <- joined %>%
    filter(.data[[rank_b_col]] <= k_eff) %>%
    pull(variable)
  
  intersection_n <- length(intersect(top_a, top_b))
  union_n <- length(union(top_a, top_b))
  
  tibble(
    !!paste0("top", k, "_k_eff") := k_eff,
    !!paste0("top", k, "_intersection_n") := intersection_n,
    !!paste0("top", k, "_overlap_prop") := ifelse(k_eff > 0, intersection_n / k_eff, NA_real_),
    !!paste0("top", k, "_jaccard") := ifelse(union_n > 0, intersection_n / union_n, NA_real_),
    !!paste0("top", k, "_is_full_slice") := nrow(joined) <= k
  )
}

compare_two_rankings <- function(
    df_group,
    comparison_id,
    baseline_spec,
    baseline_metric,
    comparison_spec,
    comparison_metric
) {
  
  baseline <- df_group %>%
    filter(
      spec_id == baseline_spec,
      importance_metric == baseline_metric
    ) %>%
    select(
      variable,
      baseline_rank = rank_in_model,
      baseline_norm_importance = norm_importance
    )
  
  comparison <- df_group %>%
    filter(
      spec_id == comparison_spec,
      importance_metric == comparison_metric
    ) %>%
    select(
      variable,
      comparison_rank = rank_in_model,
      comparison_norm_importance = norm_importance
    )
  
  joined <- inner_join(baseline, comparison, by = "variable")
  
  if (nrow(joined) == 0) {
    return(tibble())
  }
  
  spearman_rank_correlation <- suppressWarnings(
    cor(joined$baseline_rank, joined$comparison_rank, method = "spearman", use = "complete.obs")
  )
  
  pearson_norm_importance_correlation <- suppressWarnings(
    cor(joined$baseline_norm_importance, joined$comparison_norm_importance, method = "pearson", use = "complete.obs")
  )
  
  top5_stats <- calculate_topk_overlap(joined, "baseline_rank", "comparison_rank", 5)
  top10_stats <- calculate_topk_overlap(joined, "baseline_rank", "comparison_rank", 10)
  top20_stats <- calculate_topk_overlap(joined, "baseline_rank", "comparison_rank", 20)
  
  bind_cols(
    tibble(
      comparison_id = comparison_id,
      baseline_spec = baseline_spec,
      baseline_metric = baseline_metric,
      comparison_spec = comparison_spec,
      comparison_metric = comparison_metric,
      n_predictors_compared = nrow(joined),
      spearman_rank_correlation = as.numeric(spearman_rank_correlation),
      pearson_norm_importance_correlation = as.numeric(pearson_norm_importance_correlation)
    ),
    top5_stats,
    top10_stats,
    top20_stats
  )
}

compute_sensitivity_comparisons <- function(df_group) {
  
  comparisons <- list(
    metric_sensitivity_nodepurity_vs_permutation = list(
      comparison_id = "metric_sensitivity_nodepurity_vs_permutation",
      baseline_spec = "baseline_default",
      baseline_metric = "IncNodePurity",
      comparison_spec = "baseline_default",
      comparison_metric = "Permutation_%IncMSE"
    ),
    ntree_sensitivity_500_vs_1000 = list(
      comparison_id = "ntree_sensitivity_500_vs_1000",
      baseline_spec = "baseline_default",
      baseline_metric = "IncNodePurity",
      comparison_spec = "ntree_1000",
      comparison_metric = "IncNodePurity"
    ),
    mtry_sensitivity_default_vs_sqrt = list(
      comparison_id = "mtry_sensitivity_default_vs_sqrt",
      baseline_spec = "baseline_default",
      baseline_metric = "IncNodePurity",
      comparison_spec = "mtry_sqrt",
      comparison_metric = "IncNodePurity"
    )
  )
  
  map_dfr(comparisons, function(comp) {
    compare_two_rankings(
      df_group = df_group,
      comparison_id = comp$comparison_id,
      baseline_spec = comp$baseline_spec,
      baseline_metric = comp$baseline_metric,
      comparison_spec = comp$comparison_spec,
      comparison_metric = comp$comparison_metric
    )
  })
}


# --------------------------------------
# 15. Compute pairwise sensitivity comparisons
# --------------------------------------

sensitivity_pairwise_comparisons <- sensitivity_importance_by_model %>%
  group_by(
    model_family,
    validation_level,
    framework_group,
    framework_context,
    granularity,
    domain_label,
    construct_label,
    outcome,
    pv
  ) %>%
  group_modify(~ compute_sensitivity_comparisons(.x)) %>%
  ungroup()

sensitivity_pairwise_comparisons_file <- file.path(
  output_dir,
  "rf_sensitivity_pairwise_comparisons.csv"
)

write_csv(sensitivity_pairwise_comparisons, sensitivity_pairwise_comparisons_file)

cat("\n✅ Pairwise sensitivity comparisons saved to:\n", sensitivity_pairwise_comparisons_file, "\n")


# --------------------------------------
# 16. Compact top-20 comparison table for reporting
# --------------------------------------

top20_baseline_for_reporting <- sensitivity_importance_by_model %>%
  filter(
    spec_id == "baseline_default",
    importance_metric == "IncNodePurity",
    top20
  ) %>%
  group_by(
    model_family,
    validation_level,
    framework_group,
    framework_context,
    granularity,
    domain_label,
    construct_label,
    outcome,
    variable
  ) %>%
  summarise(
    n_pvs_tested = length(sensitivity_pv_indices),
    n_selected_pvs = n_distinct(pv),
    n_top20_selected_pvs = n(),
    mean_rank_baseline = mean(rank_in_model, na.rm = TRUE),
    sd_rank_baseline = sd(rank_in_model, na.rm = TRUE),
    mean_norm_importance_baseline = mean(norm_importance, na.rm = TRUE),
    sd_norm_importance_baseline = sd(norm_importance, na.rm = TRUE),
    n_predictors = first(n_predictors),
    top20_is_full_slice = first(top20_is_full_slice),
    .groups = "drop"
  ) %>%
  arrange(
    validation_level,
    framework_group,
    framework_context,
    granularity,
    model_family,
    outcome,
    mean_rank_baseline,
    desc(mean_norm_importance_baseline)
  )

top20_baseline_for_reporting_file <- file.path(
  output_dir,
  "rf_sensitivity_top20_comparison_for_reporting.csv"
)

write_csv(top20_baseline_for_reporting, top20_baseline_for_reporting_file)

cat("\n✅ Compact top-20 baseline sensitivity table saved to:\n", top20_baseline_for_reporting_file, "\n")


# --------------------------------------
# 17. Outcome-level sensitivity summary
# --------------------------------------

sensitivity_summary_by_outcome <- sensitivity_pairwise_comparisons %>%
  group_by(
    model_family,
    validation_level,
    framework_group,
    framework_context,
    granularity,
    domain_label,
    construct_label,
    outcome,
    comparison_id
  ) %>%
  summarise(
    n_pv_comparisons = n(),
    mean_spearman_rank_correlation = mean(spearman_rank_correlation, na.rm = TRUE),
    sd_spearman_rank_correlation = sd(spearman_rank_correlation, na.rm = TRUE),
    min_spearman_rank_correlation = min(spearman_rank_correlation, na.rm = TRUE),
    max_spearman_rank_correlation = max(spearman_rank_correlation, na.rm = TRUE),
    mean_pearson_norm_importance_correlation = mean(pearson_norm_importance_correlation, na.rm = TRUE),
    
    mean_top5_overlap_prop = mean(top5_overlap_prop, na.rm = TRUE),
    mean_top5_jaccard = mean(top5_jaccard, na.rm = TRUE),
    
    mean_top10_overlap_prop = mean(top10_overlap_prop, na.rm = TRUE),
    mean_top10_jaccard = mean(top10_jaccard, na.rm = TRUE),
    
    mean_top20_overlap_prop = mean(top20_overlap_prop, na.rm = TRUE),
    mean_top20_jaccard = mean(top20_jaccard, na.rm = TRUE),
    
    n_predictors_compared = first(n_predictors_compared),
    top10_is_full_slice = first(top10_is_full_slice),
    top20_is_full_slice = first(top20_is_full_slice),
    .groups = "drop"
  ) %>%
  mutate(
    interpretive_note = case_when(
      n_predictors_compared <= 10 &
        mean_spearman_rank_correlation >= 0.80 &
        mean_top5_overlap_prop >= 0.70 ~
        "High sensitivity robustness; top-10/top-20 overlap less informative because the slice is small",
      
      n_predictors_compared <= 10 &
        mean_spearman_rank_correlation >= 0.60 &
        mean_top5_overlap_prop >= 0.50 ~
        "Moderate sensitivity robustness; top-10/top-20 overlap less informative because the slice is small",
      
      n_predictors_compared > 10 &
        mean_spearman_rank_correlation >= 0.80 &
        mean_top10_overlap_prop >= 0.70 ~
        "High sensitivity robustness",
      
      n_predictors_compared > 10 &
        mean_spearman_rank_correlation >= 0.60 &
        mean_top10_overlap_prop >= 0.50 ~
        "Moderate sensitivity robustness",
      
      mean_spearman_rank_correlation >= 0.40 ~
        "Limited but usable sensitivity robustness; interpret exact ranks cautiously",
      
      TRUE ~
        "Low sensitivity robustness; avoid strong interpretation of exact ranks"
    ),
    top10_overlap_note = if_else(
      top10_is_full_slice,
      "Top-10 overlap is not informative because this model family has 10 or fewer predictors.",
      "Top-10 overlap is informative for this model family."
    ),
    top20_overlap_note = if_else(
      top20_is_full_slice,
      "Top-20 overlap is not informative because this model family has 20 or fewer predictors.",
      "Top-20 overlap is informative for this model family."
    )
  ) %>%
  arrange(validation_level, framework_group, framework_context, granularity, model_family, outcome, comparison_id)

sensitivity_summary_by_outcome_file <- file.path(
  output_dir,
  "rf_sensitivity_summary_by_outcome.csv"
)

write_csv(sensitivity_summary_by_outcome, sensitivity_summary_by_outcome_file)

cat("\n✅ Outcome-level sensitivity summary saved to:\n", sensitivity_summary_by_outcome_file, "\n")


# --------------------------------------
# 18. Model-family-level sensitivity summary
# --------------------------------------

sensitivity_summary_by_outcome_for_family <- sensitivity_summary_by_outcome %>%
  rename(
    outcome_mean_spearman_rank_correlation = mean_spearman_rank_correlation,
    outcome_min_spearman_rank_correlation = min_spearman_rank_correlation,
    outcome_max_spearman_rank_correlation = max_spearman_rank_correlation,
    outcome_mean_pearson_norm_importance_correlation = mean_pearson_norm_importance_correlation,
    outcome_mean_top5_overlap_prop = mean_top5_overlap_prop,
    outcome_mean_top5_jaccard = mean_top5_jaccard,
    outcome_mean_top10_overlap_prop = mean_top10_overlap_prop,
    outcome_mean_top10_jaccard = mean_top10_jaccard,
    outcome_mean_top20_overlap_prop = mean_top20_overlap_prop,
    outcome_mean_top20_jaccard = mean_top20_jaccard
  )

sensitivity_summary_by_model_family <- sensitivity_summary_by_outcome_for_family %>%
  group_by(
    model_family,
    validation_level,
    framework_group,
    framework_context,
    granularity,
    domain_label,
    construct_label,
    comparison_id
  ) %>%
  summarise(
    outcomes_covered = paste(sort(unique(outcome)), collapse = "; "),
    n_outcome_models = n(),
    mean_n_predictors_compared = mean(n_predictors_compared, na.rm = TRUE),
    
    mean_spearman_rank_correlation = mean(outcome_mean_spearman_rank_correlation, na.rm = TRUE),
    sd_spearman_rank_correlation = sd(outcome_mean_spearman_rank_correlation, na.rm = TRUE),
    min_spearman_rank_correlation = min(outcome_min_spearman_rank_correlation, na.rm = TRUE),
    max_spearman_rank_correlation = max(outcome_max_spearman_rank_correlation, na.rm = TRUE),
    mean_pearson_norm_importance_correlation = mean(outcome_mean_pearson_norm_importance_correlation, na.rm = TRUE),
    
    mean_top5_overlap_prop = mean(outcome_mean_top5_overlap_prop, na.rm = TRUE),
    mean_top5_jaccard = mean(outcome_mean_top5_jaccard, na.rm = TRUE),
    
    mean_top10_overlap_prop = mean(outcome_mean_top10_overlap_prop, na.rm = TRUE),
    mean_top10_jaccard = mean(outcome_mean_top10_jaccard, na.rm = TRUE),
    
    mean_top20_overlap_prop = mean(outcome_mean_top20_overlap_prop, na.rm = TRUE),
    mean_top20_jaccard = mean(outcome_mean_top20_jaccard, na.rm = TRUE),
    
    any_top10_full_slice = any(top10_is_full_slice, na.rm = TRUE),
    any_top20_full_slice = any(top20_is_full_slice, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    interpretive_note = case_when(
      mean_n_predictors_compared <= 10 &
        mean_spearman_rank_correlation >= 0.80 &
        mean_top5_overlap_prop >= 0.70 ~
        "High sensitivity robustness; top-10/top-20 overlap less informative because the slice is small",
      
      mean_n_predictors_compared <= 10 &
        mean_spearman_rank_correlation >= 0.60 &
        mean_top5_overlap_prop >= 0.50 ~
        "Moderate sensitivity robustness; top-10/top-20 overlap less informative because the slice is small",
      
      mean_n_predictors_compared > 10 &
        mean_spearman_rank_correlation >= 0.80 &
        mean_top10_overlap_prop >= 0.70 ~
        "High sensitivity robustness",
      
      mean_n_predictors_compared > 10 &
        mean_spearman_rank_correlation >= 0.60 &
        mean_top10_overlap_prop >= 0.50 ~
        "Moderate sensitivity robustness",
      
      mean_spearman_rank_correlation >= 0.40 ~
        "Limited but usable sensitivity robustness; interpret exact ranks cautiously",
      
      TRUE ~
        "Low sensitivity robustness; avoid strong interpretation of exact ranks"
    ),
    top10_overlap_note = if_else(
      any_top10_full_slice,
      "At least one outcome has 10 or fewer predictors, so top-10 overlap should be interpreted cautiously.",
      "Top-10 overlap is informative across outcomes."
    ),
    top20_overlap_note = if_else(
      any_top20_full_slice,
      "At least one outcome has 20 or fewer predictors, so top-20 overlap should be interpreted cautiously.",
      "Top-20 overlap is informative across outcomes."
    )
  ) %>%
  arrange(validation_level, framework_group, framework_context, granularity, model_family, comparison_id)

sensitivity_summary_by_model_family_file <- file.path(
  output_dir,
  "rf_sensitivity_summary_by_model_family.csv"
)

write_csv(sensitivity_summary_by_model_family, sensitivity_summary_by_model_family_file)

cat("\n✅ Model-family-level sensitivity summary saved to:\n", sensitivity_summary_by_model_family_file, "\n")


# --------------------------------------
# 19. Validation-level sensitivity summary
# --------------------------------------

sensitivity_summary_by_model_family_for_level <- sensitivity_summary_by_model_family %>%
  rename(
    family_mean_spearman_rank_correlation = mean_spearman_rank_correlation,
    family_min_spearman_rank_correlation = min_spearman_rank_correlation,
    family_max_spearman_rank_correlation = max_spearman_rank_correlation,
    family_mean_pearson_norm_importance_correlation = mean_pearson_norm_importance_correlation,
    family_mean_top5_overlap_prop = mean_top5_overlap_prop,
    family_mean_top5_jaccard = mean_top5_jaccard,
    family_mean_top10_overlap_prop = mean_top10_overlap_prop,
    family_mean_top10_jaccard = mean_top10_jaccard,
    family_mean_top20_overlap_prop = mean_top20_overlap_prop,
    family_mean_top20_jaccard = mean_top20_jaccard
  )

sensitivity_summary_by_validation_level <- sensitivity_summary_by_model_family_for_level %>%
  group_by(validation_level, comparison_id) %>%
  summarise(
    n_model_families = n_distinct(model_family),
    model_families_included = paste(sort(unique(model_family)), collapse = "; "),
    mean_n_predictors_compared = mean(mean_n_predictors_compared, na.rm = TRUE),
    
    mean_spearman_rank_correlation = mean(family_mean_spearman_rank_correlation, na.rm = TRUE),
    sd_spearman_rank_correlation = sd(family_mean_spearman_rank_correlation, na.rm = TRUE),
    min_spearman_rank_correlation = min(family_min_spearman_rank_correlation, na.rm = TRUE),
    max_spearman_rank_correlation = max(family_max_spearman_rank_correlation, na.rm = TRUE),
    mean_pearson_norm_importance_correlation = mean(family_mean_pearson_norm_importance_correlation, na.rm = TRUE),
    
    mean_top5_overlap_prop = mean(family_mean_top5_overlap_prop, na.rm = TRUE),
    mean_top5_jaccard = mean(family_mean_top5_jaccard, na.rm = TRUE),
    
    mean_top10_overlap_prop = mean(family_mean_top10_overlap_prop, na.rm = TRUE),
    mean_top10_jaccard = mean(family_mean_top10_jaccard, na.rm = TRUE),
    
    mean_top20_overlap_prop = mean(family_mean_top20_overlap_prop, na.rm = TRUE),
    mean_top20_jaccard = mean(family_mean_top20_jaccard, na.rm = TRUE),
    
    any_top10_full_slice = any(any_top10_full_slice, na.rm = TRUE),
    any_top20_full_slice = any(any_top20_full_slice, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    interpretive_note = case_when(
      mean_n_predictors_compared <= 10 &
        mean_spearman_rank_correlation >= 0.80 &
        mean_top5_overlap_prop >= 0.70 ~
        "High sensitivity robustness; top-10/top-20 overlap less informative because slices are small",
      
      mean_n_predictors_compared <= 10 &
        mean_spearman_rank_correlation >= 0.60 &
        mean_top5_overlap_prop >= 0.50 ~
        "Moderate sensitivity robustness; top-10/top-20 overlap less informative because slices are small",
      
      mean_n_predictors_compared > 10 &
        mean_spearman_rank_correlation >= 0.80 &
        mean_top10_overlap_prop >= 0.70 ~
        "High sensitivity robustness",
      
      mean_n_predictors_compared > 10 &
        mean_spearman_rank_correlation >= 0.60 &
        mean_top10_overlap_prop >= 0.50 ~
        "Moderate sensitivity robustness",
      
      mean_spearman_rank_correlation >= 0.40 ~
        "Limited but usable sensitivity robustness; interpret exact ranks cautiously",
      
      TRUE ~
        "Low sensitivity robustness; avoid strong interpretation of exact ranks"
    ),
    top10_overlap_note = if_else(
      any_top10_full_slice,
      "Some model families have 10 or fewer predictors, so top-10 overlap is not always informative.",
      "Top-10 overlap is informative across validation level."
    ),
    top20_overlap_note = if_else(
      any_top20_full_slice,
      "Some model families have 20 or fewer predictors, so top-20 overlap is not always informative.",
      "Top-20 overlap is informative across validation level."
    )
  ) %>%
  arrange(validation_level, comparison_id)

sensitivity_summary_by_validation_level_file <- file.path(
  output_dir,
  "rf_sensitivity_summary_by_validation_level.csv"
)

write_csv(sensitivity_summary_by_validation_level, sensitivity_summary_by_validation_level_file)

cat("\n✅ Validation-level sensitivity summary saved to:\n", sensitivity_summary_by_validation_level_file, "\n")


# --------------------------------------
# 19b. Compact sensitivity summary for manuscript reporting
# --------------------------------------

sensitivity_summary_for_manuscript <- sensitivity_summary_by_validation_level %>%
  select(
    validation_level,
    comparison_id,
    n_model_families,
    mean_n_predictors_compared,
    mean_spearman_rank_correlation,
    sd_spearman_rank_correlation,
    mean_top10_overlap_prop,
    mean_top20_overlap_prop,
    mean_top10_jaccard,
    mean_top20_jaccard,
    interpretive_note,
    top10_overlap_note,
    top20_overlap_note
  ) %>%
  arrange(validation_level, comparison_id)

sensitivity_summary_for_manuscript_file <- file.path(
  output_dir,
  "rf_sensitivity_summary_for_manuscript.csv"
)

write_csv(sensitivity_summary_for_manuscript, sensitivity_summary_for_manuscript_file)

cat("\n✅ Compact manuscript sensitivity summary saved to:\n", sensitivity_summary_for_manuscript_file, "\n")


# --------------------------------------
# 20. Row-count check
# --------------------------------------

cat("\nRow-count check:\n")
cat("Sensitivity importance rows:", nrow(sensitivity_importance_by_model), "\n")
cat("Pairwise sensitivity rows:", nrow(sensitivity_pairwise_comparisons), "\n")
cat("Outcome summary rows:", nrow(sensitivity_summary_by_outcome), "\n")
cat("Model-family summary rows:", nrow(sensitivity_summary_by_model_family), "\n")
cat("Validation-level summary rows:", nrow(sensitivity_summary_by_validation_level), "\n")
cat("Manuscript summary rows:", nrow(sensitivity_summary_for_manuscript), "\n")


# --------------------------------------
# 21. Console preview
# --------------------------------------

cat("\n========================================\n")
cat("Preview: Outcome-level sensitivity summary\n")
cat("========================================\n")
print(sensitivity_summary_by_outcome)

cat("\n========================================\n")
cat("Preview: Model-family-level sensitivity summary\n")
cat("========================================\n")
print(sensitivity_summary_by_model_family)

cat("\n========================================\n")
cat("Preview: Validation-level sensitivity summary\n")
cat("========================================\n")
print(sensitivity_summary_by_validation_level)

cat("\n========================================\n")
cat("Preview: Manuscript sensitivity summary\n")
cat("========================================\n")
print(sensitivity_summary_for_manuscript)

cat("\n🎉 Random forest sensitivity validation complete.\n")
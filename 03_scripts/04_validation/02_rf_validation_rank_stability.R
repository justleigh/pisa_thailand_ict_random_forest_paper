# --------------------------------------
# Random Forest Validation Script 02:
# Rank Stability Checks for Global and Selected Framework RF Models
# --------------------------------------
#
# Purpose:
# This standalone validation script assesses the stability of random forest
# variable-importance rankings across PISA plausible values.
#
# It mirrors the model-family structure used in:
#   01_rf_validation_performance_checks.R
#
# but focuses on rank stability rather than predictive performance.
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
#   1. PV-level variable-importance rankings:
#      rf_rank_stability_importance_by_pv.csv
#
#   2. Pairwise plausible-value rank-stability diagnostics:
#      rf_rank_stability_pairwise_pv.csv
#
#   3. Predictor-level recurrence and rank-stability diagnostics:
#      rf_rank_stability_top_predictor_recurrence.csv
#
#   4. Compact top-recurring predictor table for reporting:
#      rf_rank_stability_top20_predictors_for_reporting.csv
#
#   5. Outcome-level rank-stability summary:
#      rf_rank_stability_summary_by_outcome.csv
#
#   6. Model-family-level rank-stability summary:
#      rf_rank_stability_summary_by_model_family.csv
#
#   7. Validation-level rank-stability summary:
#      rf_rank_stability_summary_by_validation_level.csv
#
# Key metrics:
#   - Spearman rank correlation across PV pairs
#   - Top-5, top-10, and top-20 overlap across PV pairs
#   - Top-5, top-10, and top-20 Jaccard similarity across PV pairs
#   - Predictor recurrence in top-5, top-10, and top-20 across PVs
#   - Mean, SD, minimum, and maximum rank by predictor
#   - Mean and SD of normalized importance by predictor
#
# Interpretation:
# These diagnostics assess whether variable-importance rankings are sufficiently
# stable across plausible values to support exploratory interpretation.
# They do not establish causality, effect size, or design-based population
# inference. They should be interpreted alongside the performance checks,
# sensitivity checks, and correlation diagnostics.
#
# Methodological note:
# Because PISA plausible values represent uncertainty in latent achievement
# estimates, variables should not be interpreted as robustly salient if they
# appear only in a single plausible-value model. Stable recurrence across PVs
# strengthens confidence that a predictor is part of the model's recurring
# predictive structure.
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

output_dir <- "output/private/exploratory_data_analysis/2022/random_forests/rf_validation_outputs/rank_stability"

dir_create(output_dir)

set.seed(1234)

outcomes <- c("MATH", "READ", "SCIE")
weight_var <- "W_FSTUWT"
sample_size_per_pv <- 5000

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
# 9. Function to run one RF rank-stability model
# --------------------------------------

run_rf_rank_model <- function(
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
  
  imp_mat <- importance(rf_model)
  imp_df <- as.data.frame(imp_mat) %>%
    rownames_to_column("variable")
  
  if (!"IncNodePurity" %in% names(imp_df)) {
    stop("IncNodePurity column not found in RF importance output for ", model_family, " / ", pv_name)
  }
  
  max_imp <- suppressWarnings(max(imp_df$IncNodePurity, na.rm = TRUE))
  safe_denom <- ifelse(is.finite(max_imp) && max_imp > 0, max_imp, NA_real_)
  
  imp_df %>%
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
      variable = variable,
      n_predictors = length(predictors),
      n_available_before_sampling = nrow(df_model),
      n_sampled = nrow(df_sampled),
      ntree = rf_model$ntree,
      mtry_used = rf_model$mtry,
      inc_node_purity = as.numeric(IncNodePurity),
      norm_importance = as.numeric(IncNodePurity) / safe_denom
    ) %>%
    arrange(desc(norm_importance), variable) %>%
    mutate(
      rank_in_pv = row_number(),
      top5 = rank_in_pv <= min(5L, n_predictors),
      top10 = rank_in_pv <= min(10L, n_predictors),
      top20 = rank_in_pv <= min(20L, n_predictors),
      top10_is_full_slice = n_predictors <= 10L,
      top20_is_full_slice = n_predictors <= 20L
    )
}


# --------------------------------------
# 10. Main RF rank-stability loop
# --------------------------------------

all_importance_results <- list()

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
  cat("🎯 Running rank-stability RF models for:", model_family, "\n")
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
      
      cat("📊 Running RF rank-stability model:", model_family, "/", pv_name, "\n")
      
      result <- run_rf_rank_model(
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
      
      all_importance_results[[counter]] <- result
      counter <- counter + 1
    }
  }
}


# --------------------------------------
# 11. Combine and save PV-level importance rankings
# --------------------------------------

importance_by_pv <- bind_rows(all_importance_results) %>%
  arrange(
    validation_level,
    framework_group,
    framework_context,
    granularity,
    model_family,
    outcome,
    pv,
    rank_in_pv
  )

importance_by_pv_file <- file.path(
  output_dir,
  "rf_rank_stability_importance_by_pv.csv"
)

write_csv(importance_by_pv, importance_by_pv_file)

cat("\n✅ PV-level rank-stability importance results saved to:\n", importance_by_pv_file, "\n")


# --------------------------------------
# 12. Pairwise PV rank-stability diagnostics
# --------------------------------------

compute_pairwise_stability <- function(df_group) {
  
  pv_list <- sort(unique(df_group$pv))
  pv_pairs <- combn(pv_list, 2, simplify = FALSE)
  
  map_dfr(pv_pairs, function(pair) {
    
    pv_a <- pair[1]
    pv_b <- pair[2]
    
    a <- df_group %>%
      filter(pv == pv_a) %>%
      select(variable, rank_a = rank_in_pv)
    
    b <- df_group %>%
      filter(pv == pv_b) %>%
      select(variable, rank_b = rank_in_pv)
    
    joined <- inner_join(a, b, by = "variable")
    
    n_predictors_pair <- nrow(joined)
    
    spearman_rank_correlation <- suppressWarnings(
      cor(joined$rank_a, joined$rank_b, method = "spearman", use = "complete.obs")
    )
    
    calculate_topk <- function(k) {
      k_eff <- min(k, n_predictors_pair)
      
      top_a <- joined %>%
        filter(rank_a <= k_eff) %>%
        pull(variable)
      
      top_b <- joined %>%
        filter(rank_b <= k_eff) %>%
        pull(variable)
      
      intersection_n <- length(intersect(top_a, top_b))
      union_n <- length(union(top_a, top_b))
      
      tibble(
        !!paste0("top", k, "_k_eff") := k_eff,
        !!paste0("top", k, "_intersection_n") := intersection_n,
        !!paste0("top", k, "_overlap_prop") := ifelse(k_eff > 0, intersection_n / k_eff, NA_real_),
        !!paste0("top", k, "_jaccard") := ifelse(union_n > 0, intersection_n / union_n, NA_real_),
        !!paste0("top", k, "_is_full_slice") := n_predictors_pair <= k
      )
    }
    
    top5_stats <- calculate_topk(5)
    top10_stats <- calculate_topk(10)
    top20_stats <- calculate_topk(20)
    
    bind_cols(
      tibble(
        pv_a = pv_a,
        pv_b = pv_b,
        n_predictors_pair = n_predictors_pair,
        spearman_rank_correlation = as.numeric(spearman_rank_correlation)
      ),
      top5_stats,
      top10_stats,
      top20_stats
    )
  })
}

pairwise_pv_stability <- importance_by_pv %>%
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
  group_modify(~ compute_pairwise_stability(.x)) %>%
  ungroup()

pairwise_pv_stability_file <- file.path(
  output_dir,
  "rf_rank_stability_pairwise_pv.csv"
)

write_csv(pairwise_pv_stability, pairwise_pv_stability_file)

cat("\n✅ Pairwise PV rank-stability diagnostics saved to:\n", pairwise_pv_stability_file, "\n")


# --------------------------------------
# 13. Predictor-level recurrence and rank-stability diagnostics
# --------------------------------------

top_predictor_recurrence <- importance_by_pv %>%
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
    n_pv_models = n_distinct(pv),
    n_predictors = first(n_predictors),
    n_top5 = sum(top5, na.rm = TRUE),
    n_top10 = sum(top10, na.rm = TRUE),
    n_top20 = sum(top20, na.rm = TRUE),
    top5_recurrence_rate = n_top5 / n_pv_models,
    top10_recurrence_rate = n_top10 / n_pv_models,
    top20_recurrence_rate = n_top20 / n_pv_models,
    mean_rank = mean(rank_in_pv, na.rm = TRUE),
    sd_rank = sd(rank_in_pv, na.rm = TRUE),
    min_rank = min(rank_in_pv, na.rm = TRUE),
    max_rank = max(rank_in_pv, na.rm = TRUE),
    mean_norm_importance = mean(norm_importance, na.rm = TRUE),
    sd_norm_importance = sd(norm_importance, na.rm = TRUE),
    max_norm_importance = max(norm_importance, na.rm = TRUE),
    top10_is_full_slice = first(top10_is_full_slice),
    top20_is_full_slice = first(top20_is_full_slice),
    stable_top5_all_pvs = n_top5 == n_pv_models,
    stable_top10_all_pvs = n_top10 == n_pv_models,
    stable_top20_all_pvs = n_top20 == n_pv_models,
    stable_top5_80pct_pvs = top5_recurrence_rate >= 0.80,
    stable_top10_80pct_pvs = top10_recurrence_rate >= 0.80,
    stable_top20_80pct_pvs = top20_recurrence_rate >= 0.80,
    .groups = "drop"
  ) %>%
  arrange(
    validation_level,
    framework_group,
    framework_context,
    granularity,
    model_family,
    outcome,
    mean_rank,
    desc(mean_norm_importance)
  )

top_predictor_recurrence_file <- file.path(
  output_dir,
  "rf_rank_stability_top_predictor_recurrence.csv"
)

write_csv(top_predictor_recurrence, top_predictor_recurrence_file)

cat("\n✅ Predictor-level recurrence diagnostics saved to:\n", top_predictor_recurrence_file, "\n")


# --------------------------------------
# 13b. Compact top-recurring predictors for reporting
# --------------------------------------

top_recurring_predictors_for_reporting <- top_predictor_recurrence %>%
  group_by(model_family, outcome) %>%
  arrange(mean_rank, desc(mean_norm_importance), .by_group = TRUE) %>%
  slice_head(n = 20) %>%
  ungroup()

top_recurring_predictors_for_reporting_file <- file.path(
  output_dir,
  "rf_rank_stability_top20_predictors_for_reporting.csv"
)

write_csv(top_recurring_predictors_for_reporting, top_recurring_predictors_for_reporting_file)

cat("\n✅ Compact top-recurring predictor table saved to:\n", top_recurring_predictors_for_reporting_file, "\n")


# --------------------------------------
# 14. Outcome-level rank-stability summary
# --------------------------------------

rank_stability_summary_by_outcome <- pairwise_pv_stability %>%
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
    n_pv_pairwise_comparisons = n(),
    mean_spearman_rank_correlation = mean(spearman_rank_correlation, na.rm = TRUE),
    sd_spearman_rank_correlation = sd(spearman_rank_correlation, na.rm = TRUE),
    min_spearman_rank_correlation = min(spearman_rank_correlation, na.rm = TRUE),
    max_spearman_rank_correlation = max(spearman_rank_correlation, na.rm = TRUE),
    
    mean_top5_overlap_prop = mean(top5_overlap_prop, na.rm = TRUE),
    sd_top5_overlap_prop = sd(top5_overlap_prop, na.rm = TRUE),
    mean_top5_jaccard = mean(top5_jaccard, na.rm = TRUE),
    
    mean_top10_overlap_prop = mean(top10_overlap_prop, na.rm = TRUE),
    sd_top10_overlap_prop = sd(top10_overlap_prop, na.rm = TRUE),
    mean_top10_jaccard = mean(top10_jaccard, na.rm = TRUE),
    
    mean_top20_overlap_prop = mean(top20_overlap_prop, na.rm = TRUE),
    sd_top20_overlap_prop = sd(top20_overlap_prop, na.rm = TRUE),
    mean_top20_jaccard = mean(top20_jaccard, na.rm = TRUE),
    
    n_predictors_pair = first(n_predictors_pair),
    top10_is_full_slice = first(top10_is_full_slice),
    top20_is_full_slice = first(top20_is_full_slice),
    .groups = "drop"
  ) %>%
  left_join(
    top_predictor_recurrence %>%
      group_by(model_family, outcome) %>%
      summarise(
        n_predictors = first(n_predictors),
        n_predictors_stable_top5_80pct_pvs = sum(stable_top5_80pct_pvs, na.rm = TRUE),
        n_predictors_stable_top10_80pct_pvs = sum(stable_top10_80pct_pvs, na.rm = TRUE),
        n_predictors_stable_top20_80pct_pvs = sum(stable_top20_80pct_pvs, na.rm = TRUE),
        n_predictors_stable_top5_all_pvs = sum(stable_top5_all_pvs, na.rm = TRUE),
        n_predictors_stable_top10_all_pvs = sum(stable_top10_all_pvs, na.rm = TRUE),
        n_predictors_stable_top20_all_pvs = sum(stable_top20_all_pvs, na.rm = TRUE),
        .groups = "drop"
      ),
    by = c("model_family", "outcome")
  ) %>%
  mutate(
    interpretive_note = case_when(
      n_predictors_pair <= 10 &
        mean_spearman_rank_correlation >= 0.80 &
        mean_top5_overlap_prop >= 0.70 ~
        "High rank stability across plausible values; top-10/top-20 overlap less informative because the slice is small",
      
      n_predictors_pair <= 10 &
        mean_spearman_rank_correlation >= 0.60 &
        mean_top5_overlap_prop >= 0.50 ~
        "Moderate rank stability across plausible values; top-10/top-20 overlap less informative because the slice is small",
      
      n_predictors_pair > 10 &
        mean_spearman_rank_correlation >= 0.80 &
        mean_top10_overlap_prop >= 0.70 ~
        "High rank stability across plausible values",
      
      n_predictors_pair > 10 &
        mean_spearman_rank_correlation >= 0.60 &
        mean_top10_overlap_prop >= 0.50 ~
        "Moderate rank stability across plausible values",
      
      mean_spearman_rank_correlation >= 0.40 ~
        "Limited but usable rank stability; interpret top predictors cautiously",
      
      TRUE ~
        "Low rank stability; avoid strong interpretation of exact ranks"
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
  arrange(validation_level, framework_group, framework_context, granularity, model_family, outcome)

rank_stability_summary_by_outcome_file <- file.path(
  output_dir,
  "rf_rank_stability_summary_by_outcome.csv"
)

write_csv(rank_stability_summary_by_outcome, rank_stability_summary_by_outcome_file)

cat("\n✅ Outcome-level rank-stability summary saved to:\n", rank_stability_summary_by_outcome_file, "\n")


# --------------------------------------
# 15. Model-family-level rank-stability summary
# --------------------------------------

rank_stability_summary_by_outcome_for_family <- rank_stability_summary_by_outcome %>%
  rename(
    outcome_mean_spearman_rank_correlation = mean_spearman_rank_correlation,
    outcome_mean_top5_overlap_prop = mean_top5_overlap_prop,
    outcome_mean_top5_jaccard = mean_top5_jaccard,
    outcome_mean_top10_overlap_prop = mean_top10_overlap_prop,
    outcome_mean_top10_jaccard = mean_top10_jaccard,
    outcome_mean_top20_overlap_prop = mean_top20_overlap_prop,
    outcome_mean_top20_jaccard = mean_top20_jaccard
  )

rank_stability_summary_by_model_family <- rank_stability_summary_by_outcome_for_family %>%
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
    outcomes_covered = paste(sort(unique(outcome)), collapse = "; "),
    n_outcome_models = n(),
    mean_n_predictors = mean(n_predictors, na.rm = TRUE),
    
    mean_spearman_rank_correlation = mean(outcome_mean_spearman_rank_correlation, na.rm = TRUE),
    sd_spearman_rank_correlation = sd(outcome_mean_spearman_rank_correlation, na.rm = TRUE),
    
    mean_top5_overlap_prop = mean(outcome_mean_top5_overlap_prop, na.rm = TRUE),
    mean_top5_jaccard = mean(outcome_mean_top5_jaccard, na.rm = TRUE),
    
    mean_top10_overlap_prop = mean(outcome_mean_top10_overlap_prop, na.rm = TRUE),
    mean_top10_jaccard = mean(outcome_mean_top10_jaccard, na.rm = TRUE),
    
    mean_top20_overlap_prop = mean(outcome_mean_top20_overlap_prop, na.rm = TRUE),
    mean_top20_jaccard = mean(outcome_mean_top20_jaccard, na.rm = TRUE),
    
    mean_n_predictors_stable_top5_80pct_pvs = mean(n_predictors_stable_top5_80pct_pvs, na.rm = TRUE),
    mean_n_predictors_stable_top10_80pct_pvs = mean(n_predictors_stable_top10_80pct_pvs, na.rm = TRUE),
    mean_n_predictors_stable_top20_80pct_pvs = mean(n_predictors_stable_top20_80pct_pvs, na.rm = TRUE),
    
    any_top10_full_slice = any(top10_is_full_slice, na.rm = TRUE),
    any_top20_full_slice = any(top20_is_full_slice, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    interpretive_note = case_when(
      mean_n_predictors <= 10 &
        mean_spearman_rank_correlation >= 0.80 &
        mean_top5_overlap_prop >= 0.70 ~
        "High rank stability across plausible values; top-10/top-20 overlap less informative because the slice is small",
      
      mean_n_predictors <= 10 &
        mean_spearman_rank_correlation >= 0.60 &
        mean_top5_overlap_prop >= 0.50 ~
        "Moderate rank stability across plausible values; top-10/top-20 overlap less informative because the slice is small",
      
      mean_n_predictors > 10 &
        mean_spearman_rank_correlation >= 0.80 &
        mean_top10_overlap_prop >= 0.70 ~
        "High rank stability across plausible values",
      
      mean_n_predictors > 10 &
        mean_spearman_rank_correlation >= 0.60 &
        mean_top10_overlap_prop >= 0.50 ~
        "Moderate rank stability across plausible values",
      
      mean_spearman_rank_correlation >= 0.40 ~
        "Limited but usable rank stability; interpret exact ranks cautiously",
      
      TRUE ~
        "Low rank stability; avoid strong interpretation of exact ranks"
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
  arrange(validation_level, framework_group, framework_context, granularity, model_family)

rank_stability_summary_by_model_family_file <- file.path(
  output_dir,
  "rf_rank_stability_summary_by_model_family.csv"
)

write_csv(rank_stability_summary_by_model_family, rank_stability_summary_by_model_family_file)

cat("\n✅ Model-family-level rank-stability summary saved to:\n", rank_stability_summary_by_model_family_file, "\n")


# --------------------------------------
# 16. Validation-level rank-stability summary
# --------------------------------------

rank_stability_summary_by_model_family_for_level <- rank_stability_summary_by_model_family %>%
  rename(
    family_mean_spearman_rank_correlation = mean_spearman_rank_correlation,
    family_mean_top5_overlap_prop = mean_top5_overlap_prop,
    family_mean_top5_jaccard = mean_top5_jaccard,
    family_mean_top10_overlap_prop = mean_top10_overlap_prop,
    family_mean_top10_jaccard = mean_top10_jaccard,
    family_mean_top20_overlap_prop = mean_top20_overlap_prop,
    family_mean_top20_jaccard = mean_top20_jaccard
  )

rank_stability_summary_by_validation_level <- rank_stability_summary_by_model_family_for_level %>%
  group_by(validation_level) %>%
  summarise(
    n_model_families = n_distinct(model_family),
    model_families_included = paste(sort(unique(model_family)), collapse = "; "),
    mean_n_predictors = mean(mean_n_predictors, na.rm = TRUE),
    
    mean_spearman_rank_correlation = mean(family_mean_spearman_rank_correlation, na.rm = TRUE),
    sd_spearman_rank_correlation = sd(family_mean_spearman_rank_correlation, na.rm = TRUE),
    
    mean_top5_overlap_prop = mean(family_mean_top5_overlap_prop, na.rm = TRUE),
    mean_top5_jaccard = mean(family_mean_top5_jaccard, na.rm = TRUE),
    
    mean_top10_overlap_prop = mean(family_mean_top10_overlap_prop, na.rm = TRUE),
    mean_top10_jaccard = mean(family_mean_top10_jaccard, na.rm = TRUE),
    
    mean_top20_overlap_prop = mean(family_mean_top20_overlap_prop, na.rm = TRUE),
    mean_top20_jaccard = mean(family_mean_top20_jaccard, na.rm = TRUE),
    
    mean_n_predictors_stable_top5_80pct_pvs = mean(mean_n_predictors_stable_top5_80pct_pvs, na.rm = TRUE),
    mean_n_predictors_stable_top10_80pct_pvs = mean(mean_n_predictors_stable_top10_80pct_pvs, na.rm = TRUE),
    mean_n_predictors_stable_top20_80pct_pvs = mean(mean_n_predictors_stable_top20_80pct_pvs, na.rm = TRUE),
    
    any_top10_full_slice = any(any_top10_full_slice, na.rm = TRUE),
    any_top20_full_slice = any(any_top20_full_slice, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    interpretive_note = case_when(
      mean_n_predictors <= 10 &
        mean_spearman_rank_correlation >= 0.80 &
        mean_top5_overlap_prop >= 0.70 ~
        "High rank stability across plausible values; top-10/top-20 overlap less informative because slices are small",
      
      mean_n_predictors <= 10 &
        mean_spearman_rank_correlation >= 0.60 &
        mean_top5_overlap_prop >= 0.50 ~
        "Moderate rank stability across plausible values; top-10/top-20 overlap less informative because slices are small",
      
      mean_n_predictors > 10 &
        mean_spearman_rank_correlation >= 0.80 &
        mean_top10_overlap_prop >= 0.70 ~
        "High rank stability across plausible values",
      
      mean_n_predictors > 10 &
        mean_spearman_rank_correlation >= 0.60 &
        mean_top10_overlap_prop >= 0.50 ~
        "Moderate rank stability across plausible values",
      
      mean_spearman_rank_correlation >= 0.40 ~
        "Limited but usable rank stability; interpret exact ranks cautiously",
      
      TRUE ~
        "Low rank stability; avoid strong interpretation of exact ranks"
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
  arrange(validation_level)

rank_stability_summary_by_validation_level_file <- file.path(
  output_dir,
  "rf_rank_stability_summary_by_validation_level.csv"
)

write_csv(rank_stability_summary_by_validation_level, rank_stability_summary_by_validation_level_file)

cat("\n✅ Validation-level rank-stability summary saved to:\n", rank_stability_summary_by_validation_level_file, "\n")


# --------------------------------------
# 17. Console preview
# --------------------------------------

cat("\n========================================\n")
cat("Preview: Outcome-level rank-stability summary\n")
cat("========================================\n")
print(rank_stability_summary_by_outcome)

cat("\n========================================\n")
cat("Preview: Model-family-level rank-stability summary\n")
cat("========================================\n")
print(rank_stability_summary_by_model_family)

cat("\n========================================\n")
cat("Preview: Validation-level rank-stability summary\n")
cat("========================================\n")
print(rank_stability_summary_by_validation_level)

cat("\n🎉 Random forest rank-stability validation complete.\n")
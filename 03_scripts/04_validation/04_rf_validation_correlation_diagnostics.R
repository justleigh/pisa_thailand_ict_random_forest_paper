# --------------------------------------
# Random Forest Validation Script 04:
# Correlation Diagnostics for RF Predictor Families
# --------------------------------------
#
# Purpose:
# This standalone validation script examines correlations among predictors used
# in the random forest (RF) validation model families.
#
# It addresses a key limitation of RF variable-importance interpretation:
# when predictors are highly correlated, importance can be distributed across
# related variables or concentrated unevenly among them. Therefore, variable
# importance should be interpreted as evidence of predictive salience within
# broader predictor families rather than as a unique independent contribution.
#
# This script mirrors the model-family structure used in:
#   01_rf_validation_performance_checks.R
#   02_rf_validation_rank_stability.R
#   03_rf_validation_sensitivity_checks.R
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
# The script computes Spearman correlations among numerically interpretable
# predictors within each model family. Non-numeric categorical variables are
# excluded from pairwise correlation calculations and reported in variable-level
# diagnostics.
#
# Methodological note:
# Spearman correlations are used because many PISA questionnaire variables are
# ordinal, non-normally distributed, or derived from ordered response categories.
# These diagnostics therefore assess monotonic association rather than assuming
# linear interval-scale relationships.
#
# These correlations are descriptive diagnostics computed on the post-exclusion
# analytic dataset. They are not design-based population correlation estimates
# and are not interpreted inferentially. Their purpose is to identify clusters
# of related predictors that may affect interpretation of RF variable importance.
#
# Outputs:
#
#   1. Variable-level numeric usability diagnostics:
#      rf_correlation_variable_diagnostics.csv
#
#   2. All pairwise predictor correlations:
#      rf_correlation_pairwise_all.csv
#
#   3. High-correlation predictor pairs:
#      rf_correlation_high_pairs.csv
#
#   4. High-correlation pairs involving top-ranked predictors:
#      rf_correlation_top_predictor_high_pairs.csv
#
#   5. High-correlation cluster membership:
#      rf_correlation_cluster_membership.csv
#
#   6. High-correlation cluster summary:
#      rf_correlation_cluster_summary.csv
#
#   7. Compact top correlation clusters for reporting:
#      rf_correlation_top_clusters_for_reporting.csv
#
#   8. Model-family-level correlation diagnostics:
#      rf_correlation_summary_by_model_family.csv
#
#   9. Validation-level correlation diagnostics:
#      rf_correlation_summary_by_validation_level.csv
#
#   10. Compact manuscript-ready summary:
#      rf_correlation_summary_for_manuscript.csv
#
# Key metrics:
#   - Number and proportion of high-correlation pairs: |rho| >= .70
#   - Number and proportion of very-high-correlation pairs: |rho| >= .90
#   - Positive and negative high-correlation pair counts
#   - Number of predictors involved in high-correlation clusters
#   - Largest high-correlation cluster size
#   - Number of top-ranked predictors involved in high-correlation pairs/clusters
#   - Proportion of selected predictors usable for Spearman diagnostics
#
# Interpretation:
# These diagnostics do not invalidate RF variable-importance results. Instead,
# they identify where importance should be interpreted at the level of related
# predictor groups rather than as isolated independent variable effects.
#
# --------------------------------------


# --------------------------------------
# 1. Load libraries
# --------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
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

rank_stability_top20_path <- "output/private/exploratory_data_analysis/2022/random_forests/rf_validation_outputs/rank_stability/rf_rank_stability_top20_predictors_for_reporting.csv"

output_dir <- "output/private/exploratory_data_analysis/2022/random_forests/rf_validation_outputs/correlation_diagnostics"

dir_create(output_dir)

high_corr_threshold <- 0.70
very_high_corr_threshold <- 0.90

minimum_nonmissing_n <- 30
minimum_numeric_conversion_rate <- 0.80


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
# 9. Load top-predictor evidence from rank-stability validation
# --------------------------------------

required_top_predictor_cols <- c(
  "model_family",
  "outcome",
  "variable",
  "mean_rank",
  "mean_norm_importance"
)

if (file_exists(rank_stability_top20_path)) {
  
  top_predictors_raw <- read_csv(rank_stability_top20_path, show_col_types = FALSE)
  
  missing_top_predictor_cols <- setdiff(required_top_predictor_cols, names(top_predictors_raw))
  
  if (length(missing_top_predictor_cols) > 0) {
    stop(
      "Rank-stability top-20 predictor file is missing required columns: ",
      paste(missing_top_predictor_cols, collapse = ", ")
    )
  }
  
  top_predictors <- top_predictors_raw %>%
    select(any_of(c(
      "model_family",
      "validation_level",
      "framework_group",
      "framework_context",
      "granularity",
      "domain_label",
      "construct_label",
      "outcome",
      "variable",
      "mean_rank",
      "mean_norm_importance"
    ))) %>%
    distinct()
  
  cat("✅ Rank-stability top predictor file loaded:", nrow(top_predictors), "rows.\n")
  
} else {
  top_predictors <- tibble(
    model_family = character(),
    outcome = character(),
    variable = character(),
    mean_rank = numeric(),
    mean_norm_importance = numeric()
  )
  
  cat("⚠️ Rank-stability top predictor file not found. Top-predictor flags will be empty.\n")
}

top_predictor_lookup <- top_predictors %>%
  group_by(model_family, variable) %>%
  summarise(
    is_rank_stability_top20 = TRUE,
    n_top20_outcomes = n_distinct(outcome),
    top20_outcomes = paste(sort(unique(outcome)), collapse = "; "),
    mean_rank_across_top20_outputs = mean(mean_rank, na.rm = TRUE),
    mean_norm_importance_across_top20_outputs = mean(mean_norm_importance, na.rm = TRUE),
    cross_domain_top20_predictor = n_distinct(outcome) >= 2,
    all_domain_top20_predictor = n_distinct(outcome) == 3,
    .groups = "drop"
  )


# --------------------------------------
# 10. Numeric conversion and diagnostics helpers
# --------------------------------------

convert_to_numeric_safely <- function(x) {
  if (is.numeric(x) || is.integer(x) || is.logical(x)) {
    return(as.numeric(x))
  }
  
  suppressWarnings(as.numeric(x))
}

diagnose_numeric_usability <- function(x) {
  original_nonmissing_n <- sum(!is.na(x))
  x_num <- convert_to_numeric_safely(x)
  numeric_nonmissing_n <- sum(!is.na(x_num))
  
  numeric_conversion_rate <- ifelse(
    original_nonmissing_n > 0,
    numeric_nonmissing_n / original_nonmissing_n,
    NA_real_
  )
  
  distinct_numeric_n <- dplyr::n_distinct(x_num, na.rm = TRUE)
  sd_numeric <- suppressWarnings(sd(x_num, na.rm = TRUE))
  
  tibble(
    original_nonmissing_n = original_nonmissing_n,
    numeric_nonmissing_n = numeric_nonmissing_n,
    numeric_conversion_rate = numeric_conversion_rate,
    distinct_numeric_n = distinct_numeric_n,
    sd_numeric = sd_numeric,
    usable_for_spearman = numeric_nonmissing_n >= minimum_nonmissing_n &
      numeric_conversion_rate >= minimum_numeric_conversion_rate &
      distinct_numeric_n >= 2 &
      is.finite(sd_numeric) &
      sd_numeric > 0
  )
}

prepare_numeric_predictor_data <- function(df, predictors) {
  predictor_data <- df %>%
    select(all_of(predictors))
  
  diagnostics <- map_dfr(
    predictors,
    function(var_name) {
      diagnose_numeric_usability(predictor_data[[var_name]]) %>%
        mutate(variable = var_name, .before = 1)
    }
  )
  
  usable_predictors <- diagnostics %>%
    filter(usable_for_spearman) %>%
    pull(variable)
  
  numeric_data <- predictor_data %>%
    select(all_of(usable_predictors)) %>%
    mutate(across(everything(), convert_to_numeric_safely))
  
  list(
    diagnostics = diagnostics,
    numeric_data = numeric_data,
    usable_predictors = usable_predictors
  )
}


# --------------------------------------
# 11. Connected-component helper for high-correlation clusters
# --------------------------------------

compute_connected_components <- function(edges, variables) {
  if (nrow(edges) == 0 || length(variables) == 0) {
    return(tibble(
      variable = character(),
      cluster_id = integer()
    ))
  }
  
  adjacency <- setNames(vector("list", length(variables)), variables)
  
  for (i in seq_len(nrow(edges))) {
    v1 <- edges$var1[i]
    v2 <- edges$var2[i]
    
    adjacency[[v1]] <- unique(c(adjacency[[v1]], v2))
    adjacency[[v2]] <- unique(c(adjacency[[v2]], v1))
  }
  
  visited <- setNames(rep(FALSE, length(variables)), variables)
  cluster_rows <- list()
  cluster_id <- 1L
  
  for (v in variables) {
    if (!visited[[v]]) {
      queue <- v
      component <- character()
      visited[[v]] <- TRUE
      
      while (length(queue) > 0) {
        current <- queue[1]
        queue <- queue[-1]
        component <- c(component, current)
        
        neighbours <- adjacency[[current]]
        unvisited_neighbours <- neighbours[!visited[neighbours]]
        
        if (length(unvisited_neighbours) > 0) {
          visited[unvisited_neighbours] <- TRUE
          queue <- c(queue, unvisited_neighbours)
        }
      }
      
      cluster_rows[[cluster_id]] <- tibble(
        variable = component,
        cluster_id = cluster_id
      )
      
      cluster_id <- cluster_id + 1L
    }
  }
  
  bind_rows(cluster_rows)
}


# --------------------------------------
# 12. Run correlation diagnostics for one model family
# --------------------------------------

run_correlation_diagnostics_for_family <- function(family_config, df, mapping) {
  
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
  cat("🎯 Running correlation diagnostics for:", model_family, "\n")
  cat("========================================\n")
  
  predictors <- predictor_function(mapping, df)
  
  cat("📦 Predictors selected:", length(predictors), "\n")
  cat("🔎 Validation level:", validation_level, "\n")
  cat("🔎 Framework/context/granularity:", framework_group, "/", framework_context, "/", granularity, "\n")
  cat("🔎 Domain:", domain_label, "| Construct:", construct_label, "\n")
  
  if (length(predictors) == 0) {
    stop("No predictors selected for model family: ", model_family)
  }
  
  prepared <- prepare_numeric_predictor_data(df, predictors)
  
  variable_diagnostics <- prepared$diagnostics %>%
    mutate(
      model_family = model_family,
      validation_level = validation_level,
      framework_group = framework_group,
      framework_context = framework_context,
      granularity = granularity,
      domain_label = domain_label,
      construct_label = construct_label,
      subset_rule = subset_rule,
      n_predictors_selected = length(predictors),
      .before = 1
    ) %>%
    left_join(
      top_predictor_lookup,
      by = c("model_family", "variable")
    ) %>%
    mutate(
      is_rank_stability_top20 = coalesce(is_rank_stability_top20, FALSE),
      n_top20_outcomes = coalesce(n_top20_outcomes, 0L),
      top20_outcomes = coalesce(top20_outcomes, ""),
      cross_domain_top20_predictor = coalesce(cross_domain_top20_predictor, FALSE),
      all_domain_top20_predictor = coalesce(all_domain_top20_predictor, FALSE)
    )
  
  numeric_data <- prepared$numeric_data
  usable_predictors <- prepared$usable_predictors
  n_usable <- length(usable_predictors)
  
  if (n_usable < 2) {
    pairwise_correlations <- tibble(
      model_family = character(),
      validation_level = character(),
      framework_group = character(),
      framework_context = character(),
      granularity = character(),
      domain_label = character(),
      construct_label = character(),
      subset_rule = character(),
      var1 = character(),
      var2 = character(),
      spearman_rho = numeric(),
      abs_spearman_rho = numeric(),
      n_pairwise_complete = integer(),
      high_correlation_pair = logical(),
      very_high_correlation_pair = logical()
    )
    
    high_pairs <- pairwise_correlations
    top_predictor_high_pairs <- pairwise_correlations
    cluster_membership <- tibble()
    cluster_summary <- tibble()
    
    return(list(
      variable_diagnostics = variable_diagnostics,
      pairwise_correlations = pairwise_correlations,
      high_pairs = high_pairs,
      top_predictor_high_pairs = top_predictor_high_pairs,
      cluster_membership = cluster_membership,
      cluster_summary = cluster_summary
    ))
  }
  
  cor_mat <- suppressWarnings(
    cor(
      numeric_data,
      method = "spearman",
      use = "pairwise.complete.obs"
    )
  )
  
  complete_mat <- crossprod(!is.na(as.matrix(numeric_data)))
  upper_idx <- which(upper.tri(cor_mat), arr.ind = TRUE)
  
  pairwise_correlations <- tibble(
    model_family = model_family,
    validation_level = validation_level,
    framework_group = framework_group,
    framework_context = framework_context,
    granularity = granularity,
    domain_label = domain_label,
    construct_label = construct_label,
    subset_rule = subset_rule,
    var1 = colnames(cor_mat)[upper_idx[, 1]],
    var2 = colnames(cor_mat)[upper_idx[, 2]],
    spearman_rho = cor_mat[upper_idx],
    abs_spearman_rho = abs(spearman_rho),
    n_pairwise_complete = as.integer(complete_mat[upper_idx])
  ) %>%
    filter(
      !is.na(spearman_rho),
      n_pairwise_complete >= minimum_nonmissing_n
    ) %>%
    left_join(
      top_predictor_lookup %>%
        select(
          model_family,
          var1 = variable,
          var1_is_rank_stability_top20 = is_rank_stability_top20,
          var1_n_top20_outcomes = n_top20_outcomes,
          var1_top20_outcomes = top20_outcomes,
          var1_cross_domain_top20_predictor = cross_domain_top20_predictor,
          var1_all_domain_top20_predictor = all_domain_top20_predictor
        ),
      by = c("model_family", "var1")
    ) %>%
    left_join(
      top_predictor_lookup %>%
        select(
          model_family,
          var2 = variable,
          var2_is_rank_stability_top20 = is_rank_stability_top20,
          var2_n_top20_outcomes = n_top20_outcomes,
          var2_top20_outcomes = top20_outcomes,
          var2_cross_domain_top20_predictor = cross_domain_top20_predictor,
          var2_all_domain_top20_predictor = all_domain_top20_predictor
        ),
      by = c("model_family", "var2")
    ) %>%
    mutate(
      var1_is_rank_stability_top20 = coalesce(var1_is_rank_stability_top20, FALSE),
      var2_is_rank_stability_top20 = coalesce(var2_is_rank_stability_top20, FALSE),
      var1_n_top20_outcomes = coalesce(var1_n_top20_outcomes, 0L),
      var2_n_top20_outcomes = coalesce(var2_n_top20_outcomes, 0L),
      var1_top20_outcomes = coalesce(var1_top20_outcomes, ""),
      var2_top20_outcomes = coalesce(var2_top20_outcomes, ""),
      var1_cross_domain_top20_predictor = coalesce(var1_cross_domain_top20_predictor, FALSE),
      var2_cross_domain_top20_predictor = coalesce(var2_cross_domain_top20_predictor, FALSE),
      var1_all_domain_top20_predictor = coalesce(var1_all_domain_top20_predictor, FALSE),
      var2_all_domain_top20_predictor = coalesce(var2_all_domain_top20_predictor, FALSE),
      either_var_is_rank_stability_top20 = var1_is_rank_stability_top20 | var2_is_rank_stability_top20,
      both_vars_are_rank_stability_top20 = var1_is_rank_stability_top20 & var2_is_rank_stability_top20,
      either_var_is_cross_domain_top20 = var1_cross_domain_top20_predictor | var2_cross_domain_top20_predictor,
      both_vars_are_cross_domain_top20 = var1_cross_domain_top20_predictor & var2_cross_domain_top20_predictor,
      either_var_is_all_domain_top20 = var1_all_domain_top20_predictor | var2_all_domain_top20_predictor,
      both_vars_are_all_domain_top20 = var1_all_domain_top20_predictor & var2_all_domain_top20_predictor,
      high_correlation_pair = abs_spearman_rho >= high_corr_threshold,
      very_high_correlation_pair = abs_spearman_rho >= very_high_corr_threshold
    ) %>%
    arrange(desc(abs_spearman_rho), var1, var2)
  
  high_pairs <- pairwise_correlations %>%
    filter(high_correlation_pair)
  
  top_predictor_high_pairs <- high_pairs %>%
    filter(either_var_is_rank_stability_top20)
  
  high_corr_variables <- high_pairs %>%
    select(var1, var2) %>%
    pivot_longer(everything(), values_to = "variable") %>%
    distinct(variable) %>%
    pull(variable)
  
  cluster_membership <- compute_connected_components(
    edges = high_pairs %>% select(var1, var2),
    variables = high_corr_variables
  ) %>%
    mutate(
      model_family = model_family,
      validation_level = validation_level,
      framework_group = framework_group,
      framework_context = framework_context,
      granularity = granularity,
      domain_label = domain_label,
      construct_label = construct_label,
      .before = 1
    ) %>%
    left_join(
      top_predictor_lookup,
      by = c("model_family", "variable")
    ) %>%
    mutate(
      is_rank_stability_top20 = coalesce(is_rank_stability_top20, FALSE),
      n_top20_outcomes = coalesce(n_top20_outcomes, 0L),
      top20_outcomes = coalesce(top20_outcomes, ""),
      cross_domain_top20_predictor = coalesce(cross_domain_top20_predictor, FALSE),
      all_domain_top20_predictor = coalesce(all_domain_top20_predictor, FALSE)
    )
  
  if (nrow(cluster_membership) > 0) {
    
    high_pairs_with_cluster <- high_pairs %>%
      left_join(
        cluster_membership %>%
          select(variable, cluster_id),
        by = c("var1" = "variable")
      ) %>%
      rename(cluster_id_var1 = cluster_id) %>%
      left_join(
        cluster_membership %>%
          select(variable, cluster_id),
        by = c("var2" = "variable")
      ) %>%
      rename(cluster_id_var2 = cluster_id) %>%
      filter(cluster_id_var1 == cluster_id_var2) %>%
      mutate(cluster_id = cluster_id_var1)
    
    cluster_summary <- cluster_membership %>%
      group_by(
        model_family,
        validation_level,
        framework_group,
        framework_context,
        granularity,
        domain_label,
        construct_label,
        cluster_id
      ) %>%
      summarise(
        cluster_size = n_distinct(variable),
        cluster_variables = paste(sort(unique(variable)), collapse = "; "),
        n_rank_stability_top20_variables = sum(is_rank_stability_top20, na.rm = TRUE),
        rank_stability_top20_variables = paste(sort(unique(variable[is_rank_stability_top20])), collapse = "; "),
        n_cross_domain_top20_variables = sum(cross_domain_top20_predictor, na.rm = TRUE),
        cross_domain_top20_variables = paste(sort(unique(variable[cross_domain_top20_predictor])), collapse = "; "),
        n_all_domain_top20_variables = sum(all_domain_top20_predictor, na.rm = TRUE),
        all_domain_top20_variables = paste(sort(unique(variable[all_domain_top20_predictor])), collapse = "; "),
        .groups = "drop"
      ) %>%
      left_join(
        high_pairs_with_cluster %>%
          group_by(cluster_id) %>%
          summarise(
            max_abs_spearman_rho_within_cluster = max(abs_spearman_rho, na.rm = TRUE),
            mean_abs_spearman_rho_within_cluster = mean(abs_spearman_rho, na.rm = TRUE),
            n_high_pairs_within_cluster = n(),
            n_very_high_pairs_within_cluster = sum(very_high_correlation_pair, na.rm = TRUE),
            n_high_positive_pairs_within_cluster = sum(spearman_rho > 0, na.rm = TRUE),
            n_high_negative_pairs_within_cluster = sum(spearman_rho < 0, na.rm = TRUE),
            .groups = "drop"
          ),
        by = "cluster_id"
      ) %>%
      arrange(desc(cluster_size), desc(max_abs_spearman_rho_within_cluster))
    
  } else {
    cluster_summary <- tibble(
      model_family = character(),
      validation_level = character(),
      framework_group = character(),
      framework_context = character(),
      granularity = character(),
      domain_label = character(),
      construct_label = character(),
      cluster_id = integer(),
      cluster_size = integer(),
      cluster_variables = character(),
      n_rank_stability_top20_variables = integer(),
      rank_stability_top20_variables = character(),
      n_cross_domain_top20_variables = integer(),
      cross_domain_top20_variables = character(),
      n_all_domain_top20_variables = integer(),
      all_domain_top20_variables = character(),
      max_abs_spearman_rho_within_cluster = numeric(),
      mean_abs_spearman_rho_within_cluster = numeric(),
      n_high_pairs_within_cluster = integer(),
      n_very_high_pairs_within_cluster = integer(),
      n_high_positive_pairs_within_cluster = integer(),
      n_high_negative_pairs_within_cluster = integer()
    )
  }
  
  list(
    variable_diagnostics = variable_diagnostics,
    pairwise_correlations = pairwise_correlations,
    high_pairs = high_pairs,
    top_predictor_high_pairs = top_predictor_high_pairs,
    cluster_membership = cluster_membership,
    cluster_summary = cluster_summary
  )
}


# --------------------------------------
# 13. Main correlation diagnostics loop
# --------------------------------------

all_results <- map(
  model_families,
  ~ run_correlation_diagnostics_for_family(.x, df = df, mapping = mapping)
)

variable_diagnostics <- map_dfr(all_results, "variable_diagnostics")
pairwise_correlations <- map_dfr(all_results, "pairwise_correlations")
high_pairs <- map_dfr(all_results, "high_pairs")
top_predictor_high_pairs <- map_dfr(all_results, "top_predictor_high_pairs")
cluster_membership <- map_dfr(all_results, "cluster_membership")
cluster_summary <- map_dfr(all_results, "cluster_summary")


# --------------------------------------
# 14. Save detailed outputs
# --------------------------------------

variable_diagnostics_file <- file.path(output_dir, "rf_correlation_variable_diagnostics.csv")
pairwise_correlations_file <- file.path(output_dir, "rf_correlation_pairwise_all.csv")
high_pairs_file <- file.path(output_dir, "rf_correlation_high_pairs.csv")
top_predictor_high_pairs_file <- file.path(output_dir, "rf_correlation_top_predictor_high_pairs.csv")
cluster_membership_file <- file.path(output_dir, "rf_correlation_cluster_membership.csv")
cluster_summary_file <- file.path(output_dir, "rf_correlation_cluster_summary.csv")

write_csv(variable_diagnostics, variable_diagnostics_file)
write_csv(pairwise_correlations, pairwise_correlations_file)
write_csv(high_pairs, high_pairs_file)
write_csv(top_predictor_high_pairs, top_predictor_high_pairs_file)
write_csv(cluster_membership, cluster_membership_file)
write_csv(cluster_summary, cluster_summary_file)

cat("\n✅ Variable diagnostics saved to:\n", variable_diagnostics_file, "\n")
cat("✅ Pairwise correlations saved to:\n", pairwise_correlations_file, "\n")
cat("✅ High-correlation pairs saved to:\n", high_pairs_file, "\n")
cat("✅ Top-predictor high-correlation pairs saved to:\n", top_predictor_high_pairs_file, "\n")
cat("✅ Cluster membership saved to:\n", cluster_membership_file, "\n")
cat("✅ Cluster summary saved to:\n", cluster_summary_file, "\n")


# --------------------------------------
# 14b. Compact top correlation clusters for reporting
# --------------------------------------

top_correlation_clusters_for_reporting <- cluster_summary %>%
  group_by(model_family) %>%
  arrange(desc(cluster_size), desc(max_abs_spearman_rho_within_cluster), .by_group = TRUE) %>%
  slice_head(n = 10) %>%
  ungroup()

top_correlation_clusters_for_reporting_file <- file.path(
  output_dir,
  "rf_correlation_top_clusters_for_reporting.csv"
)

write_csv(top_correlation_clusters_for_reporting, top_correlation_clusters_for_reporting_file)

cat("\n✅ Top correlation clusters for reporting saved to:\n", top_correlation_clusters_for_reporting_file, "\n")


# --------------------------------------
# 15. Model-family-level summary
# --------------------------------------

summary_by_model_family <- variable_diagnostics %>%
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
    n_predictors_selected = first(n_predictors_selected),
    n_predictors_usable_for_spearman = sum(usable_for_spearman, na.rm = TRUE),
    n_predictors_not_usable_for_spearman = sum(!usable_for_spearman, na.rm = TRUE),
    proportion_predictors_usable_for_spearman = if_else(
      n_predictors_selected > 0,
      n_predictors_usable_for_spearman / n_predictors_selected,
      NA_real_
    ),
    n_rank_stability_top20_predictors = sum(is_rank_stability_top20, na.rm = TRUE),
    n_cross_domain_top20_predictors = sum(cross_domain_top20_predictor, na.rm = TRUE),
    n_all_domain_top20_predictors = sum(all_domain_top20_predictor, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    pairwise_correlations %>%
      group_by(model_family) %>%
      summarise(
        n_pairwise_correlations = n(),
        mean_abs_spearman_rho = mean(abs_spearman_rho, na.rm = TRUE),
        median_abs_spearman_rho = median(abs_spearman_rho, na.rm = TRUE),
        max_abs_spearman_rho = max(abs_spearman_rho, na.rm = TRUE),
        n_high_correlation_pairs = sum(high_correlation_pair, na.rm = TRUE),
        n_very_high_correlation_pairs = sum(very_high_correlation_pair, na.rm = TRUE),
        n_high_positive_correlation_pairs = sum(high_correlation_pair & spearman_rho > 0, na.rm = TRUE),
        n_high_negative_correlation_pairs = sum(high_correlation_pair & spearman_rho < 0, na.rm = TRUE),
        n_very_high_positive_correlation_pairs = sum(very_high_correlation_pair & spearman_rho > 0, na.rm = TRUE),
        n_very_high_negative_correlation_pairs = sum(very_high_correlation_pair & spearman_rho < 0, na.rm = TRUE),
        n_high_pairs_involving_top20_predictor = sum(high_correlation_pair & either_var_is_rank_stability_top20, na.rm = TRUE),
        n_high_pairs_between_top20_predictors = sum(high_correlation_pair & both_vars_are_rank_stability_top20, na.rm = TRUE),
        n_high_pairs_involving_cross_domain_top20_predictor = sum(high_correlation_pair & either_var_is_cross_domain_top20, na.rm = TRUE),
        n_high_pairs_between_cross_domain_top20_predictors = sum(high_correlation_pair & both_vars_are_cross_domain_top20, na.rm = TRUE),
        n_high_pairs_involving_all_domain_top20_predictor = sum(high_correlation_pair & either_var_is_all_domain_top20, na.rm = TRUE),
        n_high_pairs_between_all_domain_top20_predictors = sum(high_correlation_pair & both_vars_are_all_domain_top20, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "model_family"
  ) %>%
  left_join(
    cluster_summary %>%
      group_by(model_family) %>%
      summarise(
        n_high_correlation_clusters = n_distinct(cluster_id),
        largest_high_correlation_cluster_size = max(cluster_size, na.rm = TRUE),
        n_clusters_with_top20_predictor = sum(n_rank_stability_top20_variables > 0, na.rm = TRUE),
        max_top20_predictors_in_single_cluster = max(n_rank_stability_top20_variables, na.rm = TRUE),
        n_clusters_with_cross_domain_top20_predictor = sum(n_cross_domain_top20_variables > 0, na.rm = TRUE),
        max_cross_domain_top20_predictors_in_single_cluster = max(n_cross_domain_top20_variables, na.rm = TRUE),
        n_clusters_with_all_domain_top20_predictor = sum(n_all_domain_top20_variables > 0, na.rm = TRUE),
        max_all_domain_top20_predictors_in_single_cluster = max(n_all_domain_top20_variables, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "model_family"
  ) %>%
  mutate(
    across(
      c(
        n_pairwise_correlations,
        n_high_correlation_pairs,
        n_very_high_correlation_pairs,
        n_high_positive_correlation_pairs,
        n_high_negative_correlation_pairs,
        n_very_high_positive_correlation_pairs,
        n_very_high_negative_correlation_pairs,
        n_high_pairs_involving_top20_predictor,
        n_high_pairs_between_top20_predictors,
        n_high_pairs_involving_cross_domain_top20_predictor,
        n_high_pairs_between_cross_domain_top20_predictors,
        n_high_pairs_involving_all_domain_top20_predictor,
        n_high_pairs_between_all_domain_top20_predictors,
        n_high_correlation_clusters,
        largest_high_correlation_cluster_size,
        n_clusters_with_top20_predictor,
        max_top20_predictors_in_single_cluster,
        n_clusters_with_cross_domain_top20_predictor,
        max_cross_domain_top20_predictors_in_single_cluster,
        n_clusters_with_all_domain_top20_predictor,
        max_all_domain_top20_predictors_in_single_cluster
      ),
      ~ coalesce(.x, 0)
    ),
    proportion_high_correlation_pairs = if_else(
      n_pairwise_correlations > 0,
      n_high_correlation_pairs / n_pairwise_correlations,
      NA_real_
    ),
    proportion_very_high_correlation_pairs = if_else(
      n_pairwise_correlations > 0,
      n_very_high_correlation_pairs / n_pairwise_correlations,
      NA_real_
    ),
    interpretive_note = case_when(
      n_high_correlation_pairs == 0 ~
        "No high-correlation predictor pairs detected at the specified threshold.",
      n_high_pairs_involving_top20_predictor == 0 ~
        "High-correlation pairs detected, but none involve rank-stability top-20 predictors.",
      n_high_pairs_between_top20_predictors > 0 ~
        "Some rank-stability top-20 predictors are highly correlated with other top-20 predictors; interpret exact variable ranks as predictor-family signals.",
      n_high_pairs_involving_top20_predictor > 0 ~
        "Some rank-stability top-20 predictors are highly correlated with other predictors; interpret exact variable ranks cautiously.",
      TRUE ~
        "Correlation diagnostics completed."
    )
  ) %>%
  arrange(validation_level, framework_group, framework_context, granularity, model_family)

summary_by_model_family_file <- file.path(
  output_dir,
  "rf_correlation_summary_by_model_family.csv"
)

write_csv(summary_by_model_family, summary_by_model_family_file)

cat("\n✅ Model-family-level correlation summary saved to:\n", summary_by_model_family_file, "\n")


# --------------------------------------
# 16. Validation-level summary
# --------------------------------------

summary_by_validation_level <- summary_by_model_family %>%
  group_by(validation_level) %>%
  summarise(
    n_model_families = n_distinct(model_family),
    model_families_included = paste(sort(unique(model_family)), collapse = "; "),
    mean_n_predictors_selected = mean(n_predictors_selected, na.rm = TRUE),
    mean_n_predictors_usable_for_spearman = mean(n_predictors_usable_for_spearman, na.rm = TRUE),
    mean_proportion_predictors_usable_for_spearman = mean(proportion_predictors_usable_for_spearman, na.rm = TRUE),
    total_pairwise_correlations = sum(n_pairwise_correlations, na.rm = TRUE),
    total_high_correlation_pairs = sum(n_high_correlation_pairs, na.rm = TRUE),
    total_very_high_correlation_pairs = sum(n_very_high_correlation_pairs, na.rm = TRUE),
    total_high_positive_correlation_pairs = sum(n_high_positive_correlation_pairs, na.rm = TRUE),
    total_high_negative_correlation_pairs = sum(n_high_negative_correlation_pairs, na.rm = TRUE),
    total_very_high_positive_correlation_pairs = sum(n_very_high_positive_correlation_pairs, na.rm = TRUE),
    total_very_high_negative_correlation_pairs = sum(n_very_high_negative_correlation_pairs, na.rm = TRUE),
    total_high_pairs_involving_top20_predictor = sum(n_high_pairs_involving_top20_predictor, na.rm = TRUE),
    total_high_pairs_between_top20_predictors = sum(n_high_pairs_between_top20_predictors, na.rm = TRUE),
    total_high_pairs_involving_cross_domain_top20_predictor = sum(n_high_pairs_involving_cross_domain_top20_predictor, na.rm = TRUE),
    total_high_pairs_between_cross_domain_top20_predictors = sum(n_high_pairs_between_cross_domain_top20_predictors, na.rm = TRUE),
    total_high_pairs_involving_all_domain_top20_predictor = sum(n_high_pairs_involving_all_domain_top20_predictor, na.rm = TRUE),
    total_high_pairs_between_all_domain_top20_predictors = sum(n_high_pairs_between_all_domain_top20_predictors, na.rm = TRUE),
    mean_proportion_high_correlation_pairs = mean(proportion_high_correlation_pairs, na.rm = TRUE),
    mean_proportion_very_high_correlation_pairs = mean(proportion_very_high_correlation_pairs, na.rm = TRUE),
    mean_largest_high_correlation_cluster_size = mean(largest_high_correlation_cluster_size, na.rm = TRUE),
    max_largest_high_correlation_cluster_size = max(largest_high_correlation_cluster_size, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    interpretive_note = case_when(
      total_high_correlation_pairs == 0 ~
        "No high-correlation predictor pairs detected at this validation level.",
      total_high_pairs_between_top20_predictors > 0 ~
        "Some top-ranked predictors are mutually correlated; interpret exact ranks as signals within related predictor families.",
      total_high_pairs_involving_top20_predictor > 0 ~
        "Some top-ranked predictors are correlated with other predictors; interpret variable importance cautiously.",
      TRUE ~
        "High-correlation pairs exist but do not involve top-ranked predictors."
    )
  ) %>%
  arrange(validation_level)

summary_by_validation_level_file <- file.path(
  output_dir,
  "rf_correlation_summary_by_validation_level.csv"
)

write_csv(summary_by_validation_level, summary_by_validation_level_file)

cat("\n✅ Validation-level correlation summary saved to:\n", summary_by_validation_level_file, "\n")


# --------------------------------------
# 17. Compact manuscript-ready summary
# --------------------------------------

summary_for_manuscript <- summary_by_validation_level %>%
  select(
    validation_level,
    n_model_families,
    mean_n_predictors_selected,
    mean_n_predictors_usable_for_spearman,
    mean_proportion_predictors_usable_for_spearman,
    total_pairwise_correlations,
    total_high_correlation_pairs,
    total_very_high_correlation_pairs,
    total_high_positive_correlation_pairs,
    total_high_negative_correlation_pairs,
    total_very_high_positive_correlation_pairs,
    total_very_high_negative_correlation_pairs,
    total_high_pairs_involving_top20_predictor,
    total_high_pairs_between_top20_predictors,
    total_high_pairs_involving_cross_domain_top20_predictor,
    total_high_pairs_between_cross_domain_top20_predictors,
    total_high_pairs_involving_all_domain_top20_predictor,
    total_high_pairs_between_all_domain_top20_predictors,
    mean_proportion_high_correlation_pairs,
    mean_proportion_very_high_correlation_pairs,
    mean_largest_high_correlation_cluster_size,
    max_largest_high_correlation_cluster_size,
    interpretive_note
  )

summary_for_manuscript_file <- file.path(
  output_dir,
  "rf_correlation_summary_for_manuscript.csv"
)

write_csv(summary_for_manuscript, summary_for_manuscript_file)

cat("\n✅ Manuscript-ready correlation summary saved to:\n", summary_for_manuscript_file, "\n")


# --------------------------------------
# 18. Row-count check
# --------------------------------------

cat("\nRow-count check:\n")
cat("Variable diagnostics rows:", nrow(variable_diagnostics), "\n")
cat("Pairwise correlation rows:", nrow(pairwise_correlations), "\n")
cat("High-correlation pair rows:", nrow(high_pairs), "\n")
cat("Top-predictor high-correlation pair rows:", nrow(top_predictor_high_pairs), "\n")
cat("Cluster membership rows:", nrow(cluster_membership), "\n")
cat("Cluster summary rows:", nrow(cluster_summary), "\n")
cat("Top correlation clusters for reporting rows:", nrow(top_correlation_clusters_for_reporting), "\n")
cat("Model-family summary rows:", nrow(summary_by_model_family), "\n")
cat("Validation-level summary rows:", nrow(summary_by_validation_level), "\n")
cat("Manuscript summary rows:", nrow(summary_for_manuscript), "\n")


# --------------------------------------
# 19. Console preview
# --------------------------------------

cat("\n========================================\n")
cat("Preview: Model-family-level correlation summary\n")
cat("========================================\n")
print(summary_by_model_family)

cat("\n========================================\n")
cat("Preview: Validation-level correlation summary\n")
cat("========================================\n")
print(summary_by_validation_level)

cat("\n========================================\n")
cat("Preview: Manuscript-ready correlation summary\n")
cat("========================================\n")
print(summary_for_manuscript)

cat("\n🎉 Random forest correlation diagnostics complete.\n")
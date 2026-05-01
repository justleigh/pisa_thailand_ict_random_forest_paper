# --------------------------------------
# Random Forest Validation Script 05:
# Validation Summary Tables for Manuscript, Appendix, and Audit Trail
# --------------------------------------
#
# Purpose:
# This standalone synthesis script combines the outputs from the four RF
# validation scripts:
#
#   01_rf_validation_performance_checks.R
#   02_rf_validation_rank_stability.R
#   03_rf_validation_sensitivity_checks.R
#   04_rf_validation_correlation_diagnostics.R
#
# It creates:
#
#   A. Compact manuscript-ready validation table
#   B. Appendix-ready validation tables
#   C. Comprehensive validation audit tables
#   D. Text-friendly key statistics for manuscript writing
#
# The aim is to provide concise, transparent, and publication-defensible
# validation evidence for the exploratory random forest procedure.
#
# The validation evidence addresses four methodological questions:
#
#   1. Predictive performance:
#      Do the RF models have meaningful OOB predictive signal?
#
#   2. Rank stability:
#      Are variable-importance rankings stable across plausible values?
#
#   3. Sensitivity:
#      Are rankings robust to alternative RF specifications and importance
#      metrics?
#
#   4. Correlation diagnostics:
#      Are top-ranked predictors affected by high correlation with other
#      predictors?
#
# Interpretation:
# These validation tables do not convert the RF analysis into causal inference,
# effect-size estimation, or design-based population inference. They support the
# use of RF variable importance as an exploratory predictive-screening tool.
#
# Main outputs:
#
#   1. rf_validation_summary_for_main_text.csv
#      Compact table recommended for the manuscript body.
#
#   2. rf_validation_summary_for_appendix_by_model_family.csv
#      Model-family-level validation table for appendix/supplement.
#
#   3. rf_validation_sensitivity_for_appendix.csv
#      Sensitivity table by validation level and comparison type.
#
#   4. rf_validation_correlation_for_appendix.csv
#      Correlation diagnostics table for appendix/supplement.
#
#   5. rf_validation_master_audit_by_validation_level.csv
#      Full validation-level audit table.
#
#   6. rf_validation_master_audit_by_model_family.csv
#      Full model-family-level audit table.
#
#   7. rf_validation_key_statistics_for_text.csv
#      Compact values for manuscript prose.
#
# Expected final row counts:
#
#   Main-text summary rows: 3
#   Appendix model-family rows: 9
#   Appendix sensitivity rows: 9
#   Appendix correlation rows: 3
#   Master validation-level audit rows: 3
#   Master model-family audit rows: 9
#   Key statistics rows: 18
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
  library(tidyr)
  library(fs)
  library(purrr)
})


# --------------------------------------
# 2. Manual paths
# --------------------------------------

base_validation_dir <- "output/private/exploratory_data_analysis/2022/random_forests/rf_validation_outputs"

performance_dir <- file.path(base_validation_dir, "performance_checks")
rank_stability_dir <- file.path(base_validation_dir, "rank_stability")
sensitivity_dir <- file.path(base_validation_dir, "sensitivity_checks")
correlation_dir <- file.path(base_validation_dir, "correlation_diagnostics")

output_dir <- file.path(base_validation_dir, "validation_summary_tables")

dir_create(output_dir)


# --------------------------------------
# 3. Input file paths
# --------------------------------------

performance_by_validation_level_path <- file.path(
  performance_dir,
  "rf_validation_performance_summary_by_validation_level.csv"
)

performance_by_model_family_path <- file.path(
  performance_dir,
  "rf_validation_performance_summary_by_model_family.csv"
)

rank_by_validation_level_path <- file.path(
  rank_stability_dir,
  "rf_rank_stability_summary_by_validation_level.csv"
)

rank_by_model_family_path <- file.path(
  rank_stability_dir,
  "rf_rank_stability_summary_by_model_family.csv"
)

sensitivity_for_manuscript_path <- file.path(
  sensitivity_dir,
  "rf_sensitivity_summary_for_manuscript.csv"
)

sensitivity_by_model_family_path <- file.path(
  sensitivity_dir,
  "rf_sensitivity_summary_by_model_family.csv"
)

correlation_for_manuscript_path <- file.path(
  correlation_dir,
  "rf_correlation_summary_for_manuscript.csv"
)

correlation_by_model_family_path <- file.path(
  correlation_dir,
  "rf_correlation_summary_by_model_family.csv"
)


# --------------------------------------
# 4. Required-file checks
# --------------------------------------

required_files <- c(
  performance_by_validation_level_path,
  performance_by_model_family_path,
  rank_by_validation_level_path,
  rank_by_model_family_path,
  sensitivity_for_manuscript_path,
  sensitivity_by_model_family_path,
  correlation_for_manuscript_path,
  correlation_by_model_family_path
)

missing_files <- required_files[!file_exists(required_files)]

if (length(missing_files) > 0) {
  stop(
    "The following required validation output files are missing:\n",
    paste(missing_files, collapse = "\n")
  )
}

cat("✅ All required validation files found.\n")


# --------------------------------------
# 5. Load validation outputs
# --------------------------------------

performance_by_validation_level <- read_csv(
  performance_by_validation_level_path,
  show_col_types = FALSE
)

performance_by_model_family <- read_csv(
  performance_by_model_family_path,
  show_col_types = FALSE
)

rank_by_validation_level <- read_csv(
  rank_by_validation_level_path,
  show_col_types = FALSE
)

rank_by_model_family <- read_csv(
  rank_by_model_family_path,
  show_col_types = FALSE
)

sensitivity_for_manuscript <- read_csv(
  sensitivity_for_manuscript_path,
  show_col_types = FALSE
)

sensitivity_by_model_family <- read_csv(
  sensitivity_by_model_family_path,
  show_col_types = FALSE
)

correlation_for_manuscript <- read_csv(
  correlation_for_manuscript_path,
  show_col_types = FALSE
)

correlation_by_model_family <- read_csv(
  correlation_by_model_family_path,
  show_col_types = FALSE
)

cat("✅ Validation outputs loaded.\n")


# --------------------------------------
# 6. Column validation helpers
# --------------------------------------

check_required_cols <- function(data, required_cols, data_name) {
  missing_cols <- setdiff(required_cols, names(data))
  
  if (length(missing_cols) > 0) {
    stop(
      data_name,
      " is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  invisible(TRUE)
}

check_required_cols(
  performance_by_validation_level,
  c(
    "validation_level",
    "n_model_families",
    "mean_oob_r2",
    "mean_oob_rmse"
  ),
  "performance_by_validation_level"
)

check_required_cols(
  performance_by_model_family,
  c(
    "model_family",
    "validation_level",
    "mean_n_predictors",
    "mean_oob_r2",
    "mean_oob_rmse"
  ),
  "performance_by_model_family"
)

check_required_cols(
  rank_by_validation_level,
  c(
    "validation_level",
    "mean_spearman_rank_correlation",
    "mean_top10_overlap_prop",
    "mean_top20_overlap_prop"
  ),
  "rank_by_validation_level"
)

check_required_cols(
  rank_by_model_family,
  c(
    "model_family",
    "validation_level",
    "mean_spearman_rank_correlation",
    "mean_top10_overlap_prop",
    "mean_top20_overlap_prop"
  ),
  "rank_by_model_family"
)

check_required_cols(
  sensitivity_for_manuscript,
  c(
    "validation_level",
    "comparison_id",
    "n_model_families",
    "mean_n_predictors_compared",
    "mean_spearman_rank_correlation",
    "sd_spearman_rank_correlation",
    "mean_top10_overlap_prop",
    "mean_top20_overlap_prop",
    "mean_top10_jaccard",
    "mean_top20_jaccard",
    "interpretive_note"
  ),
  "sensitivity_for_manuscript"
)

check_required_cols(
  sensitivity_by_model_family,
  c(
    "model_family",
    "validation_level",
    "comparison_id",
    "mean_spearman_rank_correlation",
    "mean_top10_overlap_prop",
    "mean_top20_overlap_prop"
  ),
  "sensitivity_by_model_family"
)

check_required_cols(
  correlation_for_manuscript,
  c(
    "validation_level",
    "n_model_families",
    "mean_n_predictors_selected",
    "mean_n_predictors_usable_for_spearman",
    "mean_proportion_predictors_usable_for_spearman",
    "total_pairwise_correlations",
    "total_high_correlation_pairs",
    "total_very_high_correlation_pairs",
    "total_high_positive_correlation_pairs",
    "total_high_negative_correlation_pairs",
    "total_very_high_positive_correlation_pairs",
    "total_very_high_negative_correlation_pairs",
    "total_high_pairs_involving_top20_predictor",
    "total_high_pairs_between_top20_predictors",
    "mean_proportion_high_correlation_pairs",
    "mean_proportion_very_high_correlation_pairs",
    "mean_largest_high_correlation_cluster_size",
    "max_largest_high_correlation_cluster_size",
    "interpretive_note"
  ),
  "correlation_for_manuscript"
)

check_required_cols(
  correlation_by_model_family,
  c(
    "model_family",
    "validation_level",
    "n_pairwise_correlations",
    "n_high_correlation_pairs",
    "n_very_high_correlation_pairs",
    "n_high_pairs_involving_top20_predictor",
    "n_high_pairs_between_top20_predictors",
    "proportion_high_correlation_pairs",
    "interpretive_note"
  ),
  "correlation_by_model_family"
)

cat("✅ Required columns validated.\n")


# --------------------------------------
# 6b. Expected row-count checks
# --------------------------------------

expected_counts <- c(
  performance_by_validation_level = 3,
  performance_by_model_family = 9,
  rank_by_validation_level = 3,
  rank_by_model_family = 9,
  sensitivity_for_manuscript = 9,
  sensitivity_by_model_family = 27,
  correlation_for_manuscript = 3,
  correlation_by_model_family = 9
)

actual_counts <- c(
  performance_by_validation_level = nrow(performance_by_validation_level),
  performance_by_model_family = nrow(performance_by_model_family),
  rank_by_validation_level = nrow(rank_by_validation_level),
  rank_by_model_family = nrow(rank_by_model_family),
  sensitivity_for_manuscript = nrow(sensitivity_for_manuscript),
  sensitivity_by_model_family = nrow(sensitivity_by_model_family),
  correlation_for_manuscript = nrow(correlation_for_manuscript),
  correlation_by_model_family = nrow(correlation_by_model_family)
)

row_count_problems <- names(expected_counts)[actual_counts != expected_counts]

if (length(row_count_problems) > 0) {
  stop(
    "Unexpected row counts detected:\n",
    paste(
      row_count_problems,
      "expected",
      expected_counts[row_count_problems],
      "but found",
      actual_counts[row_count_problems],
      collapse = "\n"
    )
  )
}

cat("✅ Expected row counts validated.\n")


# --------------------------------------
# 6c. Expected validation-level checks
# --------------------------------------

expected_validation_levels <- c(
  "global_baseline",
  "framework_domain",
  "framework_construct_spot_check"
)

validation_level_sources <- list(
  performance_by_validation_level = performance_by_validation_level$validation_level,
  rank_by_validation_level = rank_by_validation_level$validation_level,
  sensitivity_for_manuscript = sensitivity_for_manuscript$validation_level,
  correlation_for_manuscript = correlation_for_manuscript$validation_level
)

validation_level_problems <- map_lgl(
  validation_level_sources,
  ~ !setequal(unique(.x), expected_validation_levels)
)

if (any(validation_level_problems)) {
  stop(
    "Unexpected validation levels detected in: ",
    paste(names(validation_level_problems)[validation_level_problems], collapse = ", ")
  )
}

cat("✅ Expected validation levels validated.\n")


# --------------------------------------
# 6d. Expected sensitivity-comparison checks
# --------------------------------------

expected_sensitivity_comparisons <- c(
  "metric_sensitivity_nodepurity_vs_permutation",
  "ntree_sensitivity_500_vs_1000",
  "mtry_sensitivity_default_vs_sqrt"
)

actual_sensitivity_comparisons <- unique(sensitivity_for_manuscript$comparison_id)

if (!setequal(actual_sensitivity_comparisons, expected_sensitivity_comparisons)) {
  stop(
    "Unexpected sensitivity comparison IDs detected. Found: ",
    paste(actual_sensitivity_comparisons, collapse = ", ")
  )
}

cat("✅ Expected sensitivity comparisons validated.\n")


# --------------------------------------
# 7. Label, ordering, and formatting helpers
# --------------------------------------

validation_level_order <- c(
  "global_baseline",
  "framework_domain",
  "framework_construct_spot_check"
)

sensitivity_comparison_order <- c(
  "metric_sensitivity_nodepurity_vs_permutation",
  "ntree_sensitivity_500_vs_1000",
  "mtry_sensitivity_default_vs_sqrt"
)

clean_validation_level <- function(x) {
  case_when(
    x == "global_baseline" ~ "Global baseline",
    x == "framework_domain" ~ "Framework domain",
    x == "framework_construct_spot_check" ~ "Construct spot checks",
    TRUE ~ x
  )
}

clean_sensitivity_comparison <- function(x) {
  case_when(
    x == "metric_sensitivity_nodepurity_vs_permutation" ~
      "IncNodePurity vs permutation importance",
    x == "ntree_sensitivity_500_vs_1000" ~
      "500 vs 1000 trees",
    x == "mtry_sensitivity_default_vs_sqrt" ~
      "Default mtry vs sqrt(p)",
    TRUE ~ x
  )
}

clean_model_family <- function(x) {
  case_when(
    x == "global_all_predictors" ~ "Global: all predictors",
    x == "global_in_school" ~ "Global: in-school predictors",
    x == "global_out_of_school" ~ "Global: out-of-school predictors",
    x == "pisa_inschool_process_domain" ~ "PISA: in-school Process domain",
    x == "pisa_outschool_process_domain" ~ "PISA: out-of-school Process domain",
    x == "mlftau_inschool_individual_domain" ~ "MLFTAU: in-school Individual domain",
    x == "mlftau_inschool_organisation_school_domain" ~ "MLFTAU: in-school Organisation/School domain",
    x == "mlftau_inschool_individual_user_attributes_construct" ~ "MLFTAU: Individual/User Attributes construct",
    x == "pisa_outschool_process_self_directed_ict_learning_construct" ~ "PISA ICT: self-directed learning construct",
    TRUE ~ str_replace_all(x, "_", " ")
  )
}

round3 <- function(x) {
  round(x, 3)
}

format_range <- function(min_value, max_value, digits = 3) {
  paste0(round(min_value, digits), "–", round(max_value, digits))
}


# --------------------------------------
# 8. Summarise sensitivity by validation level
# --------------------------------------

sensitivity_summary_compact <- sensitivity_for_manuscript %>%
  group_by(validation_level) %>%
  summarise(
    n_sensitivity_comparisons = n_distinct(comparison_id),
    sensitivity_spearman_min = min(mean_spearman_rank_correlation, na.rm = TRUE),
    sensitivity_spearman_max = max(mean_spearman_rank_correlation, na.rm = TRUE),
    sensitivity_top10_overlap_min = min(mean_top10_overlap_prop, na.rm = TRUE),
    sensitivity_top10_overlap_max = max(mean_top10_overlap_prop, na.rm = TRUE),
    sensitivity_top20_overlap_min = min(mean_top20_overlap_prop, na.rm = TRUE),
    sensitivity_top20_overlap_max = max(mean_top20_overlap_prop, na.rm = TRUE),
    sensitivity_comparisons_included = paste(
      clean_sensitivity_comparison(
        sensitivity_comparison_order[
          sensitivity_comparison_order %in% unique(comparison_id)
        ]
      ),
      collapse = "; "
    ),
    .groups = "drop"
  ) %>%
  mutate(
    sensitivity_spearman_range = format_range(
      sensitivity_spearman_min,
      sensitivity_spearman_max
    ),
    sensitivity_top10_overlap_range = format_range(
      sensitivity_top10_overlap_min,
      sensitivity_top10_overlap_max
    ),
    sensitivity_top20_overlap_range = format_range(
      sensitivity_top20_overlap_min,
      sensitivity_top20_overlap_max
    )
  )


# --------------------------------------
# 8b. Build wide sensitivity table for audit use
# --------------------------------------

sensitivity_wide <- sensitivity_for_manuscript %>%
  select(
    validation_level,
    comparison_id,
    mean_spearman_rank_correlation,
    mean_top10_overlap_prop,
    mean_top20_overlap_prop,
    mean_top10_jaccard,
    mean_top20_jaccard
  ) %>%
  mutate(
    comparison_short = case_when(
      comparison_id == "metric_sensitivity_nodepurity_vs_permutation" ~ "metric",
      comparison_id == "ntree_sensitivity_500_vs_1000" ~ "ntree",
      comparison_id == "mtry_sensitivity_default_vs_sqrt" ~ "mtry",
      TRUE ~ comparison_id
    )
  ) %>%
  select(-comparison_id) %>%
  pivot_wider(
    names_from = comparison_short,
    values_from = c(
      mean_spearman_rank_correlation,
      mean_top10_overlap_prop,
      mean_top20_overlap_prop,
      mean_top10_jaccard,
      mean_top20_jaccard
    ),
    names_glue = "{comparison_short}_{.value}"
  )


# --------------------------------------
# 9. Build validation-level master audit table
# --------------------------------------

performance_validation_clean <- performance_by_validation_level %>%
  transmute(
    validation_level,
    n_model_families_performance = n_model_families,
    mean_oob_r2 = mean_oob_r2,
    mean_oob_rmse = mean_oob_rmse,
    sd_oob_r2 = if ("sd_oob_r2" %in% names(.)) sd_oob_r2 else NA_real_,
    sd_oob_rmse = if ("sd_oob_rmse" %in% names(.)) sd_oob_rmse else NA_real_
  )

rank_validation_clean <- rank_by_validation_level %>%
  transmute(
    validation_level,
    mean_rank_stability_spearman = mean_spearman_rank_correlation,
    sd_rank_stability_spearman = if ("sd_spearman_rank_correlation" %in% names(.)) {
      sd_spearman_rank_correlation
    } else {
      NA_real_
    },
    mean_rank_top10_overlap = mean_top10_overlap_prop,
    mean_rank_top20_overlap = mean_top20_overlap_prop
  )

correlation_validation_clean <- correlation_for_manuscript %>%
  transmute(
    validation_level,
    total_pairwise_correlations = total_pairwise_correlations,
    total_high_correlation_pairs = total_high_correlation_pairs,
    total_very_high_correlation_pairs = total_very_high_correlation_pairs,
    total_high_positive_correlation_pairs = total_high_positive_correlation_pairs,
    total_high_negative_correlation_pairs = total_high_negative_correlation_pairs,
    total_very_high_positive_correlation_pairs = total_very_high_positive_correlation_pairs,
    total_very_high_negative_correlation_pairs = total_very_high_negative_correlation_pairs,
    total_high_pairs_involving_top20_predictor = total_high_pairs_involving_top20_predictor,
    total_high_pairs_between_top20_predictors = total_high_pairs_between_top20_predictors,
    total_high_pairs_involving_cross_domain_top20_predictor = if (
      "total_high_pairs_involving_cross_domain_top20_predictor" %in% names(.)
    ) {
      total_high_pairs_involving_cross_domain_top20_predictor
    } else {
      NA_real_
    },
    total_high_pairs_between_cross_domain_top20_predictors = if (
      "total_high_pairs_between_cross_domain_top20_predictors" %in% names(.)
    ) {
      total_high_pairs_between_cross_domain_top20_predictors
    } else {
      NA_real_
    },
    total_high_pairs_involving_all_domain_top20_predictor = if (
      "total_high_pairs_involving_all_domain_top20_predictor" %in% names(.)
    ) {
      total_high_pairs_involving_all_domain_top20_predictor
    } else {
      NA_real_
    },
    total_high_pairs_between_all_domain_top20_predictors = if (
      "total_high_pairs_between_all_domain_top20_predictors" %in% names(.)
    ) {
      total_high_pairs_between_all_domain_top20_predictors
    } else {
      NA_real_
    },
    mean_proportion_high_correlation_pairs = mean_proportion_high_correlation_pairs,
    mean_proportion_very_high_correlation_pairs = mean_proportion_very_high_correlation_pairs,
    mean_proportion_predictors_usable_for_spearman = mean_proportion_predictors_usable_for_spearman,
    mean_largest_high_correlation_cluster_size = mean_largest_high_correlation_cluster_size,
    max_largest_high_correlation_cluster_size = max_largest_high_correlation_cluster_size,
    correlation_interpretive_note = interpretive_note
  )

validation_master_audit_by_validation_level <- performance_validation_clean %>%
  full_join(rank_validation_clean, by = "validation_level") %>%
  full_join(sensitivity_summary_compact, by = "validation_level") %>%
  full_join(sensitivity_wide, by = "validation_level") %>%
  full_join(correlation_validation_clean, by = "validation_level") %>%
  mutate(
    validation_level_label = clean_validation_level(validation_level),
    validation_interpretation = case_when(
      validation_level == "global_baseline" ~
        "Strong predictive signal; stable rankings; some correlated top predictors.",
      validation_level == "framework_domain" ~
        "Meaningful predictive signal; stable framework-slice rankings; limited high-correlation pairs.",
      validation_level == "framework_construct_spot_check" ~
        "Moderate predictive signal in narrow slices; stable rankings; no high-correlation pairs detected.",
      TRUE ~
        "Validation evidence supports cautious exploratory interpretation."
    ),
    publication_note = case_when(
      validation_level == "global_baseline" ~
        "Use to justify the broad RF screening procedure; avoid exact top-k overinterpretation.",
      validation_level == "framework_domain" ~
        "Use to support framework-level interpretation of stable predictive patterns.",
      validation_level == "framework_construct_spot_check" ~
        "Use as targeted evidence that narrow theoretical slices retain interpretable signal.",
      TRUE ~
        "Use as exploratory validation evidence."
    )
  ) %>%
  select(
    validation_level,
    validation_level_label,
    everything()
  ) %>%
  arrange(
    factor(validation_level, levels = validation_level_order)
  )


# --------------------------------------
# 10. Build compact main-text validation table
# --------------------------------------

validation_summary_for_main_text <- validation_master_audit_by_validation_level %>%
  transmute(
    validation_level_raw = validation_level,
    validation_level = validation_level_label,
    model_families = n_model_families_performance,
    mean_oob_r2 = round3(mean_oob_r2),
    mean_oob_rmse = round3(mean_oob_rmse),
    rank_stability_spearman = round3(mean_rank_stability_spearman),
    sensitivity_spearman_range = sensitivity_spearman_range,
    high_correlation_pairs = total_high_correlation_pairs,
    high_pairs_involving_top20_predictor = total_high_pairs_involving_top20_predictor,
    interpretation = validation_interpretation
  ) %>%
  arrange(factor(validation_level_raw, levels = validation_level_order)) %>%
  select(-validation_level_raw)

expected_main_text_cols <- c(
  "validation_level",
  "model_families",
  "mean_oob_r2",
  "mean_oob_rmse",
  "rank_stability_spearman",
  "sensitivity_spearman_range",
  "high_correlation_pairs",
  "high_pairs_involving_top20_predictor",
  "interpretation"
)

if (!identical(names(validation_summary_for_main_text), expected_main_text_cols)) {
  stop("Main-text validation summary columns do not match the expected structure.")
}


# --------------------------------------
# 11. Build appendix table by model family
# --------------------------------------

performance_model_clean <- performance_by_model_family %>%
  transmute(
    model_family,
    validation_level,
    mean_n_predictors = mean_n_predictors,
    mean_oob_r2 = mean_oob_r2,
    mean_oob_rmse = mean_oob_rmse,
    sd_oob_r2 = if ("sd_oob_r2" %in% names(.)) sd_oob_r2 else NA_real_,
    sd_oob_rmse = if ("sd_oob_rmse" %in% names(.)) sd_oob_rmse else NA_real_
  )

rank_model_clean <- rank_by_model_family %>%
  transmute(
    model_family,
    validation_level,
    mean_rank_stability_spearman = mean_spearman_rank_correlation,
    sd_rank_stability_spearman = if ("sd_spearman_rank_correlation" %in% names(.)) {
      sd_spearman_rank_correlation
    } else {
      NA_real_
    },
    mean_rank_top10_overlap = mean_top10_overlap_prop,
    mean_rank_top20_overlap = mean_top20_overlap_prop,
    rank_top10_note = if ("top10_overlap_note" %in% names(.)) top10_overlap_note else NA_character_,
    rank_top20_note = if ("top20_overlap_note" %in% names(.)) top20_overlap_note else NA_character_
  )

sensitivity_model_compact <- sensitivity_by_model_family %>%
  group_by(model_family, validation_level) %>%
  summarise(
    n_sensitivity_comparisons = n_distinct(comparison_id),
    sensitivity_spearman_min = min(mean_spearman_rank_correlation, na.rm = TRUE),
    sensitivity_spearman_max = max(mean_spearman_rank_correlation, na.rm = TRUE),
    sensitivity_top10_overlap_min = min(mean_top10_overlap_prop, na.rm = TRUE),
    sensitivity_top10_overlap_max = max(mean_top10_overlap_prop, na.rm = TRUE),
    sensitivity_top20_overlap_min = min(mean_top20_overlap_prop, na.rm = TRUE),
    sensitivity_top20_overlap_max = max(mean_top20_overlap_prop, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    sensitivity_spearman_range = format_range(
      sensitivity_spearman_min,
      sensitivity_spearman_max
    ),
    sensitivity_top10_overlap_range = format_range(
      sensitivity_top10_overlap_min,
      sensitivity_top10_overlap_max
    ),
    sensitivity_top20_overlap_range = format_range(
      sensitivity_top20_overlap_min,
      sensitivity_top20_overlap_max
    )
  )

sensitivity_model_wide <- sensitivity_by_model_family %>%
  select(
    model_family,
    validation_level,
    comparison_id,
    mean_spearman_rank_correlation,
    mean_top10_overlap_prop,
    mean_top20_overlap_prop
  ) %>%
  mutate(
    comparison_short = case_when(
      comparison_id == "metric_sensitivity_nodepurity_vs_permutation" ~ "metric",
      comparison_id == "ntree_sensitivity_500_vs_1000" ~ "ntree",
      comparison_id == "mtry_sensitivity_default_vs_sqrt" ~ "mtry",
      TRUE ~ comparison_id
    )
  ) %>%
  select(-comparison_id) %>%
  pivot_wider(
    names_from = comparison_short,
    values_from = c(
      mean_spearman_rank_correlation,
      mean_top10_overlap_prop,
      mean_top20_overlap_prop
    ),
    names_glue = "{comparison_short}_{.value}"
  )

correlation_model_clean <- correlation_by_model_family %>%
  transmute(
    model_family,
    validation_level,
    n_pairwise_correlations = n_pairwise_correlations,
    n_high_correlation_pairs = n_high_correlation_pairs,
    n_very_high_correlation_pairs = n_very_high_correlation_pairs,
    n_high_pairs_involving_top20_predictor = n_high_pairs_involving_top20_predictor,
    n_high_pairs_between_top20_predictors = n_high_pairs_between_top20_predictors,
    proportion_high_correlation_pairs = proportion_high_correlation_pairs,
    proportion_very_high_correlation_pairs = if (
      "proportion_very_high_correlation_pairs" %in% names(.)
    ) {
      proportion_very_high_correlation_pairs
    } else {
      NA_real_
    },
    proportion_predictors_usable_for_spearman = if (
      "proportion_predictors_usable_for_spearman" %in% names(.)
    ) {
      proportion_predictors_usable_for_spearman
    } else {
      NA_real_
    },
    correlation_interpretive_note = interpretive_note
  )

validation_master_audit_by_model_family <- performance_model_clean %>%
  full_join(rank_model_clean, by = c("model_family", "validation_level")) %>%
  full_join(sensitivity_model_compact, by = c("model_family", "validation_level")) %>%
  full_join(sensitivity_model_wide, by = c("model_family", "validation_level")) %>%
  full_join(correlation_model_clean, by = c("model_family", "validation_level")) %>%
  mutate(
    validation_level_label = clean_validation_level(validation_level),
    model_family_label = clean_model_family(model_family),
    publication_note = case_when(
      validation_level == "global_baseline" ~
        "Use to justify the broad RF screening procedure; avoid exact top-k overinterpretation.",
      validation_level == "framework_domain" ~
        "Use to support framework-level interpretation of stable predictive patterns.",
      validation_level == "framework_construct_spot_check" ~
        "Use as targeted evidence that narrow theoretical slices retain interpretable signal.",
      TRUE ~
        "Use as exploratory validation evidence."
    )
  ) %>%
  select(
    validation_level,
    validation_level_label,
    model_family,
    model_family_label,
    everything()
  ) %>%
  arrange(
    factor(validation_level, levels = validation_level_order),
    model_family
  )

validation_summary_for_appendix_by_model_family <- validation_master_audit_by_model_family %>%
  transmute(
    validation_level = validation_level_label,
    model_family = model_family_label,
    mean_n_predictors = round3(mean_n_predictors),
    mean_oob_r2 = round3(mean_oob_r2),
    mean_oob_rmse = round3(mean_oob_rmse),
    rank_stability_spearman = round3(mean_rank_stability_spearman),
    rank_top10_overlap = round3(mean_rank_top10_overlap),
    rank_top20_overlap = round3(mean_rank_top20_overlap),
    sensitivity_spearman_range = sensitivity_spearman_range,
    metric_sensitivity_spearman = round3(metric_mean_spearman_rank_correlation),
    ntree_sensitivity_spearman = round3(ntree_mean_spearman_rank_correlation),
    mtry_sensitivity_spearman = round3(mtry_mean_spearman_rank_correlation),
    high_correlation_pairs = n_high_correlation_pairs,
    high_pairs_involving_top20_predictor = n_high_pairs_involving_top20_predictor,
    proportion_high_correlation_pairs = round3(proportion_high_correlation_pairs),
    correlation_interpretive_note = correlation_interpretive_note
  )


# --------------------------------------
# 12. Appendix sensitivity table
# --------------------------------------

validation_sensitivity_for_appendix <- sensitivity_for_manuscript %>%
  mutate(
    validation_level_label = clean_validation_level(validation_level),
    comparison_label = clean_sensitivity_comparison(comparison_id)
  ) %>%
  transmute(
    validation_level = validation_level_label,
    comparison = comparison_label,
    n_model_families = n_model_families,
    mean_n_predictors_compared = round3(mean_n_predictors_compared),
    mean_spearman_rank_correlation = round3(mean_spearman_rank_correlation),
    sd_spearman_rank_correlation = round3(sd_spearman_rank_correlation),
    mean_top10_overlap = round3(mean_top10_overlap_prop),
    mean_top20_overlap = round3(mean_top20_overlap_prop),
    mean_top10_jaccard = round3(mean_top10_jaccard),
    mean_top20_jaccard = round3(mean_top20_jaccard),
    interpretive_note = interpretive_note,
    validation_level_raw = validation_level,
    comparison_id_raw = comparison_id
  ) %>%
  arrange(
    factor(validation_level_raw, levels = validation_level_order),
    factor(comparison_id_raw, levels = sensitivity_comparison_order)
  ) %>%
  select(-validation_level_raw, -comparison_id_raw)


# --------------------------------------
# 13. Appendix correlation table
# --------------------------------------

validation_correlation_for_appendix <- correlation_for_manuscript %>%
  mutate(
    validation_level_label = clean_validation_level(validation_level)
  ) %>%
  transmute(
    validation_level = validation_level_label,
    n_model_families = n_model_families,
    mean_n_predictors_selected = round3(mean_n_predictors_selected),
    mean_n_predictors_usable_for_spearman = round3(mean_n_predictors_usable_for_spearman),
    mean_proportion_predictors_usable_for_spearman = round3(mean_proportion_predictors_usable_for_spearman),
    total_pairwise_correlations = total_pairwise_correlations,
    total_high_correlation_pairs = total_high_correlation_pairs,
    total_very_high_correlation_pairs = total_very_high_correlation_pairs,
    total_high_positive_correlation_pairs = total_high_positive_correlation_pairs,
    total_high_negative_correlation_pairs = total_high_negative_correlation_pairs,
    total_high_pairs_involving_top20_predictor = total_high_pairs_involving_top20_predictor,
    total_high_pairs_between_top20_predictors = total_high_pairs_between_top20_predictors,
    mean_proportion_high_correlation_pairs = round3(mean_proportion_high_correlation_pairs),
    mean_proportion_very_high_correlation_pairs = round3(mean_proportion_very_high_correlation_pairs),
    mean_largest_high_correlation_cluster_size = round3(mean_largest_high_correlation_cluster_size),
    max_largest_high_correlation_cluster_size = max_largest_high_correlation_cluster_size,
    interpretive_note = interpretive_note,
    validation_level_raw = validation_level
  ) %>%
  arrange(factor(validation_level_raw, levels = validation_level_order)) %>%
  select(-validation_level_raw)


# --------------------------------------
# 14. Key statistics for prose
# --------------------------------------

extract_value <- function(data, validation_level_value, column_name) {
  data %>%
    filter(validation_level == validation_level_value) %>%
    pull(all_of(column_name)) %>%
    first()
}

key_statistics_for_text <- tibble(
  statistic = c(
    "global_mean_oob_r2",
    "framework_domain_mean_oob_r2",
    "construct_spot_check_mean_oob_r2",
    "global_mean_oob_rmse",
    "framework_domain_mean_oob_rmse",
    "construct_spot_check_mean_oob_rmse",
    "global_rank_stability_spearman",
    "framework_domain_rank_stability_spearman",
    "construct_spot_check_rank_stability_spearman",
    "global_sensitivity_spearman_range",
    "framework_domain_sensitivity_spearman_range",
    "construct_spot_check_sensitivity_spearman_range",
    "global_high_correlation_pairs",
    "framework_domain_high_correlation_pairs",
    "construct_spot_check_high_correlation_pairs",
    "global_high_pairs_involving_top20_predictor",
    "framework_domain_high_pairs_involving_top20_predictor",
    "construct_spot_check_high_pairs_involving_top20_predictor"
  ),
  value = c(
    round3(extract_value(validation_master_audit_by_validation_level, "global_baseline", "mean_oob_r2")),
    round3(extract_value(validation_master_audit_by_validation_level, "framework_domain", "mean_oob_r2")),
    round3(extract_value(validation_master_audit_by_validation_level, "framework_construct_spot_check", "mean_oob_r2")),
    round3(extract_value(validation_master_audit_by_validation_level, "global_baseline", "mean_oob_rmse")),
    round3(extract_value(validation_master_audit_by_validation_level, "framework_domain", "mean_oob_rmse")),
    round3(extract_value(validation_master_audit_by_validation_level, "framework_construct_spot_check", "mean_oob_rmse")),
    round3(extract_value(validation_master_audit_by_validation_level, "global_baseline", "mean_rank_stability_spearman")),
    round3(extract_value(validation_master_audit_by_validation_level, "framework_domain", "mean_rank_stability_spearman")),
    round3(extract_value(validation_master_audit_by_validation_level, "framework_construct_spot_check", "mean_rank_stability_spearman")),
    extract_value(validation_master_audit_by_validation_level, "global_baseline", "sensitivity_spearman_range"),
    extract_value(validation_master_audit_by_validation_level, "framework_domain", "sensitivity_spearman_range"),
    extract_value(validation_master_audit_by_validation_level, "framework_construct_spot_check", "sensitivity_spearman_range"),
    extract_value(validation_master_audit_by_validation_level, "global_baseline", "total_high_correlation_pairs"),
    extract_value(validation_master_audit_by_validation_level, "framework_domain", "total_high_correlation_pairs"),
    extract_value(validation_master_audit_by_validation_level, "framework_construct_spot_check", "total_high_correlation_pairs"),
    extract_value(validation_master_audit_by_validation_level, "global_baseline", "total_high_pairs_involving_top20_predictor"),
    extract_value(validation_master_audit_by_validation_level, "framework_domain", "total_high_pairs_involving_top20_predictor"),
    extract_value(validation_master_audit_by_validation_level, "framework_construct_spot_check", "total_high_pairs_involving_top20_predictor")
  )
) %>%
  mutate(value = as.character(value))


# --------------------------------------
# 15. Save outputs
# --------------------------------------

main_text_file <- file.path(
  output_dir,
  "rf_validation_summary_for_main_text.csv"
)

appendix_model_family_file <- file.path(
  output_dir,
  "rf_validation_summary_for_appendix_by_model_family.csv"
)

appendix_sensitivity_file <- file.path(
  output_dir,
  "rf_validation_sensitivity_for_appendix.csv"
)

appendix_correlation_file <- file.path(
  output_dir,
  "rf_validation_correlation_for_appendix.csv"
)

master_validation_level_file <- file.path(
  output_dir,
  "rf_validation_master_audit_by_validation_level.csv"
)

master_model_family_file <- file.path(
  output_dir,
  "rf_validation_master_audit_by_model_family.csv"
)

key_statistics_file <- file.path(
  output_dir,
  "rf_validation_key_statistics_for_text.csv"
)

write_csv(validation_summary_for_main_text, main_text_file)
write_csv(validation_summary_for_appendix_by_model_family, appendix_model_family_file)
write_csv(validation_sensitivity_for_appendix, appendix_sensitivity_file)
write_csv(validation_correlation_for_appendix, appendix_correlation_file)
write_csv(validation_master_audit_by_validation_level, master_validation_level_file)
write_csv(validation_master_audit_by_model_family, master_model_family_file)
write_csv(key_statistics_for_text, key_statistics_file)

cat("\n✅ Main-text validation summary saved to:\n", main_text_file, "\n")
cat("✅ Appendix model-family validation summary saved to:\n", appendix_model_family_file, "\n")
cat("✅ Appendix sensitivity validation summary saved to:\n", appendix_sensitivity_file, "\n")
cat("✅ Appendix correlation diagnostics summary saved to:\n", appendix_correlation_file, "\n")
cat("✅ Master validation-level audit saved to:\n", master_validation_level_file, "\n")
cat("✅ Master model-family audit saved to:\n", master_model_family_file, "\n")
cat("✅ Key statistics for prose saved to:\n", key_statistics_file, "\n")


# --------------------------------------
# 16. Row-count check
# --------------------------------------

cat("\nRow-count check:\n")
cat("Main-text summary rows:", nrow(validation_summary_for_main_text), "\n")
cat("Appendix model-family rows:", nrow(validation_summary_for_appendix_by_model_family), "\n")
cat("Appendix sensitivity rows:", nrow(validation_sensitivity_for_appendix), "\n")
cat("Appendix correlation rows:", nrow(validation_correlation_for_appendix), "\n")
cat("Master validation-level audit rows:", nrow(validation_master_audit_by_validation_level), "\n")
cat("Master model-family audit rows:", nrow(validation_master_audit_by_model_family), "\n")
cat("Key statistics rows:", nrow(key_statistics_for_text), "\n")


# --------------------------------------
# 17. Final output validation
# --------------------------------------

final_expected_counts <- c(
  main_text_summary = 3,
  appendix_model_family = 9,
  appendix_sensitivity = 9,
  appendix_correlation = 3,
  master_validation_level = 3,
  master_model_family = 9,
  key_statistics = 18
)

final_actual_counts <- c(
  main_text_summary = nrow(validation_summary_for_main_text),
  appendix_model_family = nrow(validation_summary_for_appendix_by_model_family),
  appendix_sensitivity = nrow(validation_sensitivity_for_appendix),
  appendix_correlation = nrow(validation_correlation_for_appendix),
  master_validation_level = nrow(validation_master_audit_by_validation_level),
  master_model_family = nrow(validation_master_audit_by_model_family),
  key_statistics = nrow(key_statistics_for_text)
)

final_count_problems <- names(final_expected_counts)[
  final_actual_counts != final_expected_counts
]

if (length(final_count_problems) > 0) {
  stop(
    "Unexpected final output row counts detected:\n",
    paste(
      final_count_problems,
      "expected",
      final_expected_counts[final_count_problems],
      "but found",
      final_actual_counts[final_count_problems],
      collapse = "\n"
    )
  )
}

cat("\n✅ Final output row counts validated.\n")


# --------------------------------------
# 18. Console previews
# --------------------------------------

cat("\n========================================\n")
cat("Preview: Main-text validation summary\n")
cat("========================================\n")
print(validation_summary_for_main_text)

cat("\n========================================\n")
cat("Preview: Appendix model-family validation summary\n")
cat("========================================\n")
print(validation_summary_for_appendix_by_model_family)

cat("\n========================================\n")
cat("Preview: Appendix sensitivity validation summary\n")
cat("========================================\n")
print(validation_sensitivity_for_appendix)

cat("\n========================================\n")
cat("Preview: Appendix correlation diagnostics summary\n")
cat("========================================\n")
print(validation_correlation_for_appendix)

cat("\n🎉 RF validation summary-table generation complete.\n")
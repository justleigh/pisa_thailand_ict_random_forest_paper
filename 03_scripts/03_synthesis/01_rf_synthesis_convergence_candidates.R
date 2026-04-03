# --------------------------------------
# Random Forest Synthesis Script
# --------------------------------------
# File: 03_scripts/03_synthesis/rf_synthesis_script.R
# Purpose:
# This script runs the implemented synthesis workflow used to consolidate the
# completed random forest outputs for the published Thailand PISA 2022 paper.
# It identifies convergent predictors, framework-sensitive signals,
# context-specific shifts, and candidate carry-forward predictors, and produces
# the synthesis outputs directly underpinning the article’s main interpretive claims.
#
# Main tasks:
# 1. Load and standardise random forest output files across model families.
# 2. Join the project-specific variable mapping table.
# 3. Build convergence, framework-sensitive, and context-shift summaries.
# 4. Score candidate predictors for carry-forward interpretation.
# 5. Build the one-page synthesis table used in the paper.
# 6. Write the synthesis outputs used to support the published results.
#
# Public repository note:
# This script is shared to support transparency regarding the published analysis.
# It reflects the implemented workflow used in the study, but some intermediate
# inputs, metadata resources, and output paths from the broader private project
# are not publicly released.

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(readr)
  library(purrr)
  library(tidyr)
  library(tibble)
  library(glue)
})

# ---------------------------
# 0) USER PATHS
# ---------------------------
# Note: update these paths to match your own local environment and available inputs.
rf_root <- "PATH_TO_LOCAL_RF_OUTPUTS"
mapping_path <- "PATH_TO_LOCAL_METADATA/pisa2022_variable_mapping_table_final.csv"

out_dir <- "PATH_TO_LOCAL_OUTPUT/rf_synthesis_outputs"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Public repository note:
# This script depends on a project-specific variable mapping file used in the
# implemented workflow. The full private mapping table is not publicly released.

# ---------------------------
# 1) HELPERS: PATH PARSING
# ---------------------------
normalize_path <- function(p) str_replace_all(p, "\\\\", "/")

infer_framework_from_path <- function(path) {
  p <- normalize_path(path)
  
  if (str_detect(p, "/random_forests/global/all_predictors/")) return("Global_AllPredictors")
  if (str_detect(p, "/random_forests/global/in_class/"))      return("Global_InSchool")
  if (str_detect(p, "/random_forests/global/outside_class/")) return("Global_OutOfSchool")
  
  if (str_detect(p, "/pisa_contextual_ict_in_class/"))  return("PISA_Contextual_InSchool")
  if (str_detect(p, "/pisa_contextual_ict_outside/"))   return("PISA_Contextual_OutOfSchool")
  if (str_detect(p, "/mlftau_contextual_ict_in_class/"))  return("MLFTAU_Contextual_InSchool")
  if (str_detect(p, "/mlftau_contextual_ict_outside/")) return("MLFTAU_Contextual_OutOfSchool")
  
  return("Other")
}

infer_granularity_from_path <- function(path) {
  p <- normalize_path(path)
  if (str_detect(p, "/constructs/")) return("Construct")
  if (str_detect(p, "/domains/"))    return("Domain")
  return("None")
}

`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a
}

safe_col <- function(df, col) {
  if (col %in% names(df)) {
    df[[col]]
  } else {
    rep(NA_character_, nrow(df))
  }
}

infer_domain_construct_from_path <- function(path) {
  # Tries to recover folder semantics:
  # .../domains/<domain>/
  # .../constructs/<domain>/<construct>/
  p <- normalize_path(path)
  parts <- str_split(p, "/")[[1]]
  idx <- which(parts %in% c("constructs", "domains"))
  
  if (length(idx) == 0) {
    return(list(domain = NA_character_, construct = NA_character_))
  }
  
  gran <- parts[idx[1]]
  domain <- parts[idx[1] + 1] %||% NA_character_
  construct <- if (gran == "constructs") parts[idx[1] + 2] %||% NA_character_ else NA_character_
  
  list(domain = domain, construct = construct)
}

# ---------------------------
# 2) FIND AND LOAD RF OUTPUT FILES
# ---------------------------

# (A) Combined normalized outputs (PISA / MLFTAU)
combined_patterns <- c(
  "^rf_combined_normalized_long_.*\\.csv$",
  ".*_rf_combined_normalized_importance_.*\\.csv$"
)

combined_files <- map(combined_patterns, ~ list.files(
  path = rf_root,
  pattern = .x,
  recursive = TRUE,
  full.names = TRUE
)) %>% unlist() %>% unique()

# (B) Global baseline outputs (raw, per-outcome)
global_files <- list.files(
  path = file.path(rf_root, "global"),
  pattern = ".*_rf_variable_importance\\.csv$",
  recursive = TRUE,
  full.names = TRUE
) %>% unique()

rf_files <- unique(c(combined_files, global_files))

if (length(rf_files) == 0) {
  stop(glue("No RF files found under: {rf_root}"))
}

# Diagnostic sanity check
cat("RF files loaded:", length(rf_files), "\n")
cat("Global baseline files:", sum(str_detect(normalize_path(rf_files), "/global/")), "\n")
cat("Combined normalized files:", sum(str_detect(basename(rf_files), "combined_normalized")), "\n")

# ---------------------------
# 3) STANDARDISE EACH FILE INTO A LONG EVIDENCE FORMAT
# ---------------------------
standardise_rf_long <- function(df, file) {
  nm <- names(df)
  
  # variable column
  var_col <- intersect(nm, c("variable", "Predictor", "predictor", "Predictor Name", "renamed_variable"))[1]
  if (is.na(var_col)) stop(glue("No variable column detected in: {file}"))
  
  # outcome column 
  out_col <- intersect(nm, c("outcome", "Outcome", "subject", "DV"))[1] %||% NA_character_
  
  # importance column
  imp_col <- intersect(
    nm,
    c("norm_importance", "mean_norm_importance", "mean_importance", "IncNodePurity", "importance", "Importance")
  )[1]
  if (is.na(imp_col)) {
    stop(glue(
      "No importance column detected in: {file}. Expected one of: ",
      "norm_importance, mean_norm_importance, mean_importance, IncNodePurity, importance."
    ))
  }
  
  infer_outcome_from_filename <- function(file) {
    f <- tolower(basename(file))
    if (str_detect(f, "(^|_)math(_|\\.|$)")) return("MATH")
    if (str_detect(f, "(^|_)read(_|\\.|$)")) return("READ")
    if (str_detect(f, "(^|_)scie(_|\\.|$)")) return("SCIE")
    return(NA_character_)
  }
  
  # already normalized?
  p_norm <- normalize_path(file)
  
  is_global_file <- str_detect(p_norm, "/random_forests/global/")
  file_name_says_normalized <- str_detect(tolower(basename(p_norm)), "combined_normalized")
  
  is_already_normalized <- if (is_global_file) {
    FALSE  # Global variable_importance_tables are raw (including mean_importance)
  } else {
    (imp_col %in% c("norm_importance", "mean_norm_importance")) || file_name_says_normalized
  }
  
  # ---- SAFE outcome extraction (NO case_when) ----
  outcome_vec <- if (!is.na(out_col) && out_col %in% nm) {
    as.character(df[[out_col]])
  } else {
    rep(infer_outcome_from_filename(file), nrow(df))
  }
  
  if (all(is.na(outcome_vec))) {
    stop(glue("Outcome could not be inferred from filename and no outcome column exists: {file}"))
  }
  
  df2 <- df %>%
    rename(variable = all_of(var_col)) %>%
    mutate(
      outcome = outcome_vec,
      imp_raw = suppressWarnings(as.numeric(.data[[imp_col]]))
    )
  
  max_imp <- suppressWarnings(max(df2$imp_raw, na.rm = TRUE))
  safe_denom <- if (!is.finite(max_imp) || max_imp <= 0) NA_real_ else max_imp
  
  df2 %>%
    mutate(
      norm_importance = if (is_already_normalized) imp_raw else imp_raw / safe_denom
    ) %>%
    select(variable, outcome, norm_importance)
}

rf_long <- map_dfr(rf_files, function(f) {
  info_dc <- infer_domain_construct_from_path(f)
  
  df <- suppressMessages(read_csv(f, show_col_types = FALSE))
  df_std <- standardise_rf_long(df, f)
  
  df_std %>%
    mutate(
      source_file = f,
      framework = infer_framework_from_path(f),
      granularity = infer_granularity_from_path(f),
      domain_path = info_dc$domain,
      construct_path = info_dc$construct
    )
})

n_outcome_na <- sum(is.na(rf_long$outcome) | rf_long$outcome == "")
pct_outcome_na <- round(100 * n_outcome_na / nrow(rf_long), 2)
cat(glue("Outcome parsing check: {n_outcome_na} rows with missing outcome ({pct_outcome_na}%).\n"))

stopifnot(
  any(rf_long$framework == "Global_AllPredictors"),
  any(rf_long$framework == "Global_InSchool"),
  any(rf_long$framework == "Global_OutOfSchool")
)

rf_long <- rf_long %>%
  mutate(
    context = case_when(
      framework == "Global_AllPredictors" ~ "All",
      framework %in% c("Global_InSchool", "PISA_Contextual_InSchool", "MLFTAU_Contextual_InSchool") ~ "InSchool",
      framework %in% c("Global_OutOfSchool", "PISA_Contextual_OutOfSchool", "MLFTAU_Contextual_OutOfSchool") ~ "OutOfSchool",
      TRUE ~ NA_character_
    )
  )

audit <- rf_long %>%
  group_by(source_file, outcome) %>%
  summarise(
    n_nonmissing = sum(is.finite(norm_importance)),
    min_val = suppressWarnings(min(norm_importance, na.rm = TRUE)),
    max_val = suppressWarnings(max(norm_importance, na.rm = TRUE)),
    .groups = "drop"
  )

bad <- audit %>%
  filter(
    n_nonmissing == 0 |
      !is.finite(min_val) | !is.finite(max_val) |
      min_val < -1e-6 | max_val > 1 + 1e-6 |
      max_val < 0.95
  )

if (nrow(bad) > 0) {
  print(bad)
  stop("Normalization audit failed: at least one RF file/outcome is not properly scaled to ~0–1.")
}

# Public repository note:
# The synthesis workflow joins to a project-specific mapping table that is not
# fully released in this repository. The reduced public metadata files do not
# replace the full internal mapping resource used here.
# ---------------------------
# 4) JOIN MAPPING TABLE (RECOMMENDED)
# ---------------------------
mapping_tbl <- suppressMessages(read_csv(mapping_path, show_col_types = FALSE))

# In your RF outputs, 'variable' should already be the RENAMED variable.
if (!"renamed_variable" %in% names(mapping_tbl)) {
  stop("Mapping table missing 'renamed_variable'. This synthesis expects renamed_variable as the primary join key.")
}

rf_long <- rf_long %>%
  left_join(mapping_tbl, by = c("variable" = "renamed_variable"))

# ---- Join integrity check ----
n_total <- nrow(rf_long)

if (n_total == 0) stop("rf_long has 0 rows after loading/standardising. Check rf_files patterns and standardise_rf_long().")

check_col <- "broad_learning_context"
if (!check_col %in% names(rf_long)) {
  stop(glue("Join check failed: expected column '{check_col}' not found after join."))
}

n_unmapped <- sum(is.na(rf_long[[check_col]]) | rf_long[[check_col]] == "")
pct_unmapped <- round(100 * n_unmapped / n_total, 2)

cat(glue(
  "\nMapping join check: {n_total} RF rows; ",
  "{n_unmapped} rows missing '{check_col}' ({pct_unmapped}%).\n"
))

if (pct_unmapped > 1) {
  unmapped_vars <- rf_long %>%
    filter(is.na(.data[[check_col]]) | .data[[check_col]] == "") %>%
    count(variable, sort = TRUE) %>%
    slice_head(n = 20)
  
  print(unmapped_vars)
  
  if (pct_unmapped > 5) {
    stop(glue(
      "Mapping join too incomplete ({pct_unmapped}%). Investigate join key alignment (variable names)."
    ))
  } else {
    warning(glue(
      "Mapping join partially incomplete ({pct_unmapped}%). Review unmapped variables above."
    ))
  }
}

# Preserve model context (already derived from framework in Step 3).
# Store variable-level learning context separately (do NOT overwrite slice context).
rf_long <- rf_long %>%
  mutate(
    variable_learning_context = case_when(
      !is.na(broad_learning_context) & broad_learning_context == "In School"     ~ "InSchool",
      !is.na(broad_learning_context) & broad_learning_context == "Out of School" ~ "OutOfSchool",
      !is.na(broad_learning_context) & broad_learning_context == "In and Out"    ~ "InAndOut",
      TRUE ~ NA_character_
    )
  )

# Define “slice” units as the unit of competition for ranks:
# framework x context x granularity x domain_path x construct_path x outcome
rf_slices <- rf_long %>%
  mutate(
    outcome = if_else(is.na(outcome) | outcome == "", NA_character_, as.character(outcome)),
    slice_id = str_c(
      framework,
      coalesce(context, "NA"),
      coalesce(granularity, "NA"),
      coalesce(domain_path, "NA"),
      coalesce(construct_path, "NA"),
      coalesce(outcome, "NA"),
      sep = " | "
    )
  )

# Aggregate duplicates within slice
rf_evidence <- rf_slices %>%
  group_by(slice_id, framework, context, granularity, domain_path, construct_path, outcome, variable) %>%
  summarise(mean_norm_importance = mean(norm_importance, na.rm = TRUE), .groups = "drop")

# Compute within-slice ranks and percentiles
rf_evidence <- rf_evidence %>%
  group_by(slice_id) %>%
  arrange(desc(mean_norm_importance), .by_group = TRUE) %>%
  mutate(
    rank_in_slice = row_number(),
    n_in_slice = n(),
    
    rank_score_1best = if_else(n_in_slice > 1,
                               1 - (rank_in_slice - 1) / (n_in_slice - 1),
                               1),
    
    in_top20 = rank_in_slice <= 20L,
    
    k_eff = pmin(15L, pmax(1L, ceiling(0.33 * n_in_slice))),
    is_topk = rank_in_slice <= k_eff,
    is_top1 = rank_in_slice == 1L,
    is_top3 = rank_in_slice <= 3L,
    
    eligible_for_convergence = n_in_slice >= 5L
  ) %>%
  ungroup()



# ---------------------------
# 5) DEFINE SIGNAL RULES
# ---------------------------
top_k_fixed <- 10L

rf_signals <- rf_evidence %>%
  mutate(
    is_topk_fixed = rank_in_slice <= top_k_fixed,
    is_top1 = rank_in_slice == 1L
  ) %>%
  mutate(
    framework_group = case_when(
      str_detect(framework, "^PISA")   ~ "PISA",
      str_detect(framework, "^MLFTAU") ~ "MLFTAU",
      str_detect(framework, "^Global") ~ "Global",
      TRUE ~ "Other"
    ),
    is_global_total_baseline    = framework == "Global_AllPredictors",
    is_global_inschool_baseline = framework == "Global_InSchool",
    is_global_outschool_baseline= framework == "Global_OutOfSchool"
  )


# ---------------------------
# 6) GLOBAL BASELINES (EXPLICIT)
# ---------------------------

# Summaries per variable for baseline benchmarking.
# Note: These summarise across outcomes (READ/MATH/SCIE) by taking the best rank / max importance observed
# for each variable within each global baseline space.

global_baseline_flags <- rf_signals %>%
  filter(framework %in% c("Global_AllPredictors", "Global_InSchool", "Global_OutOfSchool")) %>%
  group_by(variable) %>%
  summarise(
    appears_global_all = any(framework == "Global_AllPredictors"),
    appears_global_in  = any(framework == "Global_InSchool"),
    appears_global_out = any(framework == "Global_OutOfSchool"),
    
    best_rank_global_all_any_outcome =
      min(rank_in_slice[framework == "Global_AllPredictors"], na.rm = TRUE),
    best_rank_global_in_any_outcome  =
      min(rank_in_slice[framework == "Global_InSchool"], na.rm = TRUE),
    best_rank_global_out_any_outcome =
      min(rank_in_slice[framework == "Global_OutOfSchool"], na.rm = TRUE),
    
    max_imp_global_all_any_outcome =
      max(mean_norm_importance[framework == "Global_AllPredictors"], na.rm = TRUE),
    max_imp_global_in_any_outcome  =
      max(mean_norm_importance[framework == "Global_InSchool"], na.rm = TRUE),
    max_imp_global_out_any_outcome =
      max(mean_norm_importance[framework == "Global_OutOfSchool"], na.rm = TRUE),
    
    # Which outcome produced the max importance (useful for later reporting/debugging)
    max_imp_global_all_outcome = {
      tmp <- mean_norm_importance[framework == "Global_AllPredictors"]
      tmp_out <- outcome[framework == "Global_AllPredictors"]
      ok <- is.finite(tmp)
      if (length(tmp) == 0 || !any(ok, na.rm = TRUE)) NA_character_
      else tmp_out[which.max(replace(tmp, !ok, -Inf))][1]
    },
    
    max_imp_global_in_outcome = {
      tmp <- mean_norm_importance[framework == "Global_InSchool"]
      tmp_out <- outcome[framework == "Global_InSchool"]
      ok <- is.finite(tmp)
      if (length(tmp) == 0 || !any(ok, na.rm = TRUE)) NA_character_
      else tmp_out[which.max(replace(tmp, !ok, -Inf))][1]
    },
    
    max_imp_global_out_outcome = {
      tmp <- mean_norm_importance[framework == "Global_OutOfSchool"]
      tmp_out <- outcome[framework == "Global_OutOfSchool"]
      ok <- is.finite(tmp)
      if (length(tmp) == 0 || !any(ok, na.rm = TRUE)) NA_character_
      else tmp_out[which.max(replace(tmp, !ok, -Inf))][1]
    },
    
    .groups = "drop"
  ) %>%
  mutate(
    best_rank_global_all_any_outcome = if_else(is.infinite(best_rank_global_all_any_outcome), NA_real_, best_rank_global_all_any_outcome),
    best_rank_global_in_any_outcome  = if_else(is.infinite(best_rank_global_in_any_outcome),  NA_real_, best_rank_global_in_any_outcome),
    best_rank_global_out_any_outcome = if_else(is.infinite(best_rank_global_out_any_outcome), NA_real_, best_rank_global_out_any_outcome),
    
    max_imp_global_all_any_outcome = if_else(is.infinite(max_imp_global_all_any_outcome), NA_real_, max_imp_global_all_any_outcome),
    max_imp_global_in_any_outcome  = if_else(is.infinite(max_imp_global_in_any_outcome),  NA_real_, max_imp_global_in_any_outcome),
    max_imp_global_out_any_outcome = if_else(is.infinite(max_imp_global_out_any_outcome), NA_real_, max_imp_global_out_any_outcome)
  )


# ---------------------------
# 7) GOAL 1: CONVERGENCE MAP (DESCRIPTIVE, NOT A GATEKEEPER)
# ---------------------------
# “Convergence” is reported for interpretive structure; it is not required for carry-forward.

convergence_map <- rf_signals %>%
  filter(is_topk & eligible_for_convergence) %>%
  group_by(variable) %>%
  summarise(
    n_topk_slices = n_distinct(slice_id),
    frameworks    = str_c(sort(unique(framework)), collapse = "; "),
    contexts      = str_c(sort(unique(context)), collapse = "; "),
    granularities = str_c(sort(unique(granularity)), collapse = "; "),
    max_importance = max(mean_norm_importance, na.rm = TRUE),
    best_rank_any  = min(rank_in_slice, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_topk_slices), best_rank_any, desc(max_importance))


# ---------------------------
# 8) GOAL 2: FRAMEWORK- AND CONTEXT-SENSITIVE SIGNALS
# ---------------------------
# Framework-sensitive: top-k in one lens group but not top-k in the other.
# Context-sensitive: top-k in InSchool but not OutOfSchool (or vice versa) within a lens group.
# Note: These are computed in the slice-based top-k signal space (rank_in_slice <= top_k).

# Use framework_group already defined in Step 5
rf_top_tagged <- rf_signals %>%
  mutate(
    context = if_else(is.na(context) | context == "", "NA", context),
    outcome = if_else(is.na(outcome) | outcome == "", "NA", outcome)
  )

# ---- A) Framework-sensitive (PISA vs MLFTAU), with strength metadata ----
framework_sensitive <- rf_top_tagged %>%
  filter(framework_group %in% c("PISA", "MLFTAU")) %>%
  filter(is_topk) %>%
  group_by(variable) %>%
  summarise(
    pisa_topk   = any(framework_group == "PISA"),
    mlftau_topk = any(framework_group == "MLFTAU"),
    
    # Context coverage (drop NAs defensively)
    pisa_contexts = str_c(sort(unique(na.omit(context[framework_group == "PISA"]))), collapse = "; "),
    mlftau_contexts = str_c(sort(unique(na.omit(context[framework_group == "MLFTAU"]))), collapse = "; "),
    
    # Strength signals (best rank / max importance within each lens group)
    pisa_best_rank = suppressWarnings(min(rank_in_slice[framework_group == "PISA"], na.rm = TRUE)),
    mlftau_best_rank = suppressWarnings(min(rank_in_slice[framework_group == "MLFTAU"], na.rm = TRUE)),
    pisa_max_imp = suppressWarnings(max(mean_norm_importance[framework_group == "PISA"], na.rm = TRUE)),
    mlftau_max_imp = suppressWarnings(max(mean_norm_importance[framework_group == "MLFTAU"], na.rm = TRUE)),
    
    # Where the max importance came from (slice breadcrumbs)
    pisa_peak_slice = {
      tmp <- mean_norm_importance[framework_group == "PISA"]
      tmp_slice <- slice_id[framework_group == "PISA"]
      ok <- is.finite(tmp)
      if (length(tmp) == 0 || !any(ok)) NA_character_
      else tmp_slice[which.max(replace(tmp, !ok, -Inf))][1]
    },
    
    mlftau_peak_slice = {
      tmp <- mean_norm_importance[framework_group == "MLFTAU"]
      tmp_slice <- slice_id[framework_group == "MLFTAU"]
      ok <- is.finite(tmp)
      if (length(tmp) == 0 || !any(ok)) NA_character_
      else tmp_slice[which.max(replace(tmp, !ok, -Inf))][1]
    },
    
    .groups = "drop"
  ) %>%
  mutate(
    pisa_best_rank   = if_else(is.infinite(pisa_best_rank), NA_real_, pisa_best_rank),
    mlftau_best_rank = if_else(is.infinite(mlftau_best_rank), NA_real_, mlftau_best_rank),
    pisa_max_imp     = if_else(is.infinite(pisa_max_imp), NA_real_, pisa_max_imp),
    mlftau_max_imp   = if_else(is.infinite(mlftau_max_imp), NA_real_, mlftau_max_imp),
    
    sensitivity_type = case_when(
      pisa_topk & !mlftau_topk ~ "PISA-only (top-k)",
      !pisa_topk & mlftau_topk ~ "MLFTAU-only (top-k)",
      pisa_topk & mlftau_topk  ~ "Shared (top-k)",
      TRUE ~ "Other"
    )
  ) %>%
  # For “sensitive signals”, keep only the one-lens cases.
  filter(sensitivity_type %in% c("PISA-only (top-k)", "MLFTAU-only (top-k)")) %>%
  arrange(sensitivity_type, variable)

# ---- B) Context shifts within each lens group (including Global baselines), with strength metadata ----
context_shift <- rf_top_tagged %>%
  filter(framework_group %in% c("PISA", "MLFTAU", "Global")) %>%
  filter(is_topk) %>%
  group_by(framework_group, variable) %>%
  summarise(
    topk_in_school  = any(context == "InSchool"),
    topk_out_school = any(context == "OutOfSchool"),
    
    # Strength by context
    best_rank_in_school  = suppressWarnings(min(rank_in_slice[context == "InSchool"], na.rm = TRUE)),
    best_rank_out_school = suppressWarnings(min(rank_in_slice[context == "OutOfSchool"], na.rm = TRUE)),
    max_imp_in_school    = suppressWarnings(max(mean_norm_importance[context == "InSchool"], na.rm = TRUE)),
    max_imp_out_school   = suppressWarnings(max(mean_norm_importance[context == "OutOfSchool"], na.rm = TRUE)),
    
    peak_slice_in_school = {
      tmp <- mean_norm_importance[context == "InSchool"]
      tmp_slice <- slice_id[context == "InSchool"]
      ok <- is.finite(tmp)
      if (length(tmp) == 0 || !any(ok)) NA_character_
      else tmp_slice[which.max(replace(tmp, !ok, -Inf))][1]
    },
    
    peak_slice_out_school = {
      tmp <- mean_norm_importance[context == "OutOfSchool"]
      tmp_slice <- slice_id[context == "OutOfSchool"]
      ok <- is.finite(tmp)
      if (length(tmp) == 0 || !any(ok)) NA_character_
      else tmp_slice[which.max(replace(tmp, !ok, -Inf))][1]
    },
    
    .groups = "drop"
  ) %>%
  mutate(
    best_rank_in_school  = if_else(is.infinite(best_rank_in_school), NA_real_, best_rank_in_school),
    best_rank_out_school = if_else(is.infinite(best_rank_out_school), NA_real_, best_rank_out_school),
    max_imp_in_school    = if_else(is.infinite(max_imp_in_school), NA_real_, max_imp_in_school),
    max_imp_out_school   = if_else(is.infinite(max_imp_out_school), NA_real_, max_imp_out_school)
  ) %>%
  filter(xor(topk_in_school, topk_out_school)) %>%
  mutate(
    shift_type = if_else(topk_in_school, "InSchool-only (top-k)", "OutOfSchool-only (top-k)")
  ) %>%
  arrange(framework_group, shift_type, variable)

# ---------------------------
# 9) GOAL 3: CANDIDATE SET (SCORING THAT RESPECTS “PEAK SALIENCE”)
# ---------------------------
# Key design choice:
# - Recurrence is informative but NOT required.
# - A variable can be carried forward if it is extremely salient in one lens (e.g., top1/top3),
#   even if it appears only once.

# Prepare per-variable summary (slice-aware)
var_summary <- rf_signals %>%
  group_by(variable) %>%
  summarise(
    # peak metrics anywhere (across all slices/outcomes)
    best_rank_any = suppressWarnings(min(rank_in_slice, na.rm = TRUE)),
    max_importance_any = suppressWarnings(max(mean_norm_importance, na.rm = TRUE)),
    
    # breadth metrics among TOP-K only (slice-aware)
    n_topk_slices = n_distinct(slice_id[is_topk & eligible_for_convergence], na.rm = TRUE),
    n_framework_groups_topk = n_distinct(framework_group[is_topk & eligible_for_convergence], na.rm = TRUE),
    n_contexts_topk = n_distinct(context[is_topk & eligible_for_convergence], na.rm = TRUE),
    n_outcomes_topk = n_distinct(outcome[is_topk & eligible_for_convergence], na.rm = TRUE),
    
    # “peak salience flags”
    any_top1 = any(is_top1, na.rm = TRUE),
    any_top3 = any(rank_in_slice <= 3, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    best_rank_any = if_else(is.infinite(best_rank_any), NA_real_, best_rank_any),
    max_importance_any = if_else(is.infinite(max_importance_any), NA_real_, max_importance_any)
  ) %>%
  left_join(global_baseline_flags, by = "variable")

# Replace NA baseline values safely for scoring (Step 6 names)
var_summary <- var_summary %>%
  mutate(
    max_imp_global_all_any_outcome = coalesce(max_imp_global_all_any_outcome, 0),
    max_imp_global_in_any_outcome  = coalesce(max_imp_global_in_any_outcome, 0),
    max_imp_global_out_any_outcome = coalesce(max_imp_global_out_any_outcome, 0),
    
    appears_global_all = coalesce(appears_global_all, FALSE),
    appears_global_in  = coalesce(appears_global_in, FALSE),
    appears_global_out = coalesce(appears_global_out, FALSE),
    
    # Stabilise in case max_importance_any is NA (should not happen if data are clean, but safe)
    max_importance_any = coalesce(max_importance_any, 0)
  )

# Candidate score (transparent; weights can be tuned later)
candidate_scored <- var_summary %>%
  mutate(
    # A) global baseline salience (weighted; benchmarks)
    global_baseline_score =
      1.5 * max_imp_global_all_any_outcome +
      1.0 * pmax(max_imp_global_in_any_outcome, max_imp_global_out_any_outcome),
    
    # B) peak salience bonuses (explicitly supports one-off but extreme signals)
    peak_bonus =
      if_else(any_top1, 1.5, 0) +
      if_else(!any_top1 & any_top3, 0.75, 0),
    
    # C) breadth bonuses (secondary; not a gate)
    breadth_bonus =
      0.15 * n_topk_slices +
      0.25 * pmin(n_framework_groups_topk, 3) +
      0.20 * pmin(n_contexts_topk, 3) +
      0.10 * pmin(n_outcomes_topk, 3),
    
    # D) overall importance stabiliser (prevents purely rank-driven anomalies)
    importance_bonus = 1.5 * max_importance_any,
    
    candidate_score = global_baseline_score + peak_bonus + breadth_bonus + importance_bonus
  ) %>%
  arrange(desc(candidate_score), best_rank_any)

# ---------------------------
# 10) OUTCOME- AND CONTEXT-SPECIFIC CANDIDATE TABLES (CARRY-FORWARD VIEWS)
# ---------------------------
# Purpose:
# - Outcome-specific: top predictors per outcome (MATH/READ/SCIE) using only slices where outcome matches.
# - Context-specific: top predictors for InSchool vs OutOfSchool using only slices where context matches.
# These tables do NOT replace the main candidate_scored list; they are diagnostic/carry-forward views.

# Helper to summarise candidate evidence within any filtered slice subset
summarise_candidates_in_subset <- function(df_subset) {
  df_subset %>%
    group_by(variable) %>%
    summarise(
      best_rank_subset = suppressWarnings(min(rank_in_slice, na.rm = TRUE)),
      max_importance_subset = suppressWarnings(max(mean_norm_importance, na.rm = TRUE)),
      
      n_topk_slices_subset = n_distinct(slice_id[is_topk & eligible_for_convergence], na.rm = TRUE),
      n_framework_groups_topk_subset = n_distinct(framework_group[is_topk & eligible_for_convergence], na.rm = TRUE),
      n_contexts_topk_subset = n_distinct(context[is_topk & eligible_for_convergence], na.rm = TRUE),
      n_outcomes_topk_subset = n_distinct(outcome[is_topk & eligible_for_convergence], na.rm = TRUE),
      
      any_top1_subset = any(is_top1, na.rm = TRUE),
      any_top3_subset = any(rank_in_slice <= 3, na.rm = TRUE),
      
      peak_slice_subset = {
        tmp <- mean_norm_importance
        tmp_slice <- slice_id
        ok <- is.finite(tmp)
        if (length(tmp) == 0 || !any(ok)) NA_character_
        else tmp_slice[which.max(replace(tmp, !ok, -Inf))][1]
      },
      
      .groups = "drop"
    ) %>%
    mutate(
      best_rank_subset = if_else(is.infinite(best_rank_subset), NA_real_, best_rank_subset),
      max_importance_subset = if_else(!is.finite(max_importance_subset), NA_real_, max_importance_subset)
    )
}

# Subset scoring: same philosophy as Step 9, but computed only within the subset evidence space.
score_candidates_in_subset <- function(sub_summary) {
  sub_summary %>%
    mutate(
      peak_bonus_subset =
        if_else(any_top1_subset, 1.5, 0) +
        if_else(!any_top1_subset & any_top3_subset, 0.75, 0),
      
      breadth_bonus_subset =
        0.15 * n_topk_slices_subset +
        0.25 * pmin(n_framework_groups_topk_subset, 3) +
        0.20 * pmin(n_contexts_topk_subset, 3) +
        0.10 * pmin(n_outcomes_topk_subset, 3),
      
      importance_bonus_subset = 1.5 * coalesce(max_importance_subset, 0),
      
      subset_score = peak_bonus_subset + breadth_bonus_subset + importance_bonus_subset
    ) %>%
    arrange(desc(subset_score), best_rank_subset)
}

# ---- Outcome-specific candidate tables ----
outcomes <- c("MATH", "READ", "SCIE")

rf_outcome_candidates <- map_dfr(outcomes, function(o) {
  sub <- rf_signals %>% filter(outcome == o)
  
  sub_sum <- summarise_candidates_in_subset(sub) %>%
    score_candidates_in_subset() %>%
    mutate(outcome_focus = o)
  
  # Attach global baseline context (same columns as main carry-forward pack)
  sub_sum %>%
    left_join(global_baseline_flags, by = "variable") %>%
    left_join(
      mapping_tbl %>% transmute(variable = renamed_variable,
                                broad_learning_context = safe_col(., "broad_learning_context"),
                                pisa_domain = safe_col(., "pisa_domain"),
                                pisa_construct = safe_col(., "pisa_construct"),
                                mlftau_domain = safe_col(., "mlftau_domain"),
                                mlftau_construct = safe_col(., "mlftau_construct")),
      by = "variable"
    )
})

# Keep only top-N per outcome (adjust as needed)
top_n_outcome <- 50
rf_outcome_candidates_top <- rf_outcome_candidates %>%
  group_by(outcome_focus) %>%
  slice_head(n = top_n_outcome) %>%
  ungroup()

# ---- Context-specific candidate tables (InSchool vs OutOfSchool) ----
contexts_focus <- c("InSchool", "OutOfSchool")

rf_context_candidates <- map_dfr(contexts_focus, function(cx) {
  sub <- rf_signals %>% filter(context == cx)
  
  sub_sum <- summarise_candidates_in_subset(sub) %>%
    score_candidates_in_subset() %>%
    mutate(context_focus = cx)
  
  sub_sum %>%
    left_join(global_baseline_flags, by = "variable") %>%
    left_join(
      mapping_tbl %>% transmute(variable = renamed_variable,
                                broad_learning_context = safe_col(., "broad_learning_context"),
                                pisa_domain = safe_col(., "pisa_domain"),
                                pisa_construct = safe_col(., "pisa_construct"),
                                mlftau_domain = safe_col(., "mlftau_domain"),
                                mlftau_construct = safe_col(., "mlftau_construct")),
      by = "variable"
    )
})

top_n_context <- 75
rf_context_candidates_top <- rf_context_candidates %>%
  group_by(context_focus) %>%
  slice_head(n = top_n_context) %>%
  ungroup()

# Simple console sanity previews
cat(glue("\nStep 10: Outcome-specific candidate rows = {nrow(rf_outcome_candidates)} (top per outcome = {top_n_outcome}).\n"))
cat(glue("Step 10: Context-specific candidate rows = {nrow(rf_context_candidates)} (top per context = {top_n_context}).\n"))

# ---------------------------
# 11) RULE-BASED INTERACTION SHORTLIST (CIPO + MLFTAU, MODE SWITCH)
# ---------------------------
# Exploratory: “candidate interactions warranted by RF patterns”.
# Goal: pre-filter plausible interaction pairs (not equations).
#
# Design principle:
# - Use the SAME foundational structures as the RF lenses:
#   • PISA: CIPO (Context, Input, Process, Output)
#   • MLFTAU: Individual, Group/Class, Organisation/Family, Organisation/School, Organisation/Society, Output
#
# This step:
# 1) Builds variable families FROM mapping table labels (semicolon-safe).
# 2) Builds interaction candidates via expand_grid() based on a mode switch.
# 3) Restricts to top-N candidate_scored variables (by design).
# 4) Adds evidence notes showing each variable’s top-k provenance (frameworks/contexts/etc.).

# ---- Step 11 mode switch ----
# "combined"   = PISA (CIPO) + MLFTAU cross-level interactions (recommended default)
# "pisa_only"  = only PISA (CIPO) cross-level interactions
# "mlftau_only"= only MLFTAU cross-level interactions
interaction_mode <- "combined"  # change as needed


# ---------------------------
# 12) LABEL HANDLING UTILITIES (semicolon-safe)
# ---------------------------
split_labels <- function(x) {
  x <- if_else(is.na(x), "", as.character(x))
  x <- str_split(x, "\\s*;\\s*")
  map(x, ~ .x[.x != ""])
}

has_any_label <- function(df, col, targets) {
  labs <- split_labels(df[[col]])
  map_lgl(labs, ~ any(.x %in% targets))
}

get_label_universe <- function(df, col) {
  labs <- split_labels(df[[col]])
  sort(unique(unlist(labs)))
}

assert_labels_exist <- function(label_universe, targets, label_name) {
  missing <- setdiff(targets, label_universe)
  if (length(missing) > 0) {
    stop(glue(
      "Step 12 label check failed for {label_name}. These labels were not found in the mapping table:\n",
      paste0(" - ", missing, collapse = "\n")
    ))
  }
}


# ---------------------------
# 13) BUILD MAPPING SLICE (ONLY VARIABLES IN candidate_scored)
# ---------------------------
mapping_avail <- mapping_tbl %>%
  transmute(
    variable = renamed_variable,
    pisa_domain = safe_col(., "pisa_domain"),
    pisa_construct = safe_col(., "pisa_construct"),
    pisa_ict_school_domain = safe_col(., "pisa_ict_school_domain"),
    pisa_ict_school_construct = safe_col(., "pisa_ict_school_construct"),
    pisa_ict_home_domain = safe_col(., "pisa_ict_home_domain"),
    pisa_ict_home_construct = safe_col(., "pisa_ict_home_construct"),
    mlftau_domain = safe_col(., "mlftau_domain"),
    mlftau_construct = safe_col(., "mlftau_construct")
  ) %>%
  filter(variable %in% candidate_scored$variable)


# ---------------------------
# 14) TARGET LABELS (CONFIG) — EXACT STRINGS USED IN THE MAPPING TABLE
# ---------------------------
# PISA CIPO domains
pisa_cipo_context <- c("Context")
pisa_cipo_input   <- c("Input")
pisa_cipo_process <- c("Process")
pisa_cipo_output  <- c("Output")

# MLFTAU domains
mlftau_individual <- c("Individual")
mlftau_groupclass <- c("Group/Class")
mlftau_org_family <- c("Organisation/Family")
mlftau_org_school <- c("Organisation/School")
mlftau_org_society<- c("Organisation/Society")
mlftau_output     <- c("Output")

# ---- Validate that these labels exist in the mapping vocabulary (prevents silent failures) ----
pisa_domain_vocab   <- get_label_universe(mapping_avail, "pisa_domain")
mlftau_domain_vocab <- get_label_universe(mapping_avail, "mlftau_domain")

assert_labels_exist(pisa_domain_vocab,   pisa_cipo_context, "pisa_cipo_context")
assert_labels_exist(pisa_domain_vocab,   pisa_cipo_input,   "pisa_cipo_input")
assert_labels_exist(pisa_domain_vocab,   pisa_cipo_process, "pisa_cipo_process")
assert_labels_exist(pisa_domain_vocab,   pisa_cipo_output,  "pisa_cipo_output")

assert_labels_exist(mlftau_domain_vocab, mlftau_individual,  "mlftau_individual")
assert_labels_exist(mlftau_domain_vocab, mlftau_groupclass,  "mlftau_groupclass")
assert_labels_exist(mlftau_domain_vocab, mlftau_org_family,  "mlftau_org_family")
assert_labels_exist(mlftau_domain_vocab, mlftau_org_school,  "mlftau_org_school")
assert_labels_exist(mlftau_domain_vocab, mlftau_org_society, "mlftau_org_society")
assert_labels_exist(mlftau_domain_vocab, mlftau_output,      "mlftau_output")


# ---------------------------
# 15) DERIVE FAMILIES FROM MAPPING LABELS (semicolon-safe)
# ---------------------------
# PISA (CIPO) families
fam_context <- mapping_avail %>%
  filter(has_any_label(., "pisa_domain", pisa_cipo_context)) %>%
  pull(variable) %>% unique()

fam_input <- mapping_avail %>%
  filter(has_any_label(., "pisa_domain", pisa_cipo_input)) %>%
  pull(variable) %>% unique()

fam_process <- mapping_avail %>%
  filter(has_any_label(., "pisa_domain", pisa_cipo_process)) %>%
  pull(variable) %>% unique()

fam_output_pisa <- mapping_avail %>%
  filter(has_any_label(., "pisa_domain", pisa_cipo_output)) %>%
  pull(variable) %>% unique()

# MLFTAU families
fam_individual <- mapping_avail %>%
  filter(has_any_label(., "mlftau_domain", mlftau_individual)) %>%
  pull(variable) %>% unique()

fam_groupclass <- mapping_avail %>%
  filter(has_any_label(., "mlftau_domain", mlftau_groupclass)) %>%
  pull(variable) %>% unique()

fam_org <- mapping_avail %>%
  filter(has_any_label(., "mlftau_domain", c(mlftau_org_family, mlftau_org_school, mlftau_org_society))) %>%
  pull(variable) %>% unique()

fam_output_mlftau <- mapping_avail %>%
  filter(has_any_label(., "mlftau_domain", mlftau_output)) %>%
  pull(variable) %>% unique()


# ---------------------------
# 16) INTERACTION SEARCH SPACE (TOP-N ONLY, BY DESIGN)
# ---------------------------
top_n_for_interactions <- 60

top_candidates <- candidate_scored %>%
  slice_head(n = top_n_for_interactions) %>%
  select(variable, candidate_score)


# ---------------------------
# 17) GENERATE CANDIDATE PAIRS (MODE-AWARE expand_grid WRAPPER)
# ---------------------------
interaction_candidates <- bind_rows(
  
  # ----- PISA CIPO interactions (cross-level) -----
  # Context × Process; Input × Process are the most interpretable "conditions × mechanisms" pairs.
  if (interaction_mode %in% c("combined", "pisa_only"))
    expand_grid(A = fam_context, B = fam_process),
  
  if (interaction_mode %in% c("combined", "pisa_only"))
    expand_grid(A = fam_input,   B = fam_process),
  
  # ----- MLFTAU interactions (cross-level) -----
  # Organisation (Family/School/Society) × Individual is the cleanest cross-level MLFTAU interaction template.
  if (interaction_mode %in% c("combined", "mlftau_only"))
    expand_grid(A = fam_org,     B = fam_individual)
  
) %>%
  # defensive cleanup
  filter(!is.na(A), !is.na(B), A != B) %>%
  
  # enforce ordering to avoid mirrored duplicates
  mutate(A2 = pmin(A, B), B2 = pmax(A, B)) %>%
  select(A = A2, B = B2) %>%
  distinct() %>%
  
  # score only among top-N candidates
  left_join(top_candidates, by = c("A" = "variable")) %>% rename(score_A = candidate_score) %>%
  left_join(top_candidates, by = c("B" = "variable")) %>% rename(score_B = candidate_score) %>%
  mutate(pair_score = score_A + score_B) %>%
  filter(!is.na(pair_score)) %>%
  
  arrange(desc(pair_score)) %>%
  mutate(interaction = paste0(A, " × ", B)) %>%
  select(interaction, A, B, score_A, score_B, pair_score) %>%
  slice_head(n = 25)

cat(glue(
  "Step 17 diagnostic: interaction mode = {interaction_mode}; ",
  "shortlist size = {nrow(interaction_candidates)} ",
  "(after top-{top_n_for_interactions} restriction).\n"
))


# ---------------------------
# 18) EVIDENCE NOTES FOR INTERACTIONS (TOP-K PROVENANCE)
# ---------------------------
var_topk_evidence <- rf_signals %>%
  filter(is_topk & eligible_for_convergence) %>%
  group_by(variable) %>%
  summarise(
    topk_frameworks        = str_c(sort(unique(framework)), collapse = "; "),
    topk_framework_groups  = str_c(sort(unique(framework_group)), collapse = "; "),
    topk_contexts          = str_c(sort(unique(context)), collapse = "; "),
    topk_granularity       = str_c(sort(unique(granularity)), collapse = "; "),
    topk_n_slices          = n_distinct(slice_id),
    topk_best_rank         = min(rank_in_slice, na.rm = TRUE),
    topk_max_imp           = max(mean_norm_importance, na.rm = TRUE),
    .groups = "drop"
  )

clean_note <- function(x) {
  x <- as.character(x)
  x[x %in% c("NA", "", NA_character_)] <- NA_character_
  x
}

var_topk_evidence <- var_topk_evidence %>%
  mutate(
    topk_frameworks       = clean_note(topk_frameworks),
    topk_framework_groups = clean_note(topk_framework_groups),
    topk_contexts         = clean_note(topk_contexts),
    topk_granularity      = clean_note(topk_granularity)
  )

interaction_candidates <- interaction_candidates %>%
  left_join(
    var_topk_evidence %>% rename_with(~ paste0("A_", .x), -variable),
    by = c("A" = "variable")
  ) %>%
  left_join(
    var_topk_evidence %>% rename_with(~ paste0("B_", .x), -variable),
    by = c("B" = "variable")
  ) %>%
  mutate(
    evidence_note = str_c(
      "A: ", A, " [",
      "frameworks: ", coalesce(A_topk_frameworks, "none"), "; ",
      "groups: ", coalesce(A_topk_framework_groups, "none"), "; ",
      "contexts: ", coalesce(A_topk_contexts, "none"), "; ",
      "best_rank: ", coalesce(as.character(A_topk_best_rank), "NA"), "; ",
      "max_imp: ", coalesce(as.character(round(A_topk_max_imp, 3)), "NA"),
      "] | ",
      "B: ", B, " [",
      "frameworks: ", coalesce(B_topk_frameworks, "none"), "; ",
      "groups: ", coalesce(B_topk_framework_groups, "none"), "; ",
      "contexts: ", coalesce(B_topk_contexts, "none"), "; ",
      "best_rank: ", coalesce(as.character(B_topk_best_rank), "NA"), "; ",
      "max_imp: ", coalesce(as.character(round(B_topk_max_imp, 3)), "NA"),
      "]"
    )
  )

# ---------------------------
# 19) ONE-PAGE SYNTHESIS TABLE (CSV)
# ---------------------------

# Define join key explicitly (matches earlier logic: renamed variable)
mapping_key <- "renamed_variable"

# Helper to collapse labels safely (keeps ordering stable and drops blanks)
collapse_labels <- function(x) {
  x <- x[!is.na(x)]
  x <- str_trim(x)
  x <- x[x != ""]
  if (length(x) == 0) return("")
  str_c(sort(unique(x)), collapse = "; ")
}

# Helper to collapse PISA/MLFTAU “locations” (domains/constructs where top-k)
# Best-practice split:
# - Robust locations (for main synthesis): require eligible_for_convergence
# - Audit locations (for record-keeping): include all top-k slices, flagged with support diagnostics

build_location_summary <- function(df, robust_only = TRUE) {
  df2 <- df %>%
    filter(is_topk) %>%
    { if (robust_only) filter(., eligible_for_convergence) else . } %>%
    mutate(
      # Defensive framework group derivation (stable even if upstream changes)
      framework_group = case_when(
        str_detect(framework, "^PISA")   ~ "PISA",
        str_detect(framework, "^MLFTAU") ~ "MLFTAU",
        str_detect(framework, "^Global") ~ "Global",
        TRUE ~ "Other"
      ),
      domain = domain_path,
      construct = construct_path,
      loc = case_when(
        granularity == "Domain"    ~ str_c("domain=", domain),
        granularity == "Construct" ~ str_c("domain=", domain, " | construct=", construct),
        TRUE                       ~ str_c("domain=", domain, " | construct=", construct)
      )
    ) %>%
    group_by(variable) %>%
    summarise(
      pisa_locations   = str_c(sort(unique(loc[framework_group == "PISA"])),   collapse = "; "),
      mlftau_locations = str_c(sort(unique(loc[framework_group == "MLFTAU"])), collapse = "; "),
      global_locations = str_c(sort(unique(loc[framework_group == "Global"])), collapse = "; "),
      topk_contexts    = str_c(sort(unique(context)),   collapse = "; "),
      topk_frameworks  = str_c(sort(unique(framework)), collapse = "; "),
      .groups = "drop"
    )
  
  if (!robust_only) {
    # Add support diagnostics so small-slice signals are explicitly visible
    support_diag <- df %>%
      filter(is_topk) %>%
      group_by(variable) %>%
      summarise(
        topk_n_slices_all          = n_distinct(slice_id),
        topk_n_slices_eligible     = n_distinct(slice_id[eligible_for_convergence]),
        topk_n_slices_small        = n_distinct(slice_id[!eligible_for_convergence]),
        any_small_slice_support    = any(!eligible_for_convergence, na.rm = TRUE),
        min_n_in_slice_topk        = suppressWarnings(min(n_in_slice, na.rm = TRUE)),
        median_n_in_slice_topk     = suppressWarnings(median(n_in_slice, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      mutate(
        min_n_in_slice_topk    = if_else(is.infinite(min_n_in_slice_topk), NA_real_, min_n_in_slice_topk),
        median_n_in_slice_topk = if_else(is.infinite(median_n_in_slice_topk), NA_real_, median_n_in_slice_topk)
      )
    
    df2 <- df2 %>% left_join(support_diag, by = "variable")
  }
  
  df2
}

location_summary_robust <- build_location_summary(rf_top_tagged, robust_only = TRUE)
location_summary_audit  <- build_location_summary(rf_top_tagged, robust_only = FALSE)

# Bring mapping fields needed to build framework-grounded profiles
mapping_profile_tbl <- mapping_tbl %>%
  transmute(
    variable = .data[[mapping_key]],
    broad_learning_context = safe_col(., "broad_learning_context"),
    
    pisa_domain = safe_col(., "pisa_domain"),
    pisa_construct = safe_col(., "pisa_construct"),
    
    pisa_ict_school_domain = safe_col(., "pisa_ict_school_domain"),
    pisa_ict_school_construct = safe_col(., "pisa_ict_school_construct"),
    pisa_ict_school_subconstruct = safe_col(., "pisa_ict_school_subconstruct"),
    
    pisa_ict_home_domain = safe_col(., "pisa_ict_home_domain"),
    pisa_ict_home_construct = safe_col(., "pisa_ict_home_construct"),
    pisa_ict_home_subconstruct = safe_col(., "pisa_ict_home_subconstruct"),
    
    mlftau_domain = safe_col(., "mlftau_domain"),
    mlftau_construct = safe_col(., "mlftau_construct")
  )

one_page_synthesis <- candidate_scored %>%
  left_join(location_summary_robust, by = "variable") %>%
  rename(
    pisa_locations_robust   = pisa_locations,
    mlftau_locations_robust = mlftau_locations,
    global_locations_robust = global_locations,
    topk_contexts_robust    = topk_contexts,
    topk_frameworks_robust  = topk_frameworks
  ) %>%
  left_join(mapping_profile_tbl, by = "variable") %>%
  mutate(
    # Clean and standardise empty strings
    across(
      c(
        broad_learning_context,
        pisa_domain, pisa_construct,
        pisa_ict_school_domain, pisa_ict_school_construct, pisa_ict_school_subconstruct,
        pisa_ict_home_domain, pisa_ict_home_construct, pisa_ict_home_subconstruct,
        mlftau_domain, mlftau_construct,
        pisa_locations_robust, mlftau_locations_robust, global_locations_robust,
        topk_contexts_robust, topk_frameworks_robust
      ),
      ~ if_else(is.na(.x), "", str_trim(as.character(.x)))
    ),
    
    # Mechanism profiles: framework-grounded, not guessed
    pisa_profile = case_when(
      pisa_domain == "" & pisa_construct == "" ~ "",
      TRUE ~ str_c("PISA: ", pisa_domain,
                   if_else(pisa_construct != "", str_c(" | ", pisa_construct), ""))
    ),
    
    pisa_ict_in_profile = case_when(
      pisa_ict_school_domain == "" & pisa_ict_school_construct == "" & pisa_ict_school_subconstruct == "" ~ "",
      TRUE ~ str_c(
        "PISA_ICT_In: ", pisa_ict_school_domain,
        if_else(pisa_ict_school_construct != "", str_c(" | ", pisa_ict_school_construct), ""),
        if_else(pisa_ict_school_subconstruct != "", str_c(" | ", pisa_ict_school_subconstruct), "")
      )
    ),
    
    pisa_ict_out_profile = case_when(
      pisa_ict_home_domain == "" & pisa_ict_home_construct == "" & pisa_ict_home_subconstruct == "" ~ "",
      TRUE ~ str_c(
        "PISA_ICT_Out: ", pisa_ict_home_domain,
        if_else(pisa_ict_home_construct != "", str_c(" | ", pisa_ict_home_construct), ""),
        if_else(pisa_ict_home_subconstruct != "", str_c(" | ", pisa_ict_home_subconstruct), "")
      )
    ),
    
    mlftau_profile = case_when(
      mlftau_domain == "" & mlftau_construct == "" ~ "",
      TRUE ~ str_c("MLFTAU: ", mlftau_domain,
                   if_else(mlftau_construct != "", str_c(" | ", mlftau_construct), ""))
    )
  ) %>%
  rowwise() %>%
  mutate(
    mechanism_profile = collapse_labels(c(pisa_profile, pisa_ict_in_profile, pisa_ict_out_profile, mlftau_profile))
  ) %>%
  ungroup() %>%
  mutate(
    carry_forward_rationale = case_when(
      max_imp_global_all_any_outcome >= 0.75 ~ "High total salience in Global all-predictors baseline",
      pmax(max_imp_global_in_any_outcome, max_imp_global_out_any_outcome) >= 0.75 ~ "High context-conditional salience in Global in/out baseline",
      any_top1 ~ "Peak salience (rank 1) in at least one framework/context slice",
      any_top3 ~ "High salience (top 3) in at least one framework/context slice",
      n_topk_slices >= 2 ~ "Recurring top-k across multiple slices",
      TRUE ~ "Lower priority (retain only if theoretically required)"
    )
  ) %>%
  transmute(
    variable,
    
    # Global baselines
    appears_global_all,
    appears_global_in,
    appears_global_out,
    max_imp_global_all_any_outcome,
    max_imp_global_in_any_outcome,
    max_imp_global_out_any_outcome,
    
    # Peak / breadth
    best_rank_any,
    max_importance_any,
    n_topk_slices,
    n_framework_groups_topk,
    n_contexts_topk,
    any_top1,
    any_top3,
    
    # RF-lens locations (ROBUST)
    topk_frameworks_robust,
    topk_contexts_robust,
    global_locations_robust,
    pisa_locations_robust,
    mlftau_locations_robust,
    
    # Framework-grounded mechanism profiles
    mechanism_profile,
    pisa_profile,
    pisa_ict_in_profile,
    pisa_ict_out_profile,
    mlftau_profile,
    
    carry_forward_rationale,
    candidate_score
  ) %>%
  arrange(desc(candidate_score))

one_page_synthesis_top40 <- one_page_synthesis %>% slice_head(n = 40)

# ---------------------------
# 20) WRITE OUTPUTS
# ---------------------------

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

to_write <- list(
  rf_convergence_map_topk              = convergence_map,
  rf_framework_sensitive_topk          = framework_sensitive,
  rf_context_shift_topk                = context_shift,
  rf_candidate_predictors_scored       = candidate_scored,
  rf_interaction_candidates            = interaction_candidates,
  rf_one_page_synthesis_table_full     = one_page_synthesis,
  rf_one_page_synthesis_table_top40    = one_page_synthesis_top40,
  rf_location_summary_audit_all_slices = location_summary_audit,
  rf_outcome_specific_candidates_all   = rf_outcome_candidates,
  rf_outcome_specific_candidates_top   = rf_outcome_candidates_top,
  rf_context_specific_candidates_all   = rf_context_candidates,
  rf_context_specific_candidates_top   = rf_context_candidates_top
)

# Write each output with an explicit existence check + row count
walk(names(to_write), function(nm) {
  obj <- to_write[[nm]]
  
  if (is.null(obj)) {
    stop(glue("Step 12 write failed: object '{nm}' is NULL. Upstream step did not produce it."))
  }
  
  # Basic structural sanity: must be a data.frame / tibble
  if (!inherits(obj, "data.frame")) {
    stop(glue("Step 12 write failed: object '{nm}' is not a data.frame. Class: {class(obj)[1]}"))
  }
  
  out_path <- file.path(out_dir, paste0(nm, ".csv"))
  write_csv(obj, out_path)
  
  cat(glue("Wrote {nm}.csv ({nrow(obj)} rows)\n"))
})

written_files <- file.path(out_dir, paste0(names(to_write), ".csv"))
missing_files <- written_files[!file.exists(written_files)]
if (length(missing_files) > 0) {
  stop("Some outputs were not written: ", paste(basename(missing_files), collapse = ", "))
}

# ---------------------------
# 21) SANITY PREVIEWS (CONSOLE)
# ---------------------------

cat("\nRF synthesis outputs written to:\n", out_dir, "\n\n")

cat("Preview: One-page synthesis table (Top 15)\n")
print(one_page_synthesis %>% slice_head(n = 15))

cat("\nPreview: Framework-sensitive predictors (Top 15)\n")
print(framework_sensitive %>% slice_head(n = 15))

cat("\nPreview: Context shifts (Top 15)\n")
print(context_shift %>% slice_head(n = 15))

cat("\nPreview: Candidate interactions (Top 10)\n")
print(interaction_candidates %>% slice_head(n = 10))



# --------------------------------------
# Random Forest Model: Global In-Class Predictors Only
# --------------------------------------
# File: 03_scripts/02_random_forests/02_rf_global_in_class_predictors.R
# Purpose:
# This script runs the implemented global in-class random forest model used in
# the published Thailand PISA 2022 analysis. It estimates variable importance
# using all valid non-input, non-COVID predictors labeled "In School" or
# "In and Out" in the broad learning context classification across mathematics,
# reading, and science plausible values, then produces aggregated importance
# summaries and selected partial dependence plots.
#
# Main tasks:
# 1. Load the analysis-ready dataset and mapping file.
# 2. Exclude selected flag-based cases and non-eligible predictors.
# 3. Select valid in-class and in-and-out predictors.
# 4. Run random forest models across PV1–10 for MATH, READ, and SCIE.
# 5. Save raw and aggregated importance outputs and top-20 plots.
# 6. Update the model tracker.
# 7. Generate PDPs for the top global in-class predictors.
#
# Public repository note:
# This script is shared to support transparency regarding the published analysis.
# It reflects the implemented workflow used in the study, but some intermediate
# inputs, metadata resources, and output paths from the broader private project
# are not publicly released.

# Load libraries
library(dplyr)
library(readr)
library(randomForest)
library(ggplot2)
library(tibble)
library(stringr)
library(purrr)
library(glue)
library(fs)
library(pdp)

# -----------------------------------------------
# --- MANUAL PARAMETERS ---
# -----------------------------------------------
# Note: update these paths to match your own local environment and available inputs.
input_data <- "PATH_TO_LOCAL_INPUT/pisa2022_cleaned_19_composites_added.csv"
mapping_table_path <- "PATH_TO_LOCAL_METADATA/pisa2022_variable_mapping_table_final.csv"
tracker_path <- "PATH_TO_LOCAL_METADATA/pisa2022_master_model_tracker.csv"

# Public repository note:
# This script depends on a project-specific variable mapping file used in the
# implemented workflow. The full private mapping table is not publicly released.

framework <- "global"
outcomes <- c("MATH", "READ", "SCIE")
model_id <- "in_class"

output_folder_base <- file.path("PATH_TO_LOCAL_OUTPUT", "random_forests", "global", model_id)

# --- SET GLOBAL SEED ---
set.seed(1234)

dir_create(file.path(output_folder_base, "long_form_importance_tables"))
dir_create(file.path(output_folder_base, "variable_importance_tables"))
dir_create(file.path(output_folder_base, "plots"))
dir_create(file.path(output_folder_base, "plots/pdp_plots"))

# --- LOAD DATA ---
df <- read_csv(input_data, show_col_types = FALSE)
mapping <- read_csv(mapping_table_path, show_col_types = FALSE)

# -----------------------------------------------
# ---EXCLUSION PATTERNS---
# -----------------------------------------------

# --- EXCLUDE STUDENTS WITH TWO OR MORE FLAGS ---
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

df <- df %>%
  mutate(n_flags = rowSums(across(all_of(flag_vars), ~ . == "Flagged" & !is.na(.)))) %>%
  filter(n_flags < 2) %>%
  select(-n_flags)

cat("✅ Remaining sample size after exclusion:", nrow(df), "\n")

# --- EXCLUSION PATTERNS ---
exclude_patterns <- c(
  "^PV\\d", "^W_", "WVARSTRR", "SENWT", "VER_DAT", "^country_id$", "^school_id$",
  "^student_id$", "^assessment_cycle$", "^sampling_stratum$",
  "^subnational_region$", "^administration_mode$", "^st_effort_invested_pisa$", 
  "^st_effort_invested_marks_pisa$", "^st_effort_accurate_pisa$", 
  "^stdv_test_effort_actual$", "^stdv_test_effort_hypothetical$"
)

# --- FUNCTION: SELECT PREDICTORS ---
select_all_valid_predictors <- function() {
  mapping %>%
    filter(
      !str_detect(pisa_construct, regex("\\bGlobal Crises\\b", ignore_case = TRUE)),
      (input_variable == "No" | derived_variable == "Yes"),
      status == "Included",
      broad_learning_context %in% c("In School", "In and Out")
    ) %>%
    pull(renamed_variable) %>%
    intersect(names(df)) %>%
    discard(~ any(str_detect(.x, exclude_patterns)))
}

# --- FUNCTION: RUN RANDOM FOREST FOR ONE PV ---
run_rf_for_pv <- function(pv_name, df, predictors, weight_var) {
  df_sampled <- df %>%
    select(all_of(c(pv_name, predictors, weight_var))) %>%
    filter(!is.na(.data[[pv_name]]), !is.na(.data[[weight_var]])) %>%
    slice_sample(n = 5000, weight_by = !!sym(weight_var), replace = TRUE)
  
  cat("📊 [", pv_name, "] Sample size used for RF:", nrow(df_sampled), "\n")
  
  rf_model <- randomForest(
    formula = as.formula(paste(pv_name, "~ .")),
    data = df_sampled %>% select(-all_of(weight_var)),
    importance = TRUE
  )
  
  list(
    importance = importance(rf_model) %>%
      as.data.frame() %>%
      rownames_to_column("variable") %>%
      select(variable, IncNodePurity) %>%
      mutate(pv = pv_name),
    n_students_used = nrow(df_sampled)
  )
}

# -----------------------------------------------
# --- MAIN LOOP ---
# -----------------------------------------------
all_predictors <- select_all_valid_predictors()
cat("📦 Number of predictors included after exclusions:", length(all_predictors), "\n")

if (length(all_predictors) == 0) stop("❌ No predictors available for global model.")

for (outcome in outcomes) {
  cat("\n➡️ Running Random Forests for outcome:", outcome, "\n")
  
  prefix <- glue("{framework}_{model_id}_{tolower(outcome)}")
  
  pvs <- paste0("PV", 1:10, outcome)
  
  raw_output_file  <- file.path(output_folder_base, "long_form_importance_tables", glue("{prefix}_rf_raw_importance.csv"))
  output_file      <- file.path(output_folder_base, "variable_importance_tables", glue("{prefix}_rf_variable_importance.csv"))
  plot_file        <- file.path(output_folder_base, "plots", glue("{prefix}_rf_top20.png"))
  
  rf_results_list <- map(pvs, run_rf_for_pv, df = df, predictors = all_predictors, weight_var = "W_FSTUWT")
  rf_results_all <- bind_rows(map(rf_results_list, "importance"))
  n_students_used_vector <- map_int(rf_results_list, "n_students_used")
  
  avg_importance <- rf_results_all %>%
    group_by(variable) %>%
    summarise(mean_importance = mean(IncNodePurity, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(mean_importance))
  
  write_csv(rf_results_all, raw_output_file)
  write_csv(avg_importance, output_file)
  
  # Plot Top 20
  top20 <- avg_importance %>% slice_max(mean_importance, n = 20)
  ggplot(top20, aes(x = mean_importance, y = reorder(variable, mean_importance))) +
    geom_col(fill = "steelblue") +
    labs(x = "Mean Increase in Node Purity", y = "Variable") +
    theme_bw() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text = element_text(size = 12),
      panel.grid.major = element_line(color = "gray85"),
      panel.grid.minor = element_blank()
    )
  ggsave(plot_file, width = 10, height = 6, dpi = 300)
  
  # Tracker
  tracker_model_id <- paste0(framework, "_", model_id, "_", tolower(outcome))
  tracker <- if (file.exists(tracker_path)) {
    read_csv(tracker_path, show_col_types = FALSE) %>%
      mutate(across(c(n_predictors, sample_size_per_pv, n_students_used), as.integer))
  } else tibble()
  
  tracker_row <- tibble(
    model_id = tracker_model_id,
    target = paste0("PV1-10", outcome),
    predictors_used = "All valid in-class questionnaire and derived variables (excluding COVID variables)",
    subset_rule = "Included predictors labeled In School or In and Out",
    domain_source_column = "None", 
    description = paste(framework, model_id, outcome, "PVs"),
    date_run = format(Sys.Date(), "%d/%m/%y"),
    n_predictors = length(all_predictors),
    sample_size_per_pv = 5000,
    n_students_used = as.integer(mean(n_students_used_vector)),
    covid_excluded = "Yes",
    output_file = output_file,
    raw_result_file = raw_output_file,
    plot_file = plot_file,
    framework = framework,
    domain_or_construct = model_id
  )
  
  tracker <- bind_rows(tracker, tracker_row)
  write_csv(tracker, tracker_path)
  cat("📌 Master model tracker updated at:", tracker_path, "\n")
}

# -----------------------------------------------
# --- AGGREGATED NORMALIZED IMPORTANCE & PLOT ---
# -----------------------------------------------
summary_prefix <- glue("{framework}_{model_id}")
summary_top_predictors_file <- file.path(output_folder_base, glue("{summary_prefix}_rf_all_top_predictors_summary.csv"))
summary_top20_file          <- file.path(output_folder_base, glue("{summary_prefix}_rf_top20_predictors_by_importance.csv"))
summary_freq_file           <- file.path(output_folder_base, glue("{summary_prefix}_rf_predictor_frequency_table.csv"))
summary_plot_file           <- file.path(output_folder_base, glue("{summary_prefix}_rf_top20_predictors_by_importance.png"))

importance_files <- list.files(file.path(output_folder_base, "variable_importance_tables"), pattern = "\\.csv$", full.names = TRUE)

normalized_top20_list <- lapply(importance_files, function(file) {
  df <- read_csv(file, show_col_types = FALSE)
  if ("mean_importance" %in% names(df)) df <- df %>% rename(IncNodePurity = mean_importance)
  df %>%
    mutate(norm_importance = IncNodePurity / max(IncNodePurity, na.rm = TRUE)) %>%
    slice_max(norm_importance, n = 20)
})

all_top20 <- bind_rows(normalized_top20_list)
write_csv(all_top20, summary_top_predictors_file)

top20_importance <- all_top20 %>%
  group_by(variable) %>%
  summarise(mean_norm_importance = mean(norm_importance, na.rm = TRUE)) %>%
  arrange(desc(mean_norm_importance)) %>%
  slice_max(mean_norm_importance, n = 20)

write_csv(top20_importance, summary_top20_file)

predictor_frequency <- all_top20 %>% count(variable, sort = TRUE)
write_csv(predictor_frequency, summary_freq_file)

plot_data <- top20_importance %>% left_join(predictor_frequency, by = "variable")

top20_plot <- ggplot(plot_data, aes(x = mean_norm_importance, y = reorder(variable, mean_norm_importance))) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = paste0("Freq: ", n)), hjust = -0.1, size = 3.5) +
  labs(x = "Average Normalized Importance", y = "Predictor") +
  xlim(0, 1.1) +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 10),
    panel.grid.major = element_line(color = "gray85"),
    panel.grid.minor = element_blank()
  )

ggsave(filename = summary_plot_file, plot = top20_plot, width = 10, height = 8, dpi = 300)

# -----------------------------------------------
# Top 5 global predictors based on normalized importance
# -----------------------------------------------
global_top5 <- top20_importance %>% slice_max(mean_norm_importance, n = 5) %>% pull(variable)

# Notify user
cat("🔄 Constructing combined training dataset from PV1–10 × 3 outcomes...\n")

# Create a unified training dataset: 5000 rows for each of the 30 PVs
combined_df <- map_dfr(
  c("MATH", "READ", "SCIE"),
  function(subject) {
    map_dfr(1:10, function(pv_num) {
      target <- paste0("PV", pv_num, subject)
      df %>%
        select(all_of(c(target, global_top5, "W_FSTUWT"))) %>%
        filter(!is.na(.data[[target]]), !is.na(W_FSTUWT)) %>%
        slice_sample(n = 5000, weight_by = W_FSTUWT, replace = TRUE) %>%
        rename(score = !!target) %>%
        mutate(subject = subject)
    })
  }
)

# Notify that model training is starting
cat("⏳ Training global Random Forest model on PV1–10 × 3 outcomes...\n")

# Train a single global RF model
rf_global_model <- randomForest(
  formula = score ~ .,
  data = combined_df %>% select(-W_FSTUWT, -subject),  # subject can be kept for later faceting if needed
  importance = TRUE
)

# -----------------------------------------------
# Generate and save PDPs for each of the top 5 global predictors
# -----------------------------------------------
for (var in global_top5) {
  pdp_result <- partial(
    object = rf_global_model,
    pred.var = var,
    train = combined_df %>% select(-W_FSTUWT, -subject),
    type = "regression",
    plot = FALSE
  )
  
  pdp_plot <- autoplot(pdp_result, contour = FALSE) +
    geom_line(color = "steelblue", linewidth = 1.1) +
    labs(
      x = var,
      y = "Predicted Score"
    ) +
    theme_classic()
  
  ggsave(
    filename = file.path(output_folder_base, "plots/pdp_plots", paste0(framework, "_", model_id, "_pdp_", var, ".png")),
    plot = pdp_plot,
    width = 8, height = 5, dpi = 300
  )

  cat("✅ PDP saved for:", var, "\n")
}

# ✅ Final confirmation message
cat("🎉 All PDPs generated and saved successfully.\n")


# --------------------------------------
# Random Forest Model: MLFTAU Contextual ICT Outside-School Domains
# --------------------------------------
# File: 03_scripts/02_random_forests/10_rf_mlftau_contextual_ict_outside_domains.R
# Purpose:
# This script runs the implemented domain-level random forest models for the
# MLFTAU contextual ICT outside-school framework used in the published Thailand
# PISA 2022 analysis. It estimates variable importance for each MLFTAU domain
# using valid non-input, non-COVID outside-class predictors across mathematics,
# reading, and science plausible values, then produces aggregated importance
# summaries, domain-level top-20 plots, and selected partial dependence plots.
#
# Main tasks:
# 1. Load the analysis-ready dataset and mapping file.
# 2. Exclude selected flag-based cases.
# 3. Identify eligible predictors for each MLFTAU outside-school domain.
# 4. Run random forest models across PV1–10 for MATH, READ, and SCIE.
# 5. Save raw and aggregated importance outputs and top-20 plots per domain.
# 6. Update the model tracker.
# 7. Build combined normalized top-20 results across outcomes for each domain.
# 8. Generate PDPs for the top domain-level predictors.
#
# Public repository note:
# This script is shared to support transparency regarding the published analysis.
# It reflects the implemented workflow used in the study, but some intermediate
# inputs, metadata resources, and output paths from the broader private project
# are not publicly released.

# 📚 Load libraries
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
# --- FRAMEWORK SETTINGS ---
# -----------------------------------------------
framework_settings <- list(
  pisa = list(
    domain_source_column = "pisa_domain",
    description_prefix = "",
    framework_name = "pisa"
  ),
  pisa_ict_in_class = list(
    domain_source_column = "pisa_ict_school_domain",
    description_prefix = "PISA Contextual ICT In-Class ",
    framework_name = "pisa_ict_in_class"
  ),
  pisa_ict_outside = list(
    domain_source_column = "pisa_ict_home_domain",
    description_prefix = "PISA Contextual ICT Outside-Class ",
    framework_name = "pisa_ict_outside"
  ),
  mlftau = list(
    domain_source_column = "mlftau_domain",
    description_prefix = "MLFTAU Contextual ICT Outside School ",
    framework_name = "mlftau"
  )
)

# -----------------------------------------------
# --- MANUAL PARAMETERS ---
# -----------------------------------------------
# Note: update these paths to match your own local environment and available inputs.
input_data <- "PATH_TO_LOCAL_INPUT/pisa2022_cleaned_19_composites_added.csv"
mapping_table_path <- "PATH_TO_LOCAL_METADATA/pisa2022_variable_mapping_table_final.csv"
tracker_path <- "PATH_TO_LOCAL_METADATA/pisa2022_master_model_tracker.csv"
output_folder_base <- file.path("PATH_TO_LOCAL_OUTPUT", "random_forests", "mlftau_contextual_ict_outside", "domains")

# Public repository note:
# This script depends on a project-specific variable mapping file used in the
# implemented workflow. The full private mapping table is not publicly released.

# --- SET GLOBAL SEED ---
set.seed(1234)

# --- SELECT FRAMEWORK ---
framework_choice <- "mlftau"  # <--- Just change this to run pisa, mlftau, etc.
framework_label <- "mlftau_contextual_ict_outside"
framework_info <- framework_settings[[framework_choice]]

domain_source_column <- framework_info$domain_source_column
description_prefix <- framework_info$description_prefix
framework <- "mlftau_contextual_ict_outside"

outcomes <- c("MATH", "READ", "SCIE")
domains_capitalized <- c(
  "Individual", 
  "Group/Class", 
  "Organisation/Family", 
  "Organisation/School", 
  "Organisation/Society", 
  "Output"
)  # For modeling
domains_lowercase <- c(
  "individual", 
  "group_class", 
  "organisation_family", 
  "organisation_school", 
  "organisation_society", 
  "output"
)  # For folder creation

# --- Create necessary subfolders under each domain ---
subfolders <- c("long_form_importance_tables", "variable_importance_tables", "plots")

for (domain in domains_lowercase) { 
  for (subfolder in subfolders) {
    dir_create(file.path(output_folder_base, domain, subfolder))
  }
}

# --- LOAD DATA ---
df <- read_csv(input_data, show_col_types = FALSE)
mapping <- read_csv(mapping_table_path, show_col_types = FALSE)

# -----------------------------------------------
# --- EXCLUSION PATTERNS ---
# -----------------------------------------------
# --- EXCLUDE STUDENTS WITH TWO OR MORE FLAGS (using "Flagged") ---
flag_vars <- c(
  "perception_straight_line_flag",
  "feeling_straight_line_flag",
  "persistence_straight_line_flag",
  "math_class_periods_flag",
  "total_class_periods_flag"
)

# Check all flag columns exist
missing_flags <- setdiff(flag_vars, colnames(df))
if (length(missing_flags) > 0) {
  stop("Missing flag columns: ", paste(missing_flags, collapse = ", "))
}

# Count how many flags each student received
df <- df %>%
  mutate(n_flags = rowSums(across(all_of(flag_vars), ~ . == "Flagged" & !is.na(.))))

# Record exclusion count and apply filter
n_before <- nrow(df)
df <- df %>% filter(n_flags < 2) %>% select(-n_flags)
n_after <- nrow(df)

# Log result
cat("⚠️", n_before - n_after, "students removed due to ≥2 flags.\n")
cat("✅ Remaining sample size after exclusion:", n_after, "\n")

exclude_patterns <- c(
  "^PV\\d", "^W_", "WVARSTRR", "SENWT", "VER_DAT", "^country_id$", "^school_id$", 
  "^student_id$", "^assessment_cycle$", "^sampling_stratum$", 
  "^subnational_region$", "^administration_mode$", "^st_effort_invested_pisa$", 
  "^st_effort_invested_marks_pisa$", "^st_effort_accurate_pisa$", "^stdv_test_effort_actual$",
  "^stdv_test_effort_hypothetical$"
)

exclude_regex <- paste(exclude_patterns, collapse = "|")

# --- DEFINE weight variable ---
weight_var <- "W_FSTUWT"

# -----------------------------------------------
# --- FUNCTION: SELECT PREDICTORS PER DOMAIN ---
# -----------------------------------------------
select_predictors_for_domain <- function(domain_name) {
  mapping %>%
    filter(
      str_detect(.data[[domain_source_column]], fixed(domain_name)),
      !str_detect(pisa_construct, regex("\\bGlobal Crises\\b", ignore_case = TRUE)),  # Exclude COVID
      (input_variable == "No" | derived_variable == "Yes"),  # Exclude inputs unless derived
      status == "Included"
    ) %>%
    filter(
      broad_learning_context %in% c("Out of School", "In and Out")
    ) %>%
    pull(renamed_variable) %>%
    intersect(names(df)) %>%
    discard(~ str_detect(.x, exclude_regex))
}

# -----------------------------------------------
# --- FUNCTION: RUN RANDOM FOREST FOR ONE PV ---
# -----------------------------------------------
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
# --- MAIN LOOP: LOOP OVER DOMAINS ---
# -----------------------------------------------
for (i in seq_along(domains_capitalized)) {
  domain_cap <- domains_capitalized[i]
  domain_folder <- domains_lowercase[i]
  domain_output_base  <- file.path(output_folder_base, domain_folder)
  
  cat("\n🎯 Running Random Forests for domain:", domain_cap, "\n")
  
  predictors_all <- select_predictors_for_domain(domain_cap)
  
  if (length(predictors_all) == 0) {
    cat("⚠️ No predictors found for domain:", domain_cap, "- skipping.\n")
    next
  }
  
  # Initialize container to store normalized tables per outcome
  normalized_results_all <- list()
  
  for (outcome in outcomes) {
    cat("\n➡️ Running Random Forests for outcome:", outcome, "\n")
    
    pvs <- paste0("PV", 1:10, outcome)
    
    # --- DEFINE OUTPUT PATHS ---
    raw_output_subfolder <- file.path(domain_output_base, "long_form_importance_tables")
    output_subfolder     <- file.path(domain_output_base, "variable_importance_tables")
    plot_subfolder       <- file.path(domain_output_base, "plots")
    
    filename_base <- paste0(framework_label, "_rf_", tolower(outcome), "_", domain_folder)
    
    raw_output_file <- file.path(raw_output_subfolder, paste0(filename_base, "_raw_importance.csv"))
    output_file     <- file.path(output_subfolder, paste0(filename_base, "_variable_importance.csv"))
    plot_file       <- file.path(plot_subfolder, paste0(filename_base, "_top20.png"))
    
    # --- RUN RANDOM FORESTS ---
    rf_results_list <- map(pvs, run_rf_for_pv, df = df, predictors = predictors_all, weight_var = weight_var)
    
    # --- AGGREGATE IMPORTANCE ---
    rf_results_all <- bind_rows(map(rf_results_list, "importance"))
    n_students_used_vector <- map_int(rf_results_list, "n_students_used")
    
    avg_importance <- rf_results_all %>%
      group_by(variable) %>%
      summarise(mean_importance = mean(IncNodePurity, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(mean_importance))
    
    # Normalize and store per outcome
    normalized_results_all[[outcome]] <- avg_importance %>%
      mutate(
        norm_importance = mean_importance / max(mean_importance, na.rm = TRUE),
        outcome = outcome
      ) %>%
      select(variable, norm_importance, outcome)
    
    # --- SAVE OUTPUTS ---
    write_csv(rf_results_all, raw_output_file)
    write_csv(avg_importance, output_file)
    
    # --- PLOT TOP 20 ---
    top20 <- avg_importance %>% slice_max(mean_importance, n = 20)
    ggplot(top20, aes(x = mean_importance, y = reorder(variable, mean_importance))) +
      geom_col(fill = "steelblue") +
      labs(
        title = paste("Top 20 Variable Importances (", outcome, "-", domain_cap, ")"),
        x = "Mean Increase in Node Purity",
        y = "Variable"
      ) +
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
    cat("📈 Plot saved to:", plot_file, "\n")
    
    # --- UPDATE MASTER MODEL TRACKER ---
    model_id <- paste0("rf_", framework_label, "_", tolower(domain_cap))
    tracker_model_id <- paste0(model_id, "_", tolower(outcome))
    
    if (file.exists(tracker_path)) {
      tracker <- read_csv(tracker_path, show_col_types = FALSE) %>%
        mutate(
          n_predictors = as.integer(n_predictors),
          sample_size_per_pv = as.integer(sample_size_per_pv),
          n_students_used = as.integer(n_students_used)
        )
    } else {
      tracker <- tibble()
    }
    
    tracker_row <- tibble(
      model_id = tracker_model_id,
      target = paste0("PV1-10", outcome),
      predictors_used = "Non-input questionnaire variables and derived variables (excluding COVID variables)",
      subset_rule = "Included predictors labeled Out of School or In and Out",
      domain_source_column = domain_source_column,
      description = paste(description_prefix, domain_cap, "~", outcome, "PVs"),
      date_run = format(Sys.Date(), "%d/%m/%y"),
      n_predictors = as.integer(length(predictors_all)),
      sample_size_per_pv = as.integer(5000),
      n_students_used = as.integer(mean(n_students_used_vector)),
      covid_excluded = "Yes",
      output_file = output_file,
      raw_result_file = raw_output_file,
      plot_file = plot_file,
      framework = framework, 
      domain_or_construct = paste0(framework_choice, ":", domain_cap)
    )
    
    tracker <- bind_rows(tracker, tracker_row)
    write_csv(tracker, tracker_path)
    cat("📌 Master model tracker updated at:", tracker_path, "\n")
  }
  
  # -----------------------------------------------
  # ---- COMBINE NORMALIZED TOP 20 RESULTS ACROSS OUTCOMES ----
  # -----------------------------------------------
  combined_normalized <- bind_rows(normalized_results_all)
  
  top20_combined <- combined_normalized %>%
    group_by(variable) %>%
    summarise(mean_norm_importance = mean(norm_importance, na.rm = TRUE), n = n(), .groups = "drop") %>%
    arrange(desc(mean_norm_importance)) %>%
    slice_max(mean_norm_importance, n = 20)
  
  # Save combined normalized table
  norm_combined_file <- file.path(domain_output_base, 
                                  paste0("rf_combined_normalized_importance_", domain_folder, ".csv"))
  write_csv(combined_normalized, norm_combined_file)
  cat("✅ Combined normalized importance table saved to:", norm_combined_file, "\n")
  
  # Plot combined Top 20
  norm_combined_plot <- file.path(domain_output_base, 
                                  paste0("rf_top20_combined_normalized_", domain_folder, ".png"))
  
  ggplot(top20_combined, aes(x = mean_norm_importance, y = reorder(variable, mean_norm_importance))) +
    geom_col(fill = "steelblue") +
    geom_text(aes(label = paste0("Freq: ", n)), hjust = -0.1, size = 3.5) +
    labs(
      x = "Average Normalized Importance",
      y = "Predictor"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 10),
      panel.grid.major = element_line(color = "gray85"),
      panel.grid.minor = element_blank()
    )
  
  ggsave(norm_combined_plot, width = 10, height = 8, dpi = 300)
  cat("✅ Combined normalized plot saved to:", norm_combined_plot, "\n")
  
  # -----------------------------------------------
  # Generate and save PDPs for top 5 predictors in this domain
  # -----------------------------------------------
  domain_top5 <- top20_combined %>%
    slice_max(mean_norm_importance, n = 5) %>%
    pull(variable)
  
  cat("🔄 Constructing combined training dataset from PV1–10 × 3 outcomes...\n")
  
  combined_df <- map_dfr(
    c("MATH", "READ", "SCIE"),
    function(subject) {
      map_dfr(1:10, function(pv_num) {
        target <- paste0("PV", pv_num, subject)
        df %>%
          select(all_of(c(target, domain_top5, "W_FSTUWT"))) %>%
          filter(!is.na(.data[[target]]), !is.na(W_FSTUWT)) %>%
          slice_sample(n = 5000, weight_by = W_FSTUWT, replace = TRUE) %>%
          rename(score = !!target) %>%
          mutate(subject = subject)
      })
    }
  )
  
  cat("⏳ Training domain-level Random Forest model...\n")
  
  rf_model <- randomForest(
    formula = score ~ .,
    data = combined_df %>% select(-W_FSTUWT, -subject),
    importance = TRUE
  )
  
  # Create output folder if needed
  dir_create(file.path(domain_output_base, "pdp_plots"))
  
  # Save PDPs
  for (var in domain_top5) {
    # Skip PDP if the variable is not numeric or has <= 2 unique values
    if (!is.numeric(combined_df[[var]]) || n_distinct(combined_df[[var]]) <= 2) {
      cat("⚠️ Skipping PDP for", var, "- not numeric or has ≤ 2 unique values.\n")
      next
    }
    # Compute PDP
    pdp_result <- partial(
      object = rf_model,
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
      filename = file.path(
        domain_output_base,
        "pdp_plots",
        paste0(
          framework_label,
          "_pdp_",
          domain_folder,
          "_",
          var,
          ".png"
        )
      ),
      plot = pdp_plot,
      width = 8,
      height = 5,
      dpi = 300
    )
    
    cat("✅ PDP saved for:", var, "\n")
  }
  
  cat("🎉 All PDPs generated successfully for domain:", domain_cap, "\n")
}

cat("🏁 All domains processed. Random Forest modeling and PDP generation complete.\n")


# --------------------------------------
# Random Forest Model: MLFTAU Contextual In-School Constructs
# --------------------------------------
# This RF script:
# - Identifies all analytically eligible in-class predictors using:
#     • MLFTAU construct labels (mlftau_construct)
#     • MLFTAU domain labels (mlftau_domain)
# - Includes only variables that are:
#     • Marked as "Included" in the mapping table
#     • Non-input or derived variables
#     • Associated with in-class learning contexts ("In School" or "In and Out")
# - Groups predictors by MLFTAU construct and its associated MLFTAU domain(s)
# - Ensures that both constructs and domains are sourced exclusively from
#   the MLFTAU framework (no PISA construct labels are used for construct grouping)
# - Excludes:
#     • COVID-related variables (Global Crises)
#     • Plausible values, weights, identifiers, and metadata columns from predictors
# - Uses W_FSTUWT for weighted resampling but never as a predictor
# - Fits Random Forest models using PV1–PV10 for each outcome:
#     • MATH, READ, and SCIE
# - Organizes outputs by construct and domain with directory structure:
#     • /constructs/{mlftau_domain}/{mlftau_construct}/
#     • Raw and averaged importance tables per outcome
#     • Top-20 variable importance plots per outcome
#     • Combined normalized importance tables and plots per construct
# - Logs full model metadata to the master model tracker for reproducibility
# --------------------------------------

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
    domain_source_column = "pisa_ict_outside_domain",
    description_prefix = "PISA Contextual ICT Outside-Class ",
    framework_name = "pisa_ict_outside"
  ),
  mlftau = list(
    domain_source_column = "mlftau_domain",
    description_prefix = "MLFTAU Contextual ICT In-Class ",
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
output_folder_base <- file.path("PATH_TO_LOCAL_OUTPUT", "random_forests", "mlftau_contextual_ict_in_class", "constructs")

# Public repository note:
# This script depends on a project-specific variable mapping file used in the
# implemented workflow. The full private mapping table is not publicly released.

# --- SET GLOBAL SEED ---
set.seed(1234)

# --- SELECT FRAMEWORK ---
framework_choice <- "mlftau"
framework_label <- "mlftau_contextual_ict_in_class"
framework_info <- framework_settings[[framework_choice]]

domain_mapping_column <- framework_settings[[framework_choice]]$domain_source_column
description_prefix <- framework_info$description_prefix
framework <- "mlftau_contextual_ict_in_class"

outcomes <- c("MATH", "READ", "SCIE")

# --- LOAD DATA ---
df <- read_csv(input_data, show_col_types = FALSE)
mapping <- read_csv(mapping_table_path, show_col_types = FALSE)

# -----------------------------------------------
# --- FILTER MAPPING TABLE TO ANALYTICALLY ELIGIBLE VARIABLES ---
# -----------------------------------------------

mapping_filtered <- mapping %>%
  filter(
    status == "Included",
    (input_variable == "No" | derived_variable == "Yes"),
    broad_learning_context %in% c("In School", "In and Out"),
    !str_detect(pisa_construct, regex("\\bGlobal Crises\\b", ignore_case = TRUE))
  )

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

# --- EXCLUSION PATTERNS: PVs, Weights, Metadata, IDs ---
exclude_patterns <- c(
  "^PV\\d", "^W_", "WVARSTRR", "SENWT", "VER_DAT", "^country_id$", "^school_id$", 
  "^student_id$", "^assessment_cycle$", "^sampling_stratum$", 
  "^subnational_region$", "^administration_mode$", "^st_effort_invested_pisa$", 
  "^st_effort_invested_marks_pisa$", "^st_effort_accurate_pisa$", "^stdv_test_effort_actual$",
  "^stdv_test_effort_hypothetical$"
)

# Collapse exclusion patterns into a single regex for safe detection
exclude_regex <- paste(exclude_patterns, collapse = "|")

# --- DEFINE weight variable ---
weight_var <- "W_FSTUWT"


# -----------------------------------------------
# --- FUNCTION: SELECT PREDICTORS PER DOMAIN ---
# -----------------------------------------------
select_predictors_for_construct <- function(construct_label, domain_label) {
  
  mapping_filtered_local <- mapping_filtered %>%
    filter(
      !is.na(mlftau_construct),
      map_lgl(str_split(mlftau_construct, ";"), ~ construct_label %in% str_trim(.x))
    ) %>%
    filter(
      !is.na(mlftau_domain),
      map_lgl(str_split(mlftau_domain, ";"), ~ domain_label %in% str_trim(.x))
    )
  
  mapping_filtered_local %>%
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
    slice_sample(n = 5000, weight_by = !!sym(weight_var), replace = TRUE)  # Sampling with replacement
  
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
# --- MAIN LOOP: LOOP OVER CONSTRUCTS ---
# -----------------------------------------------
# The implemented workflow identifies all eligible MLFTAU in-school constructs
# from the filtered mapping table before pairing each construct with its associated domain(s).
constructs <- mapping_filtered %>%
  filter(!is.na(mlftau_construct), !mlftau_construct %in% c("", "Not Applicable")) %>%
  pull(mlftau_construct) %>%
  str_split(";") %>% unlist() %>% str_trim() %>%
  discard(~ .x %in% c("", "Not Applicable")) %>%
  unique()

# -----------------------------------------------
# --- MLFTAU-ONLY DOMAIN LOOKUP FOR EACH CONSTRUCT ---
# -----------------------------------------------
get_domains_for_construct <- function(construct_label) {
  mapping_filtered %>%
    filter(
      !is.na(mlftau_construct),
      map_lgl(str_split(mlftau_construct, ";"), ~ construct_label %in% str_trim(.x))
    ) %>%
    pull(mlftau_domain) %>%
    str_split(";") %>% flatten_chr() %>% str_trim() %>%
    discard(~ .x %in% c("", "Not Applicable")) %>%
    unique()
}

for (construct_label in constructs) {
  construct_folder <- construct_label %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("_$", "")
  
  # 🔎 Get domains using custom fallback logic
  associated_domains <- get_domains_for_construct(construct_label)
  associated_domains <- associated_domains[associated_domains %in% c(
    "Individual", "Group/Class", "Organisation/School", "Organisation/Family", "Organisation/Society", "Output"
  )]
  cat("🔍 Domains associated with construct", construct_label, ":", paste(associated_domains, collapse = ", "), "\n")
  
  for (parent_domain in associated_domains) {
    
    # Make domain safe for folder names, filenames, and IDs (handles "/" etc.)
    parent_domain_safe <- parent_domain %>%
      str_to_lower() %>%
      str_replace_all("[^a-z0-9]+", "_") %>%
      str_replace_all("_$", "")
    
    cat("\n🎯 Running Random Forests for construct:", construct_label, 
        "under domain:", parent_domain, "\n")
    
    predictors_all <- select_predictors_for_construct(construct_label, parent_domain)
    
    cat("📦 Number of predictors included after exclusions:", length(predictors_all), "\n")
    
    if (length(predictors_all) == 0) {
      cat("⚠️ No predictors found for construct:", construct_label, "- skipping.\n")
      next
    }
    
    # Create folders
    construct_output_base <- file.path(output_folder_base, parent_domain_safe, construct_folder)
    long_form_dir <- file.path(construct_output_base, "long_form_importance_tables")
    variable_table_dir <- file.path(construct_output_base, "variable_importance_tables")
    plot_dir <- file.path(construct_output_base, "plots")
    
    dir_create(long_form_dir)
    dir_create(variable_table_dir)
    dir_create(plot_dir)
    
    
    # Initialize container to store normalized tables per outcome
    normalized_results_all <- list()
    
    for (outcome in outcomes) {
      cat("\n➡️ Running Random Forests for outcome:", outcome, "\n")
      
      pvs <- paste0("PV", 1:10, outcome)
      
      # --- DEFINE OUTPUT PATHS ---
      filename_base <- paste0(
        tolower(outcome), "_", framework_label, "_", 
        parent_domain_safe, "_", construct_folder
      )
      
      raw_output_file <- file.path(long_form_dir, paste0("rf_raw_importance_", filename_base, ".csv"))
      output_file     <- file.path(variable_table_dir, paste0("rf_variable_importance_", filename_base, ".csv"))
      plot_file       <- file.path(plot_dir, paste0("rf_top20_", filename_base, ".png"))
      
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
      model_id <- paste0(
        "rf_", framework_label, "_",
        parent_domain_safe, "_",
        construct_folder
      )
      
      tracker_model_id <- paste0(model_id, "_", str_to_lower(outcome))
      tracker <- if (file.exists(tracker_path)) {
        read_csv(tracker_path, show_col_types = FALSE) %>%
          mutate(across(c(n_predictors, sample_size_per_pv, n_students_used), as.integer))
      } else tibble()
      
      tracker_row <- tibble(
        model_id = tracker_model_id,
        target = paste0("PV1-10", outcome),
        predictors_used = "All valid in-class questionnaire and derived variables (excluding COVID variables)",
        subset_rule = "Included predictors labeled In School or In and Out",
        domain_source_column = domain_mapping_column,
        description = paste(description_prefix, construct_label, "~", outcome, "PVs"),
        date_run = format(Sys.Date(), "%d/%m/%y"),
        n_predictors = as.integer(length(predictors_all)),
        sample_size_per_pv = as.integer(5000),
        n_students_used = as.integer(mean(n_students_used_vector)),
        covid_excluded = "Yes",
        output_file = output_file,
        raw_result_file = raw_output_file,
        plot_file = plot_file,
        framework = framework, 
        domain_or_construct = paste0("construct:", construct_label, " | domain:", parent_domain)
      )
      
      tracker <- tracker %>% filter(model_id != tracker_model_id)
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
    
    # -----------------------------------------------
    # --- SAVE COMBINED NORMALIZED RESULTS ---
    # -----------------------------------------------
    
    # 1) Long-form normalized table (variable × outcome)
    long_form_file <- file.path(
      construct_output_base,
      paste0("rf_combined_normalized_long_", construct_folder, ".csv")
    )
    
    write_csv(combined_normalized, long_form_file)
    cat("✅ Long-form normalized importance table saved to:", long_form_file, "\n")
    
    
    # 2) Aggregated Top-20 normalized table (collapsed across outcomes)
    top20_file <- file.path(
      construct_output_base,
      paste0("rf_top20_combined_normalized_", construct_folder, ".csv")
    )
    
    write_csv(top20_combined, top20_file)
    cat("✅ Aggregated Top-20 normalized table saved to:", top20_file, "\n")
    
    # -----------------------------------------------
    # --- PLOT COMBINED TOP 20 ---
    # -----------------------------------------------
    norm_combined_plot <- file.path(construct_output_base, 
                                    paste0("rf_top20_combined_normalized_", construct_folder, ".png"))
    
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
  }
}

cat("🏁 All constructs processed. Random Forest modeling complete.\n")


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
    domain_source_column = "pisa_ict_outside_domain",
    description_prefix = "PISA Contextual ICT Outside-Class ",
    framework_name = "pisa_ict_outside"
  ),
  mlftau = list(
    domain_source_column = "mlftau_domain",
    description_prefix = "MLFTAU Contextual ICT In-Class ",
    framework_name = "mlftau"
  )
)

# -----------------------------------------------
# --- MANUAL PARAMETERS ---
# -----------------------------------------------
input_data <- "data/private/processed/2022/pisa2022_cleaned_19_composites_added.csv"
mapping_table_path <- "data/private/metadata/2022/pisa2022_variable_mapping_table_final.csv"
tracker_path <- "data/private/metadata/2022/pisa2022_master_model_tracker.csv"
output_folder_base <- "output/private/exploratory_data_analysis/2022/random_forests/mlftau_contextual_ict_in_class/constructs"

# --- SET GLOBAL SEED ---
set.seed(1234)

# --- SELECT FRAMEWORK ---
framework_choice <- "mlftau"
framework_label <- "mlftau_contextual_ict_in_class"
framework_info <- framework_settings[[framework_choice]]

domain_mapping_column <- framework_settings[[framework_choice]]$domain_source_column
description_prefix <- framework_info$description_prefix
framework <- "mlftau_contextual_ict_in_class"

outcomes <- c("MATH", "READ", "SCIE")

# --- LOAD DATA ---
df <- read_csv(input_data, show_col_types = FALSE)
mapping <- read_csv(mapping_table_path, show_col_types = FALSE)

# -----------------------------------------------
# --- FILTER MAPPING TABLE TO ANALYTICALLY ELIGIBLE VARIABLES ---
# -----------------------------------------------

mapping_filtered <- mapping %>%
  filter(
    status == "Included",
    (input_variable == "No" | derived_variable == "Yes"),
    broad_learning_context %in% c("In School", "In and Out"),
    !str_detect(pisa_construct, regex("\\bGlobal Crises\\b", ignore_case = TRUE))
  )

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

# --- EXCLUSION PATTERNS: PVs, Weights, Metadata, IDs ---
exclude_patterns <- c(
  "^PV\\d", "^W_", "WVARSTRR", "SENWT", "VER_DAT", "^country_id$", "^school_id$", 
  "^student_id$", "^assessment_cycle$", "^sampling_stratum$", 
  "^subnational_region$", "^administration_mode$", "^st_effort_invested_pisa$", 
  "^st_effort_invested_marks_pisa$", "^st_effort_accurate_pisa$", "^stdv_test_effort_actual$",
  "^stdv_test_effort_hypothetical$"
)

# Collapse exclusion patterns into a single regex for safe detection
exclude_regex <- paste(exclude_patterns, collapse = "|")

# --- DEFINE weight variable ---
weight_var <- "W_FSTUWT"


# -----------------------------------------------
# --- FUNCTION: SELECT PREDICTORS PER DOMAIN ---
# -----------------------------------------------
select_predictors_for_construct <- function(construct_label, domain_label) {
  
  mapping_filtered_local <- mapping_filtered %>%
    filter(
      !is.na(mlftau_construct),
      map_lgl(str_split(mlftau_construct, ";"), ~ construct_label %in% str_trim(.x))
    ) %>%
    filter(
      !is.na(mlftau_domain),
      map_lgl(str_split(mlftau_domain, ";"), ~ domain_label %in% str_trim(.x))
    )
  
  mapping_filtered_local %>%
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
    slice_sample(n = 5000, weight_by = !!sym(weight_var), replace = TRUE)  # Sampling with replacement
  
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
# --- MAIN LOOP: LOOP OVER CONSTRUCTS ---
# -----------------------------------------------
constructs <- mapping_filtered %>%
  filter(!is.na(mlftau_construct), !mlftau_construct %in% c("", "Not Applicable")) %>%
  pull(mlftau_construct) %>%
  str_split(";") %>% unlist() %>% str_trim() %>%
  discard(~ .x %in% c("", "Not Applicable")) %>%
  unique()

# -----------------------------------------------
# --- MLFTAU-ONLY DOMAIN LOOKUP FOR EACH CONSTRUCT ---
# -----------------------------------------------
get_domains_for_construct <- function(construct_label) {
  mapping_filtered %>%
    filter(
      !is.na(mlftau_construct),
      map_lgl(str_split(mlftau_construct, ";"), ~ construct_label %in% str_trim(.x))
    ) %>%
    pull(mlftau_domain) %>%
    str_split(";") %>% flatten_chr() %>% str_trim() %>%
    discard(~ .x %in% c("", "Not Applicable")) %>%
    unique()
}

for (construct_label in constructs) {
  construct_folder <- construct_label %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("_$", "")
  
  # 🔎 Get domains using custom fallback logic
  associated_domains <- get_domains_for_construct(construct_label)
  associated_domains <- associated_domains[associated_domains %in% c(
    "Individual", "Group/Class", "Organisation/School", "Organisation/Family", "Organisation/Society", "Output"
  )]
  cat("🔍 Domains associated with construct", construct_label, ":", paste(associated_domains, collapse = ", "), "\n")
  
  for (parent_domain in associated_domains) {
    
    # Make domain safe for folder names, filenames, and IDs (handles "/" etc.)
    parent_domain_safe <- parent_domain %>%
      str_to_lower() %>%
      str_replace_all("[^a-z0-9]+", "_") %>%
      str_replace_all("_$", "")
    
    cat("\n🎯 Running Random Forests for construct:", construct_label, 
        "under domain:", parent_domain, "\n")
    
    predictors_all <- select_predictors_for_construct(construct_label, parent_domain)
    
    cat("📦 Number of predictors included after exclusions:", length(predictors_all), "\n")
    
    if (length(predictors_all) == 0) {
      cat("⚠️ No predictors found for construct:", construct_label, "- skipping.\n")
      next
    }
    
    # Create folders
    construct_output_base <- file.path(output_folder_base, parent_domain_safe, construct_folder)
    long_form_dir <- file.path(construct_output_base, "long_form_importance_tables")
    variable_table_dir <- file.path(construct_output_base, "variable_importance_tables")
    plot_dir <- file.path(construct_output_base, "plots")
    
    dir_create(long_form_dir)
    dir_create(variable_table_dir)
    dir_create(plot_dir)
    
    
    # Initialize container to store normalized tables per outcome
    normalized_results_all <- list()
    
    for (outcome in outcomes) {
      cat("\n➡️ Running Random Forests for outcome:", outcome, "\n")
      
      pvs <- paste0("PV", 1:10, outcome)
      
      # --- DEFINE OUTPUT PATHS ---
      filename_base <- paste0(
        tolower(outcome), "_", framework_label, "_", 
        parent_domain_safe, "_", construct_folder
      )
      
      raw_output_file <- file.path(long_form_dir, paste0("rf_raw_importance_", filename_base, ".csv"))
      output_file     <- file.path(variable_table_dir, paste0("rf_variable_importance_", filename_base, ".csv"))
      plot_file       <- file.path(plot_dir, paste0("rf_top20_", filename_base, ".png"))
      
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
      model_id <- paste0(
        "rf_", framework_label, "_",
        parent_domain_safe, "_",
        construct_folder
      )
      
      tracker_model_id <- paste0(model_id, "_", str_to_lower(outcome))
      tracker <- if (file.exists(tracker_path)) {
        read_csv(tracker_path, show_col_types = FALSE) %>%
          mutate(across(c(n_predictors, sample_size_per_pv, n_students_used), as.integer))
      } else tibble()
      
      tracker_row <- tibble(
        model_id = tracker_model_id,
        target = paste0("PV1-10", outcome),
        predictors_used = "All valid in-class questionnaire and derived variables (excluding COVID variables)",
        subset_rule = "Included predictors labeled In School or In and Out",
        domain_source_column = domain_mapping_column,
        description = paste(description_prefix, construct_label, "~", outcome, "PVs"),
        date_run = format(Sys.Date(), "%d/%m/%y"),
        n_predictors = as.integer(length(predictors_all)),
        sample_size_per_pv = as.integer(5000),
        n_students_used = as.integer(mean(n_students_used_vector)),
        covid_excluded = "Yes",
        output_file = output_file,
        raw_result_file = raw_output_file,
        plot_file = plot_file,
        framework = framework, 
        domain_or_construct = paste0("construct:", construct_label, " | domain:", parent_domain)
      )
      
      tracker <- tracker %>% filter(model_id != tracker_model_id)
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
    
    # -----------------------------------------------
    # --- SAVE COMBINED NORMALIZED RESULTS ---
    # -----------------------------------------------
    
    # 1) Long-form normalized table (variable × outcome)
    long_form_file <- file.path(
      construct_output_base,
      paste0("rf_combined_normalized_long_", construct_folder, ".csv")
    )
    
    write_csv(combined_normalized, long_form_file)
    cat("✅ Long-form normalized importance table saved to:", long_form_file, "\n")
    
    
    # 2) Aggregated Top-20 normalized table (collapsed across outcomes)
    top20_file <- file.path(
      construct_output_base,
      paste0("rf_top20_combined_normalized_", construct_folder, ".csv")
    )
    
    write_csv(top20_combined, top20_file)
    cat("✅ Aggregated Top-20 normalized table saved to:", top20_file, "\n")
    
    # -----------------------------------------------
    # --- PLOT COMBINED TOP 20 ---
    # -----------------------------------------------
    norm_combined_plot <- file.path(construct_output_base, 
                                    paste0("rf_top20_combined_normalized_", construct_folder, ".png"))
    
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
  }
}

cat("🏁 All constructs processed. Random Forest modeling complete.\n")

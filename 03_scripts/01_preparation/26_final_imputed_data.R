# ----------------------------------------------
# PISA 2022 Final Imputation and Audit
# ----------------------------------------------
# File: 03_scripts/01_preparation/26_final_imputed_data.R
# Purpose:
# This script applies the final implemented imputation and audit workflow used
# to produce the analysis-ready dataset for the published Thailand random forest
# study based on PISA 2022.
#
# Main tasks:
# 1. Load the prior-step cleaned dataset.
# 2. Standardise missingness by converting random_skip values to NA.
# 3. Apply selected valid_skip imputations where substantively justified.
# 4. Impute remaining missing values using median or mode, depending on variable type.
# 5. Convert selected quality-control flag variables to factors.
# 6. Save the final imputed dataset.
# 7. Audit the final dataset and save the audit summary.
#
# Public repository note:
# This script is shared to support transparency regarding the published analysis.
# It reflects the implemented workflow used in the study, but some intermediate
# inputs and broader private project resources are not publicly released.

library(dplyr)
library(readr)
library(stringr)
library(forcats)
library(tibble)

# 1. Load prior-step cleaned dataset
# Note: update these paths to match your own local environment and available inputs.
input_path <- "PATH_TO_LOCAL_INPUT/pisa2022_cleaned_16_final_cleaned_data.rds"
df <- readRDS(input_path)

# 2. Helper Functions
save_cleaned <- function(df, csv_path) {
  write_csv(df, csv_path)
  rds_path <- str_replace(csv_path, ".csv$", ".rds")
  saveRDS(df, rds_path)
  cat("\U00002705 Saved as CSV and RDS.\n")
}

# 3. Step 1: Convert random_skip to NA
cat("\U0001F4CB Converting 'random_skip' to NA...\n")
df[df == "random_skip"] <- NA

# 4. Step 2: Handle Special Valid Skip Imputations

# 4.1 "None" for devices and books
cat("\U0001F4D5 Imputing 'valid_skip' with 'None' for devices and books...\n")
device_none_vars <- c(
  "home_televisions", "home_desktop_computers", "home_laptops",
  "home_tablets", "home_ebook_readers", "home_smartphones"
)

book_none_vars <- c(
  "home_books_religious", "home_books_classics", "home_books_contemporary",
  "home_books_science", "home_books_art_music", "home_books_technical",
  "home_books_dictionaries", "home_books_school"
)

for (var in c(device_none_vars, book_none_vars)) {
  if (var %in% names(df)) {
    df[[var]] <- fct_expand(df[[var]], "None")
    df[[var]][df[[var]] == "valid_skip"] <- "None"
    df[[var]] <- droplevels(as.factor(df[[var]]))
  }
}

# 4.2 "0" for student_arrival_age
cat("\U0001F4C8 Imputing 'valid_skip' with '0' for student_arrival_age...\n")
if ("student_arrival_age" %in% names(df)) {
  df$student_arrival_age <- fct_expand(df$student_arrival_age, "0")
  df$student_arrival_age[df$student_arrival_age == "valid_skip"] <- "0"
  df$student_arrival_age <- droplevels(as.factor(df$student_arrival_age))
}

# 4.3 "No" for reasons for missing school
cat("\U0001F4DD Imputing 'valid_skip' with 'No' for missed school reasons...\n")
reason_no_vars <- c(
  "reason_school_missed_bored", "reason_school_missed_suspended",
  "reason_school_missed_pregnant", "reason_school_missed_transport",
  "reason_school_missed_family_care", "reason_school_missed_family_work",
  "reason_school_missed_earning", "reason_school_missed_sick",
  "reason_school_missed_safety", "reason_school_missed_fees",
  "reason_school_missed_disaster"
)

for (var in reason_no_vars) {
  if (var %in% names(df)) {
    df[[var]][df[[var]] == "valid_skip"] <- "No"
    df[[var]] <- droplevels(as.factor(df[[var]]))
  }
}

# 4.4 "Career guidance is not offered" for student_situation_description
cat("\U0001F4CB Imputing 'valid_skip' for student_situation_description...\n")
if ("student_situation_description" %in% names(df)) {
  df$student_situation_description <- fct_expand(df$student_situation_description, "Career guidance is not offered")
  df$student_situation_description[df$student_situation_description == "valid_skip"] <- "Career guidance is not offered"
  df$student_situation_description <- droplevels(as.factor(df$student_situation_description))
}

# 4.5 "No" for testing and math differentiation
cat("\U0001F4C9 Imputing 'valid_skip' with 'No' for testing/differentiation...\n")
no_vars <- names(df)[which(names(df) == "standardized_tests_guide_learning") : 
                       which(names(df) == "math_lessons_remedial")]

for (var in no_vars) {
  if (var %in% names(df)) {
    df[[var]][df[[var]] == "valid_skip"] <- "No"
    df[[var]] <- droplevels(as.factor(df[[var]]))
  }
}

# 4.6 COVID Variables: Student and School
cat("\U0001F9A0 Handling COVID-related variables (student and school)...\n")

covid_student_vars <- names(df)[which(names(df) == "covid_materials_sent") :
                                  which(names(df) == "confidence_prepared_future_closures")]

covid_school_vars <- names(df)[which(names(df) == "covid_remote_classes") :
                                 which(names(df) == "prepare_plan_transition_remote")]

all_covid_vars <- c(covid_student_vars, covid_school_vars)

for (var in all_covid_vars) {
  if (var %in% names(df)) {
    df[[var]][df[[var]] == "valid_skip"] <- NA
  }
}

# 5. Step 3: Final Imputation for Remaining NA Values
cat("\U0001F527 Imputing remaining NA values...\n")

exclude_vars <- names(df)[str_detect(names(df), "^(W_|PV|UNIT|WVARSTRR)")]
exclude_vars <- c(exclude_vars, "country_id", "school_id", "student_id", 
                  "assessment_cycle", "sampling_stratum", "subnational_region", "administration_mode")

for (col in setdiff(names(df), exclude_vars)) {
  if (any(is.na(df[[col]]))) {
    if (is.numeric(df[[col]])) {
      df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)
    } else if (is.factor(df[[col]])) {
      mode_val <- names(sort(table(df[[col]]), decreasing = TRUE))[1]
      df[[col]][is.na(df[[col]])] <- mode_val
      df[[col]] <- droplevels(df[[col]])
    } else if (is.character(df[[col]])) {
      mode_val <- names(sort(table(df[[col]]), decreasing = TRUE))[1]
      df[[col]][is.na(df[[col]])] <- mode_val
    } else if (is.ordered(df[[col]])) {
      mode_val <- names(sort(table(df[[col]]), decreasing = TRUE))[1]
      df[[col]][is.na(df[[col]])] <- mode_val
      df[[col]] <- droplevels(df[[col]])
    }
  }
}

# 6. Step 4: Convert Flagged/Not flagged variables to factors
cat("\U0001F3F3 Converting Flagged/Not flagged variables to factors...\n")

flag_vars <- c(
  "perception_straight_line_flag",
  "feeling_straight_line_flag",
  "persistence_straight_line_flag",
  "covid_inconsistency_flag"
)

for (var in flag_vars) {
  if (var %in% names(df)) {
    if (!is.factor(df[[var]])) {
      df[[var]] <- factor(df[[var]], levels = c("Flagged", "Not flagged"))
    }
  }
}

# Public repository note:
# The final imputed dataset and audit outputs referenced here are part of the
# implemented workflow, but are not included in this repository as public data products.
# 7. Step 5: Save Cleaned Dataset
output_path <- "PATH_TO_LOCAL_OUTPUT/pisa2022_cleaned_17_final_imputed_data.csv"
save_cleaned(df, output_path)

cat("\U00002705 Cleaned_17 Final Dataset Ready!\n")

# ----------------------------------------------
# PISA 2022 Cleaned_17 Final Audit
# ----------------------------------------------

# 1. Load final imputed dataset
input_path <- "PATH_TO_LOCAL_OUTPUT/pisa2022_cleaned_17_final_imputed_data.rds"
df <- readRDS(input_path)

# 2. Function to audit dataset
audit_summary <- tibble(
  variable = names(df),
  class = sapply(df, function(x) class(x)[1]),
  n_missing = sapply(df, function(x) sum(is.na(x))),
  n_valid_skip = sapply(df, function(x) sum(x == "valid_skip", na.rm = TRUE)),
  n_random_skip = sapply(df, function(x) sum(x == "random_skip", na.rm = TRUE)),
  n_levels = sapply(df, function(x) if (is.factor(x) || is.ordered(x)) nlevels(x) else NA),
  ordered = sapply(df, function(x) is.ordered(x))
)

# 3. Save Audit Output
output_path <- "PATH_TO_LOCAL_OUTPUT/pisa2022_cleaned_17_audit.csv"
write_csv(audit_summary, output_path)

# 4. Print Confirmation
cat("✅ Cleaned_17 Audit Saved:", output_path, "\n")

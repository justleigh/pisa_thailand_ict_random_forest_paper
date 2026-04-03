# --------------------------------------
# PISA 2022 ICT Derived Variables Cleaning
# --------------------------------------
# File: 03_scripts/01_preparation/21_ict_dv_cleaning.R
# Purpose:
# This script applies the implemented cleaning and transformation workflow used
# for PISA 2022 ICT derived variables in the published Thailand random forest
# analysis.
#
# Main tasks:
# 1. Load the prior-step cleaned dataset.
# 2. Identify the ICT derived-variable block.
# 3. Apply variable-specific missing-value handling and transformation rules.
# 4. Produce debugging summaries before and after cleaning.
# 5. Save the cleaned ICT derived-variable dataset and debugging output.
#
# Public repository note:
# This script is shared to support transparency regarding the published analysis.
# It reflects the implemented workflow used in the study, but some intermediate
# inputs and broader private project resources are not publicly released.

# Load necessary libraries
library(dplyr)
library(readr)
library(stringr)

# CSV and RDS helper Function
save_cleaned <- function(df, csv_path) {
  write_csv(df, csv_path)
  rds_path <- str_replace(csv_path, ".csv$", ".rds")
  saveRDS(df, rds_path)
  cat("✅ Data saved as CSV:", csv_path, "\n")
  cat("✅ Data saved as RDS:", rds_path, "\n")
}

# Define local file paths
# Note: update these paths to match your own local environment and available inputs.
input_path <- "PATH_TO_LOCAL_INPUT/pisa2022_cleaned_11_student_dv_cleaning.rds"
output_path <- "PATH_TO_LOCAL_OUTPUT/pisa2022_cleaned_12_ict_dv_cleaning.csv"
debug_path <- "PATH_TO_LOCAL_OUTPUT/pisa2022_cleaned_12_debugging_analysis.csv"

# Load dataset
df <- readRDS(input_path)

# ----------------------------
# Automatically Identify ICT Derived Variables
# ----------------------------

# The implemented workflow identifies the ICT derived-variable block using
# the first and last renamed variables in the prepared dataset.
# Identify first and last ICT derived variables
first_ictdv_var <- "ict_at_school"  
last_ictdv_var <- "ict_self_efficacy"  

# Get all column names between the first and last student derived variable
if (first_ictdv_var %in% names(df) & last_ictdv_var %in% names(df)) {
  ictdv_vars <- names(df)[which(names(df) == first_ictdv_var):which(names(df) == last_ictdv_var)]
} else {
  ictdv_vars <- intersect(names(df), c(first_ictdv_var, last_ictdv_var)) # Ensure it doesn't break if one variable is missing
}

# Ensure that only variables that exist in df are processed
ictdv_vars <- intersect(ictdv_vars, names(df))

# ----------------------------
# Debugging Analysis (Before Cleaning)
# ----------------------------
debug_before <- data.frame(
  Dataset = "Before Cleaning",
  `95` = sum(df[, ictdv_vars] == 95, na.rm = TRUE),  # Count raw 95 values
  `97` = sum(df[, ictdv_vars] == 97, na.rm = TRUE),  # Count raw 97 values
  `98` = sum(df[, ictdv_vars] == 98, na.rm = TRUE),  # Count raw 98 values
  `99` = sum(df[, ictdv_vars] == 99, na.rm = TRUE),  # Count raw 99 values
  `999` = sum(df[, ictdv_vars] == 999, na.rm = TRUE),  # Count raw 999 values
  Total_NA = sum(is.na(df[, ictdv_vars]))  # Count total NA values
)

# -----------------------------------------------------
# Manual Transformations for ICT Derived Variables
# -----------------------------------------------------

# Handle missing values for 'ict_at_school'
df$ict_at_school <- ifelse(df$ict_at_school == 99, NA, df$ict_at_school)

# Convert 'ict_availability_school' into a Numeric Variable and Handle Missing Values
df$ict_availability_school <- ifelse(df$ict_availability_school == 99, NA, df$ict_availability_school)

# Handle missing values for 'ict_at_home'
df$ict_at_home <- ifelse(df$ict_at_home == 99, NA, df$ict_at_home)

# Convert 'ict_availability_home' into a Numeric Variable and Handle Missing Values
df$ict_availability_home <- ifelse(df$ict_availability_home == 99, NA, df$ict_availability_home)

# Handle missing values for 'ict_access_quality'
df$ict_access_quality <- ifelse(df$ict_access_quality == 99, NA, df$ict_access_quality)

# Handle missing values for 'ict_use_subject_lessons'
df$ict_use_subject_lessons <- ifelse(df$ict_use_subject_lessons == 99, NA, df$ict_use_subject_lessons)

# Handle missing values for 'ict_use_weekday'
df$ict_use_weekday <- ifelse(df$ict_use_weekday == 99, NA, df$ict_use_weekday)

# Handle missing values for 'ict_use_weekend'
df$ict_use_weekend <- ifelse(df$ict_use_weekend == 99, NA, df$ict_use_weekend)

# Handle missing values for 'ict_regulated_use'
df$ict_regulated_use <- ifelse(df$ict_regulated_use == 99, NA, df$ict_regulated_use)

# Handle missing values for 'ict_online_practices'
df$ict_online_practices <- ifelse(df$ict_online_practices == 99, NA, df$ict_online_practices)

# Convert 'ict_distress_online' into a numeric variable and handle missing values
df$ict_distress_online <- ifelse(df$ict_distress_online == 99, NA, df$ict_distress_online)

# Handle missing values for 'ict_self_efficacy'
df$ict_self_efficacy <- ifelse(df$ict_self_efficacy == 99, NA, df$ict_self_efficacy)

# Remove 'student_bmi' constant value column
df <- df %>% select(-student_bmi)

# ----------------------------
# Debugging Analysis (After Cleaning)
# ----------------------------
debug_after <- data.frame(
  Dataset = "After Cleaning",
  `95` = sum(df[, ictdv_vars] == 95, na.rm = TRUE),  # Should be 0 if cleaned correctly
  `97` = sum(df[, ictdv_vars] == 97, na.rm = TRUE),  # Should be 0 if cleaned correctly
  `98` = sum(df[, ictdv_vars] == 98, na.rm = TRUE),  # Should be 0 after transformation
  `99` = sum(df[, ictdv_vars] == 99, na.rm = TRUE),  # Should be 0 after transformation
  `999` = sum(df[, ictdv_vars] == 999, na.rm = TRUE),  # Should be 0 after transformation
  Total_NA = sum(is.na(df[, ictdv_vars]))  # Count total NA values
)

# ----------------------------
# Combine Debugging Results
# ----------------------------
debug_summary <- bind_rows(debug_before, debug_after)

# Public repository note:
# The output files referenced here are part of the implemented workflow, but are
# not included in this repository as public intermediate data products.
# ----------------------------
# Save Cleaned Dataset and Debugging Summary
# ----------------------------

# Save cleaned dataset (both CSV and RDS)
save_cleaned(df, output_path)

# Save debugging summary (CSV only)
write_csv(debug_summary, debug_path)

# Completion Message
cat("✅ ICT Derived Variable Cleaning Complete!\n")
cat("✅ Cleaned data saved to:", output_path, "\n")
cat("✅ Debugging analysis saved to:", debug_path, "\n")


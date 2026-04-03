# --------------------------------------
# PISA 2022 School Derived Variables Cleaning
# --------------------------------------
# File: 03_scripts/01_preparation/23_school_dv_cleaning.R
# Purpose:
# This script applies the implemented cleaning and transformation workflow used
# for PISA 2022 school derived variables in the published Thailand random
# forest analysis.
#
# Main tasks:
# 1. Load the prior-step cleaned dataset.
# 2. Identify the school derived-variable block.
# 3. Apply variable-specific missing-value handling, factor conversion, and numeric transformations.
# 4. Produce debugging summaries before and after cleaning.
# 5. Save the cleaned school derived-variable dataset and debugging output.
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
input_path <- "PATH_TO_LOCAL_INPUT/pisa2022_cleaned_13_school_questionnaire_cleaning.rds"
output_path <- "PATH_TO_LOCAL_OUTPUT/pisa2022_cleaned_14_school_dv_cleaning.csv"
debug_path <- "PATH_TO_LOCAL_OUTPUT/pisa2022_cleaned_14_debugging_analysis.csv"

# Load dataset
df <- readRDS(input_path)

# ----------------------------
# Automatically Identify School Derived Variables
# ----------------------------

# The implemented workflow identifies the school derived-variable block using
# the first and last renamed variables in the prepared dataset.
# Identify first and last School Derived Variables
first_scdv_var <- "school_type_derived"  
last_scdv_var <- "digital_learning_preparedness"  

# Get all column names between the first and last school derived variable
if (first_scdv_var %in% names(df) & last_scdv_var %in% names(df)) {
  scdv_vars <- names(df)[which(names(df) == first_scdv_var):which(names(df) == last_scdv_var)]
} else {
  scdv_vars <- intersect(names(df), c(first_scdv_var, last_scdv_var)) # Ensure it doesn't break if one variable is missing
}

# Ensure that only variables that exist in df are processed
scdv_vars <- intersect(scdv_vars, names(df))

# ----------------------------
# Debugging Analysis (Before Cleaning)
# ----------------------------
debug_before <- data.frame(
  Dataset = "Before Cleaning",
  `99` = sum(df[, scdv_vars] == 99, na.rm = TRUE),  # Count raw 99 values
  `999` = sum(df[, scdv_vars] == 999, na.rm = TRUE),  # Count raw 999 values
  Total_NA = sum(is.na(df[, scdv_vars]))  # Count total NA values
)

# -----------------------------------------------------
# Manual Transformations for School Derived Variables
# -----------------------------------------------------

# Convert 'school_type_derived' to a factor with levels 'public' and 'private'
df$school_type_derived <- factor(
    df$school_type_derived,
    levels = c("public", "private")
)

# Convert 'school_type_public_private' to a factor with specified levels
df$school_type_public_private <- factor(
    df$school_type_public_private,
    levels = 1:3,
    labels = c("Public", "Private Government-dependent", "Private Independent")
)

# Convert 'school_size' to numeric and handle missing values
df$school_size <- as.numeric(ifelse(df$school_size == 99999999, NA, df$school_size))

# Convert 'total_teachers' to numeric to ensure correct data type
df$total_teachers <- as.numeric(df$total_teachers)

# Convert proportional teacher variables to numeric to ensure correct data type
df$proportion_fully_cert_teachers <- as.numeric(df$proportion_fully_cert_teachers)

# Convert 'proportion_bachelor_teachers' to numeric and handle missing values
df$proportion_bachelor_teachers <- as.numeric(ifelse(df$proportion_bachelor_teachers == 99, NA, df$proportion_bachelor_teachers))

# Convert 'proportion_master_teachers' to numeric to ensure correct data type
df$proportion_master_teachers <- as.numeric(df$proportion_master_teachers)

# Convert 'proportion_doctorate_teachers' to numeric to ensure correct data type
df$proportion_doctorate_teachers <- as.numeric(df$proportion_doctorate_teachers)

# Convert 'student_teacher_ratio' to numeric and handle missing values
df$student_teacher_ratio <- as.numeric(ifelse(df$student_teacher_ratio == 999, NA, df$student_teacher_ratio))

# Convert 'total_math_teachers' to numeric to ensure correct data type
df$total_math_teachers <- as.numeric(df$total_math_teachers)

# Convert 'proportion_math_teachers' to numeric and handle missing values
df$proportion_math_teachers <- as.numeric(ifelse(df$proportion_math_teachers == 99, NA, df$proportion_math_teachers))

# Convert 'student_math_teacher_ratio' to numeric
df$student_math_teacher_ratio <- as.numeric(df$student_math_teacher_ratio)

# Convert 'total_non_teaching_staff' to numeric
df$total_non_teaching_staff <- as.numeric(df$total_non_teaching_staff)

# Convert 'proportion_pedagogical_support' to numeric
df$proportion_pedagogical_support <- as.numeric(df$proportion_pedagogical_support)

# Convert 'proportion_admin_staff' to numeric
df$proportion_admin_staff <- as.numeric(df$proportion_admin_staff)

# Convert 'school_selectivity' to an ordered factor with PISA codebook labels
df$school_selectivity <- factor(
  df$school_selectivity,
  levels = c(1, 2, 3),
  labels = c("Never Considered", "Sometimes Considered", "Always Considered"),
  ordered = TRUE
)

# Convert 99 values to NA for missing school autonomy index values
df$school_autonomy <- as.numeric(ifelse(df$school_autonomy == 99, NA, df$school_autonomy))

# Ensure teacher_participation is treated as numeric
df$teacher_participation <- as.numeric(df$teacher_participation)

# Ensure school_resp_curriculum is treated as numeric
df$school_resp_curriculum <- as.numeric(df$school_resp_curriculum)

# Ensure school_resp_resources is treated as numeric
df$school_resp_resources <- as.numeric(df$school_resp_resources)

# Ensure educational_leadership is treated as numeric
df$educational_leadership <- as.numeric(df$educational_leadership)

# Ensure instructional_leadership remains numeric
df$instructional_leadership <- as.numeric(df$instructional_leadership)

# Ensure parent_involvement_encouragement remains numeric
df$parent_involvement_encouragement <- as.numeric(df$parent_involvement_encouragement)

# Convert 99 values to NA and ensure computer_availability is numeric
df$computer_availability <- as.numeric(ifelse(df$computer_availability == 99, NA, df$computer_availability))

# Convert 99 values to NA for missing internet-connected computer index values
df$internet_connected_computers <- as.numeric(ifelse(df$internet_connected_computers == 99, NA, df$internet_connected_computers))

# Convert 99 values to NA for missing tablet availability index values
df$tablet_availability <- as.numeric(ifelse(df$tablet_availability == 99, NA, df$tablet_availability))

# Ensure teacher feedback index values are numeric
df$teacher_feedback <- as.numeric(df$teacher_feedback)

# Ensure math teacher training index values are numeric
df$math_teacher_training <- as.numeric(df$math_teacher_training)

# Ensure negative school climate index values are numeric
df$negative_school_climate <- as.numeric(df$negative_school_climate)

# Ensure staff shortage index values are numeric
df$staff_shortage <- as.numeric(df$staff_shortage)

# Ensure material shortage index values are numeric
df$material_shortage <- as.numeric(df$material_shortage)

# Ensure student school climate factors index values are numeric
df$student_school_climate_factors <- as.numeric(df$student_school_climate_factors)

# Ensure teacher school climate_factors index values are numeric
df$teacher_school_climate_factors <- as.numeric(df$teacher_school_climate_factors)

# Ensure math class size is numeric for further analysis
df$math_class_size_dv <- as.numeric(df$math_class_size_dv)

# Ensure test language class size is numeric for further analysis
df$test_language_class_size <- as.numeric(df$test_language_class_size)

# Convert 99 values to NA and ensure standardized tests usage is numeric
df$standardized_tests_usage <- as.numeric(ifelse(df$standardized_tests_usage == 99, NA, df$standardized_tests_usage))

# Convert 99 values to NA and ensure teacher developed tests is numeric
df$teacher_developed_tests <- as.numeric(ifelse(df$teacher_developed_tests == 99, NA, df$teacher_developed_tests))

# Convert MACTIV to an ordered factor using PISA codebook labels
df$math_extra_activities <- factor(df$math_extra_activities, 
    levels = 0:5, 
    labels = c("No mathematics-related extra-curricular activities offered", 
               "1 mathematics-related extra-curricular activity offered", 
               "2 mathematics-related extra-curricular activities offered", 
               "3 mathematics-related extra-curricular activities offered", 
               "4 mathematics-related extra-curricular activities offered", 
               "5 mathematics-related extra-curricular activities offered"), 
    ordered = TRUE)

# Convert 9 values to NA and factorize math_extension_courses using PISA labels
df$math_extension_courses <- factor(ifelse(df$math_extension_courses == 9, NA, df$math_extension_courses), 
    levels = 0:3, 
    labels = c("None", 
              "Mathematics extension courses offered without differentiation depending on the prior achievement level of the students", 
              "Mathematics extension courses offered for enrichment or remediation", 
              "Mathematics extension courses offered for enrichment and remediation"), 
    ordered = TRUE)

# Convert math_ability_grouping to an ordered factor using PISA labels
df$math_ability_grouping <- factor(df$math_ability_grouping, 
    levels = 1:3, 
    labels = c("No ability grouping for any classes", 
               "At least one form of ability grouping in some classes", 
               "At least one form of ability grouping in all classes"), 
    ordered = TRUE)

# Convert 9 values to NA and factorize edu_authority_support as an ordered factor
df$edu_authority_support <- factor(ifelse(df$edu_authority_support == 9, NA, df$edu_authority_support), 
    levels = 0:2, 
    labels = c("No support from education authorities", 
               "Some support from education authorities", 
               "A lot of support from education authorities"), 
    ordered = TRUE)

# Convert 9 values to NA and factorize other_support as an ordered factor
df$other_support <- factor(ifelse(df$other_support == 9, NA, df$other_support), 
    levels = 0:2, 
    labels = c("No support from other resources", 
               "Some support from other resources", 
               "A lot of support from other resources"), 
    ordered = TRUE)

# Convert 99 values to NA and ensure remote_instruction_problems is numeric
df$remote_instruction_problems <- as.numeric(ifelse(df$remote_instruction_problems == 99, NA, df$remote_instruction_problems))

# Convert 99 values to NA and ensure remote_instruction_prep_before is numeric
df$remote_instruction_prep_before <- as.numeric(ifelse(df$remote_instruction_prep_before == 99, NA, df$remote_instruction_prep_before))

# Convert 99 values to NA and ensure remote_instruction_prep_during is numeric
df$remote_instruction_prep_during <- as.numeric(ifelse(df$remote_instruction_prep_during == 99, NA, df$remote_instruction_prep_during))

# Ensure digital_learning_preparedness is numeric
df$digital_learning_preparedness <- as.numeric(df$digital_learning_preparedness)


# ----------------------------
# Debugging Analysis (After Cleaning)
# ----------------------------
debug_after <- data.frame(
  Dataset = "After Cleaning",
  `99` = sum(df[, scdv_vars] == 99, na.rm = TRUE),  # Should be 0 after transformation
  `999` = sum(df[, scdv_vars] == 999, na.rm = TRUE),  # Should be 0 after transformation
  Total_NA = sum(is.na(df[, scdv_vars]))  # Count total NA values
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
cat("✅ School Derived Variables Cleaning Complete!\n")
cat("Cleaned data saved to:", output_path, "\n")
cat("Debugging analysis saved to:", debug_path, "\n")



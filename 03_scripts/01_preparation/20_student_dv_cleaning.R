# --------------------------------------
# PISA 2022 Student Derived Variables Cleaning
# --------------------------------------
# File: 03_scripts/01_preparation/20_student_dv_cleaning.R
# Purpose:
# This script applies the implemented cleaning and transformation workflow used
# for PISA 2022 student derived variables in the published Thailand random
# forest analysis.
#
# Main tasks:
# 1. Load the prior-step cleaned dataset.
# 2. Identify the student derived-variable block.
# 3. Apply variable-specific recoding, factor conversion, and missing-value handling.
# 4. Produce debugging summaries before and after cleaning.
# 5. Save the cleaned student derived-variable dataset and debugging output.
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
input_path <- "PATH_TO_LOCAL_INPUT/pisa2022_cleaned_10_ict_questionnaire_cleaning.rds"
output_path <- "PATH_TO_LOCAL_OUTPUT/pisa2022_cleaned_11_student_dv_cleaning.csv"
debug_path <- "PATH_TO_LOCAL_OUTPUT/pisa2022_cleaned_11_debugging_analysis.csv"

# Load dataset
df <- readRDS(input_path)

# ----------------------------
# Automatically Identify Student Derived Variables
# ----------------------------

# The implemented workflow identifies the student derived-variable block using
# the first and last renamed variables in the prepared dataset.
# Identify first and last Student Derived Variables
first_sdv_var <- "test_effort_actual"  
last_sdv_var <- "parent_highest_occup"  

# Get all column names between the first and last student derived variable
if (first_sdv_var %in% names(df) & last_sdv_var %in% names(df)) {
  sdv_vars <- names(df)[which(names(df) == first_sdv_var):which(names(df) == last_sdv_var)]
} else {
  sdv_vars <- intersect(names(df), c(first_sdv_var, last_sdv_var)) # Ensure it doesn't break if one variable is missing
}

# Ensure that only variables that exist in df are processed
sdv_vars <- intersect(sdv_vars, names(df))

# ----------------------------
# Debugging Analysis (Before Cleaning)
# ----------------------------
debug_before <- data.frame(
  Dataset = "Before Cleaning",
  `95` = sum(df[, sdv_vars] == 95, na.rm = TRUE),  # Count raw 95 values
  `97` = sum(df[, sdv_vars] == 97, na.rm = TRUE),  # Count raw 97 values
  `98` = sum(df[, sdv_vars] == 98, na.rm = TRUE),  # Count raw 98 values
  `99` = sum(df[, sdv_vars] == 99, na.rm = TRUE),  # Count raw 99 values
  `999` = sum(df[, sdv_vars] == 999, na.rm = TRUE),  # Count raw 999 values
  Total_NA = sum(is.na(df[, sdv_vars]))  # Count total NA values
)

# -----------------------------------------------------
# Manual Transformations for Student Derived Variables
# -----------------------------------------------------

# Transform 'test_effort_actual': Effort scale (1 to 10)
df$test_effort_actual <- factor(ifelse(df$test_effort_actual == 99, NA, 
                                       df$test_effort_actual), 
                                levels = 1:10, 
                                ordered = TRUE)

# Transform 'test_effort_hypothetical': Effort scale (1 to 10)
df$test_effort_hypothetical <- factor(ifelse(df$test_effort_hypothetical == 99, NA, 
                                       df$test_effort_hypothetical), 
                                levels = 1:10, 
                                ordered = TRUE)

# Transform 'national_program_code': Educational attainment levels
df$national_program_code <- factor(ifelse(df$national_program_code == 7640099, NA, 
                                    df$national_program_code), 
                             levels = c(7640001, 7640002, 7640003), 
                             labels = c("Lower Secondary Level", 
                                        "Upper Secondary Level", 
                                        "Vocational Certificate level (Cert. of VOC.)"),
                             ordered = TRUE)

# Transform 'student_age': Retain as numeric
df$student_age <- ifelse(df$student_age == 99, NA, df$student_age)

# Transform 'student_grade' (GRADE) into an ordered factor with explicit labels
df$student_grade <- factor(ifelse(df$student_grade == 99, NA, df$student_grade),
                           levels = c(-3, -2, -1, 0, 1, 2),
                           labels = c("Grade 7", "Grade 8", "Grade 9", "Grade 10", "Grade 11", "Grade 12"),
                           ordered = TRUE)

# Transform 'education_level_program' (ISCEDP) into an ordered factor with explicit labels
df$education_level_program <- factor(ifelse(df$education_level_program == 99, NA, df$education_level_program),
                                     levels = c(244, 254, 344, 354),
                                     labels = c("Lower Secondary General", 
                                                "Lower Secondary Vocational", 
                                                "Upper Secondary General", 
                                                "Upper Secondary Vocational"),
                                     ordered = TRUE)

# Transform 'immigrant_background' into a categorical factor
df$immigrant_background <- factor(ifelse(df$immigrant_background == 9, NA, df$immigrant_background),
                                  levels = c(1, 2, 3),
                                  labels = c("Native Student", 
                                             "Second-Generation Student", 
                                             "First-Generation Student"))

# Transform 'home_language_dv' into a categorical factor
df$home_language_dv <- factor(ifelse(df$home_language_dv == 999, NA, df$home_language_dv),
                              levels = c(555, 616, 855),
                              labels = c("Central Thai", 
                                         "Regional Thai Dialect", 
                                         "Non-Thai Language"))

# Transform 'grade_repetition' into a categorical factor
df$grade_repetition <- factor(ifelse(df$grade_repetition == 9, NA, df$grade_repetition),
                              levels = c(0, 1),
                              labels = c("Never Repeated", 
                                         "Repeated"))

# Transform 'school_absence_3_months' into a categorical factor
df$school_absence_3_months <- factor(ifelse(df$school_absence_3_months == 9, NA, df$school_absence_3_months),
                                     levels = c(0, 1),
                                     labels = c("Never Absent", 
                                                "Absent 3+ Months"))

# Transform 'school_skipping' into a categorical factor
df$school_skipping <- factor(ifelse(df$school_skipping == 9, NA, df$school_skipping),
                             levels = c(0, 1),
                             labels = c("Never Skipped", 
                                        "Skipped at least once"))

# Transform 'tardiness_strict' into an ordered factor
df$tardiness_strict <- factor(ifelse(df$tardiness_strict == 9, NA, df$tardiness_strict),
                              levels = c(0, 1, 2),
                              labels = c("Never Late", 
                                         "Occasionally Late", 
                                         "Frequently Late"),
                              ordered = TRUE)

# Transform 'study_homework' into an ordered factor
df$study_homework <- factor(ifelse(df$study_homework == 99, NA, df$study_homework),
                            levels = 0:10,
                            labels = c("No studying", 
                                       "1 time per week", 
                                       "2 times per week", 
                                       "3 times per week", 
                                       "4 times per week", 
                                       "5 times per week", 
                                       "6 times per week", 
                                       "7 times per week", 
                                       "8 times per week", 
                                       "9 times per week", 
                                       "10+ times per week"),
                            ordered = TRUE)

# Transform 'work_for_pay' into an ordered factor
df$work_for_pay <- factor(ifelse(df$work_for_pay == 99, NA, df$work_for_pay),
                          levels = 0:10,
                          labels = c("No work for pay", 
                                     "1 time per week", 
                                     "2 times per week", 
                                     "3 times per week", 
                                     "4 times per week", 
                                     "5 times per week", 
                                     "6 times per week", 
                                     "7 times per week", 
                                     "8 times per week", 
                                     "9 times per week", 
                                     "10+ times per week"),
                          ordered = TRUE)

# Transform 'work_household' into an ordered factor
df$work_household <- factor(ifelse(df$work_household == 99, NA, df$work_household),
                            levels = 0:10,
                            labels = c("No work in household", 
                                       "1 time per week", 
                                       "2 times per week", 
                                       "3 times per week", 
                                       "4 times per week", 
                                       "5 times per week", 
                                       "6 times per week", 
                                       "7 times per week", 
                                       "8 times per week", 
                                       "9 times per week", 
                                       "10+ times per week"),
                            ordered = TRUE)

# Transform 'expected_education_level' into an ordered factor
df$expected_education_level <- factor(ifelse(df$expected_education_level == 99, NA, df$expected_education_level),
                            levels = 1:9,
                            labels = c("Less than ISCED 2",
                                       "ISCED 2 (Lower secondary)",
                                       "ISCED 3.3 (Upper secondary, no tertiary access)",
                                       "ISCED 3.4 (Upper secondary, tertiary access)",
                                       "ISCED 4 (Post-secondary non-tertiary)",
                                       "ISCED 5 (Short-cycle tertiary)",
                                       "ISCED 6 (Bachelor’s or equivalent)",
                                       "ISCED 7 (Master’s or equivalent)",
                                       "ISCED 8 (Doctoral or equivalent)"),
                            ordered = TRUE)

# Transform 'future_job_clarity' into a categorical factor
df$future_job_clarity <- factor(ifelse(df$future_job_clarity == 9, NA, df$future_job_clarity),
                                levels = c(0, 1),
                                labels = c("No clear idea of future job",
                                           "Clear idea of future job"))

# Handle missing values in 'student_teacher_relation' (IRT scale)
df$student_teacher_relation <- ifelse(df$student_teacher_relation == 99, NA, df$student_teacher_relation)

# Handle missing values in 'sense_of_belonging' (IRT scale)
df$sense_of_belonging <- ifelse(df$sense_of_belonging == 99, NA, df$sense_of_belonging)

# Handle missing values in 'bullying_experience' (IRT scale)
df$bullying_experience <- ifelse(df$bullying_experience == 99, NA, df$bullying_experience)

# Handle missing values in 'feeling_safe' (IRT scale)
df$feeling_safe <- ifelse(df$feeling_safe == 99, NA, df$feeling_safe)

# Handle missing values in 'school_safety_risk' (IRT scale)
df$school_safety_risk <- ifelse(df$school_safety_risk == 99, NA, df$school_safety_risk)

# Handle missing values in 'curiosity_agreement' (IRT scale)
df$curiosity_agreement <- ifelse(df$curiosity_agreement == 99, NA, df$curiosity_agreement)

# Handle missing values in 'emotional_control_agreement' (IRT scale)
df$emotional_control_agreement <- ifelse(df$emotional_control_agreement == 99, NA, df$emotional_control_agreement)

# Handle missing values in 'career_info_seeking' (IRT scale)
df$career_info_seeking <- ifelse(df$career_info_seeking == 99, NA, df$career_info_seeking)

# Handle missing values in 'family_support' (IRT scale)
df$family_support <- ifelse(df$family_support == 99, NA, df$family_support)

# Handle missing values in 'math_disciplinary_climate' (IRT scale)
df$math_disciplinary_climate <- ifelse(df$math_disciplinary_climate == 99, NA, df$math_disciplinary_climate)

# Handle missing values in 'teacher_support_math' (IRT scale)
df$teacher_support_math <- ifelse(df$teacher_support_math == 99, NA, df$teacher_support_math)

# Handle missing values in 'cognitive_activation_reasoning' (IRT scale)
df$cognitive_activation_reasoning <- ifelse(df$cognitive_activation_reasoning == 99, NA, df$cognitive_activation_reasoning)

# Handle missing values in 'cognitive_activation_math_thinking' (IRT scale)
df$cognitive_activation_math_thinking <- ifelse(df$cognitive_activation_math_thinking == 99, NA, df$cognitive_activation_math_thinking)

# Handle missing values in 'exposure_formal_applied_math' (IRT scale)
df$exposure_formal_applied_math <- ifelse(df$exposure_formal_applied_math == 99, NA, df$exposure_formal_applied_math)

# Handle missing values in 'exposure_21st_century_math' (IRT scale)
df$exposure_21st_century_math <- ifelse(df$exposure_21st_century_math == 99, NA, df$exposure_21st_century_math)

# Handle missing values in 'math_self_efficacy_21st' (IRT scale)
df$math_self_efficacy_21st <- ifelse(df$math_self_efficacy_21st == 99, NA, df$math_self_efficacy_21st)

# Handle missing values in 'math_persistence' (IRT scale)
df$math_persistence <- ifelse(df$math_persistence == 99, NA, df$math_persistence)

# Handle missing values in 'school_sustain_learning' (IRT scale)
df$school_sustain_learning <- ifelse(df$school_sustain_learning == 99, NA, df$school_sustain_learning)

# Handle missing values in 'learning_resources' (IRT scale)
df$learning_resources <- ifelse(df$learning_resources == 99, NA, df$learning_resources)

# Handle missing values in 'self_learning_issues' (IRT scale)
df$self_learning_issues <- ifelse(df$self_learning_issues == 99, NA, df$self_learning_issues)

# Handle missing values in 'family_support_learning' (IRT scale)
df$family_support_learning <- ifelse(df$family_support_learning == 99, NA, df$family_support_learning)

# Handle missing values in 'learning_at_home_feel' (IRT scale)
df$learning_at_home_feel <- ifelse(df$learning_at_home_feel == 99, NA, df$learning_at_home_feel)

# Handle missing values in 'self_learning_efficacy' (IRT scale)
df$self_learning_efficacy <- ifelse(df$self_learning_efficacy == 99, NA, df$self_learning_efficacy)

# Handle missing values and factorize 'mother_education_level'
df$mother_education_level <- factor(ifelse(df$mother_education_level == 99, NA, df$mother_education_level),
                                    levels = 1:10,
                                    labels = c('Less than ISCED 1', 
                                               'ISCED 1 (Primary)', 
                                               'ISCED 2 (Lower Secondary)', 
                                               'ISCED 3.3 (Upper Secondary, No Tertiary Access)', 
                                               'ISCED 3.4 (Upper Secondary, Tertiary Access)', 
                                               'ISCED 4 (Post-secondary Non-Tertiary)', 
                                               'ISCED 5 (Short-cycle Tertiary)', 
                                               'ISCED 6 (Bachelor’s or Equivalent)', 
                                               'ISCED 7 (Master’s or Equivalent)', 
                                               'ISCED 8 (Doctoral or Equivalent)'),
                                    ordered = TRUE)

# Handle missing values and factorize 'father_education_level'
df$father_education_level <- factor(ifelse(df$father_education_level == 99, NA, df$father_education_level),
                                    levels = 1:10,
                                    labels = c('Less than ISCED 1', 
                                               'ISCED 1 (Primary)', 
                                               'ISCED 2 (Lower Secondary)', 
                                               'ISCED 3.3 (Upper Secondary, No Tertiary Access)', 
                                               'ISCED 3.4 (Upper Secondary, Tertiary Access)', 
                                               'ISCED 4 (Post-secondary Non-Tertiary)', 
                                               'ISCED 5 (Short-cycle Tertiary)', 
                                               'ISCED 6 (Bachelor’s or Equivalent)', 
                                               'ISCED 7 (Master’s or Equivalent)', 
                                               'ISCED 8 (Doctoral or Equivalent)'),
                                    ordered = TRUE)

# Handle missing values and factorize 'parent_highest_edu_lvl'
df$parent_highest_edu_lvl <- factor(ifelse(df$parent_highest_edu_lvl == 99, NA, df$parent_highest_edu_lvl),
                                    levels = 1:10,
                                    labels = c('Less than ISCED 1', 
                                               'ISCED 1 (Primary)', 
                                               'ISCED 2 (Lower Secondary)', 
                                               'ISCED 3.3 (Upper Secondary, No Tertiary Access)', 
                                               'ISCED 3.4 (Upper Secondary, Tertiary Access)', 
                                               'ISCED 4 (Post-secondary Non-Tertiary)', 
                                               'ISCED 5 (Short-cycle Tertiary)', 
                                               'ISCED 6 (Bachelor’s or Equivalent)', 
                                               'ISCED 7 (Master’s or Equivalent)', 
                                               'ISCED 8 (Doctoral or Equivalent)'),
                                    ordered = TRUE)

# Handle missing values for 'parent_edu_years_index'
df$parent_edu_years_index <- ifelse(df$parent_edu_years_index == 9999, NA, df$parent_edu_years_index)

# Handle missing values for 'mother_occupation_status'
df$mother_occupation_status <- ifelse(df$mother_occupation_status == 999, NA, df$mother_occupation_status)

# Handle missing values for 'father_occupation_status'
df$father_occupation_status <- ifelse(df$father_occupation_status == 999, NA, df$father_occupation_status)

# Handle missing values for 'parent_highest_occup'
df$parent_highest_occup <- ifelse(df$parent_highest_occup == 999, NA, df$parent_highest_occup)

# Handle missing values for 'ict_resources'
df$ict_resources <- ifelse(df$ict_resources == 99, NA, df$ict_resources)

# Handle missing values for 'home_possessions'
df$home_possessions <- ifelse(df$home_possessions == 99, NA, df$home_possessions)

# Handle missing values for 'escs_index'
df$escs_index <- ifelse(df$escs_index == 99, NA, df$escs_index)


# ----------------------------
# Debugging Analysis (After Cleaning)
# ----------------------------
debug_after <- data.frame(
  Dataset = "After Cleaning",
  `95` = sum(df[, sdv_vars] == 95, na.rm = TRUE),  # Should be 0 if cleaned correctly
  `97` = sum(df[, sdv_vars] == 97, na.rm = TRUE),  # Should be 0 if cleaned correctly
  `98` = sum(df[, sdv_vars] == 98, na.rm = TRUE),  # Should be 0 after transformation
  `99` = sum(df[, sdv_vars] == 99, na.rm = TRUE),  # Should be 0 after transformation
  `999` = sum(df[, sdv_vars] == 999, na.rm = TRUE),  # Should be 0 after transformation
  Total_NA = sum(is.na(df[, sdv_vars]))  # Count total NA values
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
cat("✅ Student Derived Variables Cleaning Complete!\n")
cat("✅ Cleaned data saved to:", output_path, "\n")
cat("✅ Debugging analysis saved to:", debug_path, "\n")

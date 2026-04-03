# --------------------------------------
# PISA 2022 School Questionnaire Cleaning
# --------------------------------------
# File: 03_scripts/01_preparation/22_school_questionnaire_cleaning.R
# Purpose:
# This script applies the implemented cleaning and transformation workflow used
# for PISA 2022 school questionnaire variables in the published Thailand random
# forest analysis.
#
# Main tasks:
# 1. Load the prior-step cleaned dataset.
# 2. Identify the school questionnaire variable block.
# 3. Apply variable-specific recoding, factor conversion, and numeric transformation rules.
# 4. Produce debugging summaries before and after cleaning.
# 5. Save the cleaned school questionnaire dataset and debugging output.
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
input_path <- "PATH_TO_LOCAL_INPUT/pisa2022_cleaned_12_ict_dv_cleaning.rds"
output_path <- "PATH_TO_LOCAL_OUTPUT/pisa2022_cleaned_13_school_questionnaire_cleaning.csv"
debug_path <- "PATH_TO_LOCAL_OUTPUT/pisa2022_cleaned_13_debugging_analysis.csv"

# Load dataset
df <- readRDS(input_path)

# ----------------------------
# Automatically Identify School Questionnaire Variables
# ----------------------------

# The implemented workflow identifies the school questionnaire block using
# the first and last renamed variables in the prepared dataset.
# Identify first and last School Questionnaire Variables
first_school_var <- "community_type"  
last_school_var <- "preparedness_remote_instruction"  

# Get all column names between the first and last school questionnaire variable
if (first_school_var %in% names(df) & last_school_var %in% names(df)) {
  school_vars <- names(df)[which(names(df) == first_school_var):which(names(df) == last_school_var)]
} else {
  school_vars <- intersect(names(df), c(first_school_var, last_school_var)) # Ensure it doesn't break if one variable is missing
}

# Ensure that only variables that exist in df are processed
school_vars <- intersect(school_vars, names(df))

# ----------------------------
# Debugging Analysis (Before Cleaning)
# ----------------------------
debug_before <- data.frame(
  Dataset = "Before Cleaning",
  `95` = sum(df[, school_vars] == 95, na.rm = TRUE),  # Count raw 95 values
  valid_skip = 0,  # To be filled after cleaning
  `97` = sum(df[, school_vars] == 97, na.rm = TRUE),  # Count raw 97 values
  `98` = sum(df[, school_vars] == 98, na.rm = TRUE),  # Count raw 98 values
  `99998` = sum(df[, school_vars] == 99998, na.rm = TRUE),  # Count raw 99998 values
  `99` = sum(df[, school_vars] == 99, na.rm = TRUE),  # Count raw 99 values
  Total_NA = sum(is.na(df[, school_vars]))  # Count total NA values
)

# -----------------------------------------------------
# Manual Transformations for School Questionnaire Data
# -----------------------------------------------------

# Convert community_type to an ordered factor (Rural → Megacity)
df$community_type <- factor(df$community_type, levels = c(1, 2, 3, 4, 5, 6), labels = c("A village, hamlet or rural area", "A small town", "A town", "A city", "A large city", "A megacity"), ordered = TRUE)

# Convert school_type to a categorical factor (Public/Private)
df$school_type <- factor(df$school_type, levels = c(1, 2), labels = c("Public School", "Private School"))

# Convert organisation_type to a categorical factor
df$organisation_type <- factor(df$organisation_type, levels = c(1, 2, 3, 4), labels = c("Religious Organisation", "Not-for-profit Organisation", "For-profit Organisation", "Government"))

# Convert gov_funding_percentage to numeric
df$gov_funding_percentage <- as.numeric(df$gov_funding_percentage) 

# Convert student_fees_percentage to numeric
df$student_fees_percentage <- as.numeric(df$student_fees_percentage)

# Convert donations_percentage to numeric
df$donations_percentage <- as.numeric(df$donations_percentage)

# Convert other_funding_percentage to numeric
df$other_funding_percentage <- as.numeric(df$other_funding_percentage)

# Convert schooling_availability to an ordered factor (More competition → No competition)
df$schooling_availability <- factor(df$schooling_availability, levels = c(1, 2, 3), 
      labels = c("Two or more competing schools", 
                "One competing school", 
                "No competing schools"), 
      ordered = TRUE)

# Convert total_enrollment_boys to numeric
df$total_enrollment_boys <- ifelse(df$total_enrollment_boys == 99998, NA, df$total_enrollment_boys)
df$total_enrollment_boys <- as.numeric(df$total_enrollment_boys)

# Convert total_enrollment_girls to numeric
df$total_enrollment_girls <- ifelse(df$total_enrollment_girls == 99998, NA, df$total_enrollment_girls)
df$total_enrollment_girls <- as.numeric(df$total_enrollment_girls)

# Convert heritage_language_different to numeric
df$heritage_language_different <- as.numeric(df$heritage_language_different)

# Convert special_learning_needs to numeric
df$special_learning_needs <- as.numeric(df$special_learning_needs)

# Convert disadvantaged_homes to numeric
df$disadvantaged_homes <- as.numeric(df$disadvantaged_homes)

# Convert immigrant_students to numeric
df$immigrant_students <- as.numeric(df$immigrant_students)

# Convert immigrant_parents to numeric
df$immigrant_parents <- as.numeric(df$immigrant_parents)

# Convert refugee_students to numeric
df$refugee_students <- as.numeric(df$refugee_students)

# Convert total_teachers_full_time to numeric
df$total_teachers_full_time <- as.numeric(df$total_teachers_full_time)

# Convert total_teachers_part_time to numeric
df$total_teachers_part_time <- as.numeric(df$total_teachers_part_time)

# Convert certified_teachers_full_time to numeric
df$certified_teachers_full_time <- ifelse(df$certified_teachers_full_time == 99998, NA, df$certified_teachers_full_time)
df$certified_teachers_full_time <- as.numeric(df$certified_teachers_full_time)

# Convert certified_teachers_part_time to numeric
df$certified_teachers_part_time <- ifelse(df$certified_teachers_part_time == 99998, NA, df$certified_teachers_part_time)
df$certified_teachers_part_time <- as.numeric(df$certified_teachers_part_time)

# Convert bachelor_degree_teachers_full to numeric
df$bachelor_degree_teachers_full <- ifelse(df$bachelor_degree_teachers_full == 99998, NA, df$bachelor_degree_teachers_full)
df$bachelor_degree_teachers_full <- as.numeric(df$bachelor_degree_teachers_full)

# Convert bachelor_degree_teachers_part to numeric
df$bachelor_degree_teachers_part <- ifelse(df$bachelor_degree_teachers_part == 99998, NA, df$bachelor_degree_teachers_part)
df$bachelor_degree_teachers_part <- as.numeric(df$bachelor_degree_teachers_part)

# Convert master_degree_teachers_full to numeric
df$master_degree_teachers_full <- ifelse(df$master_degree_teachers_full == 99998, NA, df$master_degree_teachers_full)
df$master_degree_teachers_full <- as.numeric(df$master_degree_teachers_full)

# Convert master_degree_teachers_part to numeric
df$master_degree_teachers_part <- ifelse(df$master_degree_teachers_part == 99998, NA, df$master_degree_teachers_part)
df$master_degree_teachers_part <- as.numeric(df$master_degree_teachers_part)

# Convert phd_degree_teachers_full to numeric
df$phd_degree_teachers_full <- as.numeric(df$phd_degree_teachers_full)

# Convert phd_degree_teachers_part to numeric
df$phd_degree_teachers_part <- as.numeric(df$phd_degree_teachers_part)

# Convert math_teachers_full_time to numeric
df$math_teachers_full_time <- as.numeric(df$math_teachers_full_time)

# Convert math_teachers_part_time to numeric
df$math_teachers_part_time <- as.numeric(df$math_teachers_part_time)

# Convert certified_math_teachers_full to numeric
df$certified_math_teachers_full <- ifelse(df$certified_math_teachers_full == 99998, NA, df$certified_math_teachers_full)
df$certified_math_teachers_full <- as.numeric(df$certified_math_teachers_full)

# Convert certified_math_teachers_part to numeric
df$certified_math_teachers_part <- as.numeric(df$certified_math_teachers_part)

# Convert math_degree_teachers_full to numeric
df$math_degree_teachers_full <- ifelse(df$math_degree_teachers_full == 99998, NA, df$math_degree_teachers_full)
df$math_degree_teachers_full <- as.numeric(df$math_degree_teachers_full)

# Convert math_degree_teachers_part to numeric
df$math_degree_teachers_part <- as.numeric(df$math_degree_teachers_part)

# Convert math_major_degree_teachers_full to numeric
df$math_major_degree_teachers_full <- ifelse(df$math_major_degree_teachers_full == 99998, NA, df$math_major_degree_teachers_full)
df$math_major_degree_teachers_full <- as.numeric(df$math_major_degree_teachers_full)

# Convert math_major_degree_teachers_part to numeric
df$math_major_degree_teachers_part <- ifelse(df$math_major_degree_teachers_part == 99998, NA, df$math_major_degree_teachers_part)
df$math_major_degree_teachers_part <- as.numeric(df$math_major_degree_teachers_part)

# Convert pedagogy_math_degree_full to numeric
df$pedagogy_math_degree_full <- ifelse(df$pedagogy_math_degree_full == 99998, NA, df$pedagogy_math_degree_full)
df$pedagogy_math_degree_full <- as.numeric(df$pedagogy_math_degree_full)

# Convert pedagogy_math_degree_part to numeric
df$pedagogy_math_degree_part <- ifelse(df$pedagogy_math_degree_part == 99998, NA, df$pedagogy_math_degree_part)
df$pedagogy_math_degree_part <- as.numeric(df$pedagogy_math_degree_part)

# Convert math_isc_lvl5_degree_full to numeric
df$math_isc_lvl5_degree_full <- ifelse(df$math_isc_lvl5_degree_full == 99998, NA, df$math_isc_lvl5_degree_full)
df$math_isc_lvl5_degree_full <- as.numeric(df$math_isc_lvl5_degree_full)

# Convert support_staff_pedagogical to numeric
df$support_staff_pedagogical <- as.numeric(df$support_staff_pedagogical)

# Convert support_staff_admin to numeric
df$support_staff_admin <- as.numeric(df$support_staff_admin)

# Convert support_staff_management to numeric
df$support_staff_management <- as.numeric(df$support_staff_management)

# Convert support_staff_other to numeric
df$support_staff_other <- as.numeric(df$support_staff_other)

# Convert admission_consider_academic to an ordered factor (Never → Always)
df$admission_consider_academic <- factor(df$admission_consider_academic, 
       levels = c(1, 2, 3), 
       labels = c("Never", "Sometimes", "Always"), 
       ordered = TRUE)

# Convert admission_consider_programme to an ordered factor (Never → Always)
df$admission_consider_programme <- factor(df$admission_consider_programme, 
       levels = c(1, 2, 3), 
       labels = c("Never", "Sometimes", "Always"), 
       ordered = TRUE)

# Convert admission_consider_residence to an ordered factor (Never → Always)
df$admission_consider_residence <- factor(df$admission_consider_residence, 
       levels = c(1, 2, 3), 
       labels = c("Never", "Sometimes", "Always"), 
       ordered = TRUE)

# Convert admission_consider_ethnicity to an ordered factor (Never → Always)
df$admission_consider_ethnicity <- factor(df$admission_consider_ethnicity, 
       levels = c(1, 2, 3), 
       labels = c("Never", "Sometimes", "Always"), 
       ordered = TRUE)

# Convert transfer_low_achievement to an ordered factor (Not likely → Very likely)
df$transfer_low_achievement <- factor(df$transfer_low_achievement, 
       levels = c(1, 2, 3), 
       labels = c("Not likely", "Likely", "Very likely"), 
       ordered = TRUE)

# Convert transfer_special_needs to an ordered factor (Not likely → Very likely)
df$transfer_special_needs <- factor(df$transfer_special_needs, 
       levels = c(1, 2, 3), 
       labels = c("Not likely", "Likely", "Very likely"), 
       ordered = TRUE)

# Convert appoint_hiring_teachers to factor
df$appoint_hiring_teachers <- factor(df$appoint_hiring_teachers, levels = c(1, 2, 3, 4, 6), 
       labels = c("Principal", "Teachers or members of school management team", "School governing board", "Local or municipal authority", "National or federal authority"))

# Convert dismiss_teachers to factor
df$dismiss_teachers <- factor(df$dismiss_teachers, levels = c(1, 2, 3, 4, 6), 
       labels = c("Principal", "Teachers or members of school management team", "School governing board", "Local or municipal authority", "National or federal authority"))

# Convert set_starting_salary_teachers to factor
df$set_starting_salary_teachers <- factor(df$set_starting_salary_teachers, levels = c(1, 2, 3, 4, 5, 6), 
       labels = c("Principal", "Teachers or members of school management team", "School governing board", "Local or municipal authority", "Regional or state authority", "National or federal authority"))

# Convert set_salary_increases_teachers to factor
df$set_salary_increases_teachers <- factor(df$set_salary_increases_teachers, levels = c(1, 2, 3, 4, 5, 6), 
       labels = c("Principal", "Teachers or members of school management team", "School governing board", "Local or municipal authority", "Regional or state authority", "National or federal authority"))

# Convert formulate_budget to factor
df$formulate_budget <- factor(df$formulate_budget, levels = c(1, 2, 3, 4, 6), 
       labels = c("Principal", "Teachers or members of school management team", "School governing board", "Local or municipal authority", "National or federal authority"))

# Convert decide_budget_allocation to factor
df$decide_budget_allocation <- factor(df$decide_budget_allocation, levels = c(1, 2, 3, 4, 6), 
       labels = c("Principal", "Teachers or members of school management team", "School governing board", "Local or municipal authority", "National or federal authority"))

# Convert establish_disciplinary_policies to a factor
df$establish_disciplinary_policies <- factor(df$establish_disciplinary_policies, 
       levels = c(1, 2, 3, 4, 6), 
       labels = c("Principal", "Teachers or members of school management team", 
                  "School governing board", "Local or municipal authority", 
                  "National or federal authority"))

# Convert establish_assessment_policies to a factor
df$establish_assessment_policies <- factor(df$establish_assessment_policies, 
       levels = c(1, 2, 3, 4, 5, 6), 
       labels = c("Principal", "Teachers or members of school management team", 
                  "School governing board", "Local or municipal authority", 
                  "Regional or state authority", "National or federal authority"))

# Convert approve_student_admission to a factor
df$approve_student_admission <- factor(df$approve_student_admission, 
       levels = c(1, 2, 3, 4, 6), 
       labels = c("Principal", "Teachers or members of school management team", 
                  "School governing board", "Local or municipal authority", 
                  "National or federal authority"))

# Convert choose_learning_materials to a factor
df$choose_learning_materials <- factor(df$choose_learning_materials, 
       levels = c(1, 2, 3, 6), 
       labels = c("Teachers or members of school management team", 
                  "School governing board", "School governing board", 
                   "National or federal authority"))

# Convert determine_course_content to a factor
df$determine_course_content <- factor(df$determine_course_content, 
       levels = c(1, 2, 3, 4, 6), 
       labels = c("Principal", "Teachers or members of school management team", 
                  "School governing board", "Local or municipal authority", 
                  "National or federal authority"))

# Convert decide_courses_offered to a factor
df$decide_courses_offered <- factor(df$decide_courses_offered, 
       levels = c(1, 2, 3, 4, 5, 6), 
       labels = c("Principal", "Teachers or members of school management team", 
                  "School governing board", "Local or municipal authority", 
                  "Regional or state authority", "National or federal authority"))

# Convert mgmt_collaborate_class_discipline to an ordered factor
df$mgmt_collaborate_class_discipline <- factor(df$mgmt_collaborate_class_discipline, 
       levels = c(1, 2, 3, 4, 5), 
       labels = c("Never or almost never", "About once or twice a year", 
                  "About once or twice a month", "About once or twice a week", 
                  "Every day or almost every day"), ordered = TRUE)

# Convert mgmt_feedback_classroom_observe to an ordered factor
df$mgmt_feedback_classroom_observe <- factor(df$mgmt_feedback_classroom_observe, 
       levels = c(1, 2, 3, 4, 5), 
       labels = c("Never or almost never", "About once or twice a year", 
                  "About once or twice a month", "About once or twice a week", 
                  "Every day or almost every day"), ordered = TRUE)

# Convert mgmt_support_cooperation_teachers to an ordered factor
df$mgmt_support_cooperation_teachers <- factor(df$mgmt_support_cooperation_teachers, 
       levels = c(1, 2, 3, 4, 5), 
       labels = c("Never or almost never", "About once or twice a year", 
                  "About once or twice a month", "About once or twice a week", 
                  "Every day or almost every day"), ordered = TRUE)

# Convert mgmt_support_teaching_skills to an ordered factor
df$mgmt_support_teaching_skills <- factor(df$mgmt_support_teaching_skills, 
       levels = c(1, 2, 3, 4, 5), 
       labels = c("Never or almost never", "About once or twice a year", 
                  "About once or twice a month", "About once or twice a week", 
                  "Every day or almost every day"), ordered = TRUE)

# Convert mgmt_prof_development_plan to an ordered factor
df$mgmt_prof_development_plan <- factor(df$mgmt_prof_development_plan, 
       levels = c(1, 2, 3, 4, 5), 
       labels = c("Never or almost never", "About once or twice a year", 
                  "About once or twice a month", "About once or twice a week", 
                  "Every day or almost every day"), ordered = TRUE)

# Convert num_computers_educational to numeric
df$num_computers_educational <- as.numeric(df$num_computers_educational)

# Convert num_internet_connected_computers to numeric (handling invalid responses)
df$num_internet_connected_computers <- ifelse(df$num_internet_connected_computers == 99998, NA, df$num_internet_connected_computers)
df$num_internet_connected_computers <- as.numeric(df$num_internet_connected_computers)

# Convert num_tablets_ebook_readers to numeric
df$num_tablets_ebook_readers <- as.numeric(df$num_tablets_ebook_readers)

# Convert num_interactive_whiteboards to numeric
df$num_interactive_whiteboards <- as.numeric(df$num_interactive_whiteboards)

# Convert num_data_projectors to numeric
df$num_data_projectors <- as.numeric(df$num_data_projectors)

# Convert teacher_computers_internet to numeric
df$teacher_computers_internet <- as.numeric(df$teacher_computers_internet)

# Convert written_statement_digital_devices to factor
df$written_statement_digital_devices <- factor(df$written_statement_digital_devices, 
      levels = c(1, 2), 
      labels = c("Yes", "No"))

# Convert no_cell_phones_policy to factor
df$no_cell_phones_policy <- factor(df$no_cell_phones_policy, 
      levels = c(1, 2), 
      labels = c("Yes", "No"))

# Convert formal_digital_guidelines to factor
df$formal_digital_guidelines <- factor(df$formal_digital_guidelines, 
      levels = c(1, 2), 
      labels = c("Yes", "No"))

# Convert teacher_set_rules_digital_use to factor
df$teacher_set_rules_digital_use <- factor(df$teacher_set_rules_digital_use, 
      levels = c(1, 2), 
      labels = c("Yes", "No"))

# Convert collab_student_rules_digital_use to factor
df$collab_student_rules_digital_use <- factor(df$collab_student_rules_digital_use, 
      levels = c(1, 2), 
      labels = c("Yes", "No"))

# Convert responsible_internet_program to factor
df$responsible_internet_program <- factor(df$responsible_internet_program, 
      levels = c(1, 2), 
      labels = c("Yes", "No"))

# Convert social_network_policy to factor
df$social_network_policy <- factor(df$social_network_policy, 
      levels = c(1, 2), 
      labels = c("Yes", "No"))

# Convert collab_digital_teachers to factor
df$collab_digital_teachers <- factor(df$collab_digital_teachers, 
      levels = c(1, 2), 
      labels = c("Yes", "No"))

# Convert teacher_meeting_digital_materials to factor
df$teacher_meeting_digital_materials <- factor(df$teacher_meeting_digital_materials, 
      levels = c(1, 2), 
      labels = c("Yes", "No"))

# Convert self_evaluation to factor
df$self_evaluation <- factor(df$self_evaluation, levels = c(1, 2), 
      labels = c("Yes, this is mandatory, e.g. based on district or ministry policies", 
                 "Yes, based on school initiative"))

# Convert external_evaluation to factor
df$external_evaluation <- factor(df$external_evaluation, levels = c(1, 2, 3), 
      labels = c("Yes, this is mandatory, e.g. based on district or ministry policies", 
                 "Yes, based on school initiative", "No"))

# Convert written_curriculum_goals to factor
df$written_curriculum_goals <- factor(df$written_curriculum_goals, levels = c(1, 2, 3), 
      labels = c("Yes, this is mandatory, e.g. based on district or ministry policies", 
                 "Yes, based on school initiative", "No"))

# Convert written_performance_standards to factor
df$written_performance_standards <- factor(df$written_performance_standards, levels = c(1, 2, 3), 
      labels = c("Yes, this is mandatory, e.g. based on district or ministry policies", 
                 "Yes, based on school initiative", "No"))

# Convert record_teacher_student_data to factor
df$record_teacher_student_data <- factor(df$record_teacher_student_data, levels = c(1, 2, 3), 
      labels = c("Yes, this is mandatory, e.g. based on district or ministry policies", 
                 "Yes, based on school initiative", "No"))

# Convert record_test_graduation_rates to factor
df$record_test_graduation_rates <- factor(df$record_test_graduation_rates, levels = c(1, 2, 3), 
      labels = c("Yes, this is mandatory, e.g. based on district or ministry policies", 
                 "Yes, based on school initiative", "No"))

# Convert student_feedback to factor
df$student_feedback <- factor(df$student_feedback, levels = c(1, 2, 3), 
      labels = c("Yes, this is mandatory, e.g. based on district or ministry policies", 
                 "Yes, based on school initiative", "No"))

# Convert teacher_mentoring to factor
df$teacher_mentoring <- factor(df$teacher_mentoring, levels = c(1, 2, 3), 
      labels = c("Yes, this is mandatory, e.g. based on district or ministry policies", 
                 "Yes, based on school initiative", "No"))

# Convert consultation_school_improvement to factor
df$consultation_school_improvement <- factor(df$consultation_school_improvement, levels = c(1, 2, 3), 
      labels = c("Yes, this is mandatory, e.g. based on district or ministry policies", 
                 "Yes, based on school initiative", "No"))

# Convert math_policy_standardized to factor
df$math_policy_standardized <- factor(df$math_policy_standardized, levels = c(1, 2, 3), 
      labels = c("Yes, this is mandatory, e.g. based on district or ministry policies", 
                 "Yes, based on school initiative", "No"))

# Transform SC200 variables
df$external_eval_policy_changes <- factor(df$external_eval_policy_changes, levels = c(1, 2, 3), 
      labels = c("Yes", "No", "Not Applicable"))

df$external_eval_action_plan <- factor(df$external_eval_action_plan, levels = c(1, 2, 3), 
      labels = c("No", "Yes", "Not Applicable"))

df$external_eval_teaching_plan <- factor(df$external_eval_teaching_plan, levels = c(1, 2, 3), 
      labels = c("Yes", "No", "Not Applicable"))

df$external_eval_measures_practice <- factor(df$external_eval_measures_practice, levels = c(1, 2, 3), 
      labels = c("Yes", "No", "Not Applicable"))

# Transform SC032 variables
df$teacher_monitor_tests <- factor(df$teacher_monitor_tests, levels = c(1, 2), 
      labels = c("Yes", "No"))

df$teacher_monitor_peer_review <- factor(df$teacher_monitor_peer_review, levels = c(1, 2), 
      labels = c("Yes", "No"))

df$teacher_monitor_principal_observe <- factor(df$teacher_monitor_principal_observe, levels = c(1, 2), 
      labels = c("Yes", "No"))

df$teacher_monitor_external_observe <- factor(df$teacher_monitor_external_observe, levels = c(1, 2), 
      labels = c("Yes", "No"))

# Convert SC193 variables to ordered factors
df$eval_salary_change <- factor(df$eval_salary_change, levels = c(1, 2, 3, 4), 
      labels = c("No impact", "Small impact", "Moderate impact", "Large impact"), 
      ordered = TRUE)

df$eval_financial_bonus <- factor(df$eval_financial_bonus, levels = c(1, 2, 3, 4), 
      labels = c("No impact", "Small impact", "Moderate impact", "Large impact"), 
      ordered = TRUE)

df$eval_prof_dev_opportunities <- factor(df$eval_prof_dev_opportunities, levels = c(1, 2, 3, 4), 
      labels = c("No impact", "Small impact", "Moderate impact", "Large impact"), 
      ordered = TRUE)

df$eval_career_advancement <- factor(df$eval_career_advancement, levels = c(1, 2, 3, 4), 
      labels = c("No impact", "Small impact", "Moderate impact", "Large impact"), 
      ordered = TRUE)

df$eval_public_recognition <- factor(df$eval_public_recognition, levels = c(1, 2, 3, 4), 
      labels = c("No impact", "Small impact", "Moderate impact", "Large impact"), 
      ordered = TRUE)

df$eval_work_responsibilities <- factor(df$eval_work_responsibilities, levels = c(1, 2, 3, 4), 
      labels = c("No impact", "Small impact", "Moderate impact", "Large impact"), 
      ordered = TRUE)

df$eval_school_dev_role <- factor(df$eval_school_dev_role, levels = c(1, 2, 3, 4), 
      labels = c("No impact", "Small impact", "Moderate impact", "Large impact"), 
      ordered = TRUE)

# Transform all_teachers_prof_dev_percent as numeric
df$all_teachers_prof_dev_percent <- as.numeric(df$all_teachers_prof_dev_percent)

# Transform math_teachers_prof_dev_percent as numeric
df$math_teachers_prof_dev_percent <- as.numeric(df$math_teachers_prof_dev_percent)

# Convert inhouse_training_specialists to a factor
df$inhouse_training_specialists <- factor(df$inhouse_training_specialists, 
      levels = c(1, 2), 
      labels = c("Yes", "No"))

# Convert inhouse_workshops_specific_issues to a factor
df$inhouse_workshops_specific_issues <- factor(df$inhouse_workshops_specific_issues, 
      levels = c(1, 2), 
      labels = c("Yes", "No"))

# Convert inhouse_workshops_teacher_groups to a factor
df$inhouse_workshops_teacher_groups <- factor(df$inhouse_workshops_teacher_groups, 
      levels = c(1, 2), 
      labels = c("Yes", "No"))

# Convert inhouse_training_math_specialists to a factor
df$inhouse_training_math_specialists <- factor(df$inhouse_training_math_specialists, 
      levels = c(1, 2), 
      labels = c("Yes", "No"))

# Convert inhouse_workshops_math_issues to a factor
df$inhouse_workshops_math_issues <- factor(df$inhouse_workshops_math_issues, 
      levels = c(1, 2), 
      labels = c("Yes", "No"))

# Convert inhouse_workshops_math_teacher_groups to a factor
df$inhouse_workshops_math_teacher_groups <- factor(df$inhouse_workshops_math_teacher_groups, 
      levels = c(1, 2), 
      labels = c("Yes", "No"))

# Convert math_dev_content to a factor
df$math_dev_content <- factor(df$math_dev_content, 
      levels = c(1, 2), 
      labels = c("Yes", "No"))

# Convert math_dev_pedagogy to a factor
df$math_dev_pedagogy <- factor(df$math_dev_pedagogy, 
      levels = c(1, 2), 
      labels = c("Yes", "No"))

# Convert math_dev_curriculum to a factor
df$math_dev_curriculum <- factor(df$math_dev_curriculum, 
       levels = c(1, 2), 
       labels = c("Yes", "No"))

# Convert math_dev_digital_resources to a factor
df$math_dev_digital_resources <- factor(df$math_dev_digital_resources, 
       levels = c(1, 2), 
       labels = c("Yes", "No"))

# Convert math_dev_critical_thinking to a factor
df$math_dev_critical_thinking <- factor(df$math_dev_critical_thinking, 
       levels = c(1, 2), 
       labels = c("Yes", "No"))

# Convert math_dev_assessment to a factor
df$math_dev_assessment <- factor(df$math_dev_assessment, 
       levels = c(1, 2), 
       labels = c("Yes", "No"))

# Convert math_dev_individual_needs to a factor
df$math_dev_individual_needs <- factor(df$math_dev_individual_needs, 
       levels = c(1, 2), 
       labels = c("Yes", "No"))

# Convert SC017 variables to ordered factors with meaningful labels

df$hindered_lack_teaching_staff <- factor(df$hindered_lack_teaching_staff, 
       levels = c(1, 2, 3, 4), 
       labels = c("Not at all", "Very little", "To some extent", "A lot"), 
       ordered = TRUE)

df$hindered_inadequate_teaching_staff <- factor(df$hindered_inadequate_teaching_staff, 
       levels = c(1, 2, 3, 4), 
       labels = c("Not at all", "Very little", "To some extent", "A lot"), 
       ordered = TRUE)

df$hindered_lack_assisting_staff <- factor(df$hindered_lack_assisting_staff, 
       levels = c(1, 2, 3, 4), 
       labels = c("Not at all", "Very little", "To some extent", "A lot"), 
       ordered = TRUE)

df$hindered_inadequate_assisting_staff <- factor(df$hindered_inadequate_assisting_staff, 
       levels = c(1, 2, 3, 4), 
       labels = c("Not at all", "Very little", "To some extent", "A lot"), 
       ordered = TRUE)

df$hindered_lack_materials <- factor(df$hindered_lack_materials, 
       levels = c(1, 2, 3, 4), 
       labels = c("Not at all", "Very little", "To some extent", "A lot"), 
       ordered = TRUE)

df$hindered_inadequate_materials <- factor(df$hindered_inadequate_materials, 
       levels = c(1, 2, 3, 4), 
       labels = c("Not at all", "Very little", "To some extent", "A lot"), 
       ordered = TRUE)

df$hindered_lack_infrastructure <- factor(df$hindered_lack_infrastructure, 
       levels = c(1, 2, 3, 4), 
       labels = c("Not at all", "Very little", "To some extent", "A lot"), 
       ordered = TRUE)

df$hindered_inadequate_infrastructure <- factor(df$hindered_inadequate_infrastructure, 
       levels = c(1, 2, 3, 4), 
       labels = c("Not at all", "Very little", "To some extent", "A lot"), 
       ordered = TRUE)

df$hindered_lack_digital_resources <- factor(df$hindered_lack_digital_resources, 
       levels = c(1, 2, 3, 4), 
       labels = c("Not at all", "Very little", "To some extent", "A lot"), 
       ordered = TRUE)

df$hindered_inadequate_digital_resources <- factor(df$hindered_inadequate_digital_resources, 
       levels = c(1, 2, 3, 4), 
       labels = c("Not at all", "Very little", "To some extent", "A lot"), 
       ordered = TRUE)

# Convert SC061 variables to ordered factors with meaningful labels

df$hindered_student_truancy <- factor(df$hindered_student_truancy, 
       levels = c(1, 2, 3, 4), 
       labels = c("Not at all", "Very little", "To some extent", "A lot"), 
       ordered = TRUE)

df$hindered_student_skipping_classes <- factor(df$hindered_student_skipping_classes, 
       levels = c(1, 2, 3, 4), 
       labels = c("Not at all", "Very little", "To some extent", "A lot"), 
       ordered = TRUE)

df$hindered_student_disrespect_teachers <- factor(df$hindered_student_disrespect_teachers, 
       levels = c(1, 2, 3, 4), 
       labels = c("Not at all", "Very little", "To some extent", "A lot"), 
       ordered = TRUE)

df$hindered_student_alcohol_drugs <- factor(df$hindered_student_alcohol_drugs, 
       levels = c(1, 2, 3), 
       labels = c("Not at all", "Very little", "To some extent"), 
       ordered = TRUE)

df$hindered_student_bullying <- factor(df$hindered_student_bullying, 
       levels = c(1, 2, 3), 
       labels = c("Not at all", "Very little", "To some extent"), 
       ordered = TRUE)

df$hindered_student_inattention <- factor(df$hindered_student_inattention, 
       levels = c(1, 2, 3, 4), 
       labels = c("Not at all", "Very little", "To some extent", "A lot"), 
       ordered = TRUE)

df$hindered_teacher_meet_needs <- factor(df$hindered_teacher_meet_needs, 
       levels = c(1, 2, 3), 
       labels = c("Not at all", "Very little", "To some extent"), 
       ordered = TRUE)

df$hindered_teacher_absenteeism <- factor(df$hindered_teacher_absenteeism, 
       levels = c(1, 2, 3, 4), 
       labels = c("Not at all", "Very little", "To some extent", "A lot"), 
       ordered = TRUE)

df$hindered_staff_resistance_change <- factor(df$hindered_staff_resistance_change, 
       levels = c(1, 2, 3), 
       labels = c("Not at all", "Very little", "To some extent"), 
       ordered = TRUE)

df$hindered_teacher_unprepared <- factor(df$hindered_teacher_unprepared, 
       levels = c(1, 2, 3, 4), 
       labels = c("Not at all", "Very little", "To some extent", "A lot"), 
       ordered = TRUE)

# Convert SC172 variables to ordered factors

df$problem_profanity <- factor(df$problem_profanity, 
       levels = c(1, 2, 3, 4), 
       labels = c("Not at all", "Small extent", "Moderate extent", "Large extent"), 
       ordered = TRUE)

df$problem_theft <- factor(df$problem_theft, 
       levels = c(1, 2, 3, 4), 
       labels = c("Not at all", "Small extent", "Moderate extent", "Large extent"), 
       ordered = TRUE)

df$problem_student_abuse <- factor(df$problem_student_abuse, 
       levels = c(1, 2, 3, 4), 
       labels = c("Not at all", "Small extent", "Moderate extent", "Large extent"), 
       ordered = TRUE)

df$problem_teacher_abuse <- factor(df$problem_teacher_abuse, 
       levels = c(1, 2, 3, 4), 
       labels = c("Not at all", "Small extent", "Moderate extent", "Large extent"), 
       ordered = TRUE)

# Convert staff_help_recognize_similarities to an ordered factor
df$staff_help_recognize_similarities <- factor(df$staff_help_recognize_similarities, 
       levels = c(1, 2, 3, 4, 5), 
       labels = c("Never or almost never", 
                  "About once or twice a year", 
                  "About once or twice a month", 
                  "About once or twice a week", 
                  "Every day or almost every day"), 
       ordered = TRUE)

# Convert staff_encourage_common_ground to an ordered factor
df$staff_encourage_common_ground <- factor(df$staff_encourage_common_ground, 
       levels = c(1, 2, 3, 4, 5), 
       labels = c("Never or almost never", 
                  "About once or twice a year", 
                  "About once or twice a month", 
                  "About once or twice a week", 
                  "Every day or almost every day"), 
       ordered = TRUE)

# Convert staff_support_diverse_identities to an ordered factor
df$staff_support_diverse_identities <- factor(df$staff_support_diverse_identities, 
       levels = c(1, 2, 3, 4, 5), 
       labels = c("Never or almost never", 
                  "About once or twice a year", 
                  "About once or twice a month", 
                  "About once or twice a week", 
                  "Every day or almost every day"), 
       ordered = TRUE)

# Convert staff_teach_respond_discrimination to an ordered factor
df$staff_teach_respond_discrimination <- factor(df$staff_teach_respond_discrimination, 
       levels = c(1, 2, 3, 4, 5), 
       labels = c("Never or almost never", 
                  "About once or twice a year", 
                  "About once or twice a month", 
                  "About once or twice a week", 
                  "Every day or almost every day"), 
       ordered = TRUE)

# Convert staff_teach_inclusivity to an ordered factor
df$staff_teach_inclusivity <- factor(df$staff_teach_inclusivity, 
       levels = c(1, 2, 3, 4, 5), 
       labels = c("Never or almost never", 
                  "About once or twice a year", 
                  "About once or twice a month", 
                  "About once or twice a week", 
                  "Every day or almost every day"), 
       ordered = TRUE)

# Convert staff_support_disadvantaged to an ordered factor
df$staff_support_disadvantaged <- factor(df$staff_support_disadvantaged, 
       levels = c(1, 2, 3, 4, 5), 
       labels = c("Never or almost never", 
                  "About once or twice a year", 
                  "About once or twice a month", 
                  "About once or twice a week", 
                  "Every day or almost every day"), 
       ordered = TRUE)

# Convert parents_initiative_behavior to numeric
df$parents_initiative_behavior <- as.numeric(df$parents_initiative_behavior)

# Convert teachers_initiative_behavior to numeric
df$teachers_initiative_behavior <- as.numeric(df$teachers_initiative_behavior)

# Convert parents_initiative_progress to numeric
df$parents_initiative_progress <- as.numeric(df$parents_initiative_progress)

# Convert teachers_initiative_progress to numeric
df$teachers_initiative_progress <- as.numeric(df$teachers_initiative_progress)

# Convert parents_volunteering_activities to numeric
df$parents_volunteering_activities <- as.numeric(df$parents_volunteering_activities)

# Convert parents_school_governance to numeric
df$parents_school_governance <- as.numeric(df$parents_school_governance)

# Convert parents_assist_fundraising to numeric
df$parents_assist_fundraising <- as.numeric(df$parents_assist_fundraising)

# Convert staff_invite_volunteers to an ordered factor
df$staff_invite_volunteers <- factor(df$staff_invite_volunteers, 
       levels = c(1, 2, 3, 4), 
       labels = c("Never or almost never", 
                  "A few times a year", 
                  "A few times a month", 
                  "Once a week or more"), 
       ordered = TRUE)

# Convert staff_communicate_programmes to an ordered factor
df$staff_communicate_programmes <- factor(df$staff_communicate_programmes, 
       levels = c(1, 2, 3, 4), 
       labels = c("Never or almost never", 
                  "A few times a year", 
                  "A few times a month", 
                  "Once a week or more"), 
       ordered = TRUE)

# Convert staff_communicate_progress to an ordered factor
df$staff_communicate_progress <- factor(df$staff_communicate_progress, 
       levels = c(1, 2, 3, 4), 
       labels = c("Never or almost never", 
                  "A few times a year", 
                  "A few times a month", 
                  "Once a week or more"), 
       ordered = TRUE)

# Convert staff_include_in_decisions to an ordered factor
df$staff_include_in_decisions <- factor(df$staff_include_in_decisions, 
       levels = c(1, 2, 3, 4), 
       labels = c("Never or almost never", 
                  "A few times a year", 
                  "A few times a month", 
                  "Once a week or more"), 
       ordered = TRUE)

# Convert staff_help_homework_guidance to an ordered factor
df$staff_help_homework_guidance <- factor(df$staff_help_homework_guidance, 
       levels = c(1, 2, 3, 4), 
       labels = c("Never or almost never", 
                  "A few times a year", 
                  "A few times a month", 
                  "Once a week or more"), 
       ordered = TRUE)

# Convert staff_help_math_skills to an ordered factor
df$staff_help_math_skills <- factor(df$staff_help_math_skills, 
       levels = c(1, 2, 3, 4), 
       labels = c("Never or almost never", 
                  "A few times a year", 
                  "A few times a month", 
                  "Once a week or more"), 
       ordered = TRUE)

# Convert minutes_class_period_math to numeric, handling invalid (98) as NA
df$minutes_class_period_math <- as.numeric(ifelse(df$minutes_class_period_math == 99998, NA, df$minutes_class_period_math))
df$minutes_class_period_math <- as.numeric(df$minutes_class_period_math)

# Convert minutes_class_period_avg to numeric
df$minutes_class_period_avg <- as.numeric(df$minutes_class_period_avg)

# Convert study_help_homework_room to a factor (Yes/No)
df$study_help_homework_room <- factor(df$study_help_homework_room, 
       levels = c(1, 2), 
       labels = c("Yes", "No"))

# Convert study_help_staff to a factor (Yes/No)
df$study_help_staff <- factor(df$study_help_staff, 
       levels = c(1, 2), 
       labels = c("Yes", "No"))

# Convert study_help_peer_tutoring to a factor (Yes/No)
df$study_help_peer_tutoring <- factor(df$study_help_peer_tutoring, 
       levels = c(1, 2), 
       labels = c("Yes", "No"))

# Convert assessment_mandatory_tests to an ordered factor
df$assessment_mandatory_tests <- factor(df$assessment_mandatory_tests, 
       levels = c(1, 2, 3, 4, 5), 
       labels = c("Never", "1-2 times a year", "3-5 times a year", "Monthly", "More than once a month"), 
       ordered = TRUE)

# Convert assessment_nonmandatory_tests to an ordered factor
df$assessment_nonmandatory_tests <- factor(df$assessment_nonmandatory_tests, 
       levels = c(1, 2, 3, 4, 5), 
       labels = c("Never", "1-2 times a year", "3-5 times a year", "Monthly", "More than once a month"), 
       ordered = TRUE)

# Convert assessment_teacher_tests to an ordered factor
df$assessment_teacher_tests <- factor(df$assessment_teacher_tests, 
       levels = c(1, 2, 3, 4, 5), 
       labels = c("Never", "1-2 times a year", "3-5 times a year", "Monthly", "More than once a month"), 
       ordered = TRUE)

# Convert assessment_teacher_ratings to an ordered factor
df$assessment_teacher_ratings <- factor(df$assessment_teacher_ratings, 
       levels = c(1, 2, 3, 4, 5), 
       labels = c("Never", "1-2 times a year", "3-5 times a year", "Monthly", "More than once a month"), 
       ordered = TRUE)

# Convert standardized_tests_guide_learning to factor with "valid_skip" for value 95
df$standardized_tests_guide_learning <- factor(
  ifelse(df$standardized_tests_guide_learning == 95, "valid_skip", as.character(df$standardized_tests_guide_learning)),
  levels = c("1", "2", "valid_skip"),
  labels = c("Yes", "No", "valid_skip")
)

# Convert teacher_tests_guide_learning to factor with "valid_skip" for value 95
df$teacher_tests_guide_learning <- factor(
  ifelse(df$teacher_tests_guide_learning == 95, "valid_skip", as.character(df$teacher_tests_guide_learning)),
  levels = c("1", "2", "valid_skip"),
  labels = c("Yes", "No", "valid_skip")
)

# Convert standardized_tests_inform_parents to factor with "valid_skip" for value 95
df$standardized_tests_inform_parents <- factor(
  ifelse(df$standardized_tests_inform_parents == 95, "valid_skip", as.character(df$standardized_tests_inform_parents)),
  levels = c("1", "2", "valid_skip"),
  labels = c("Yes", "No", "valid_skip")
)

# Convert teacher_tests_inform_parents to factor with "valid_skip" for value 95
df$teacher_tests_inform_parents <- factor(
  ifelse(df$teacher_tests_inform_parents == 95, "valid_skip", as.character(df$teacher_tests_inform_parents)),
  levels = c("1", "2", "valid_skip"),
  labels = c("Yes", "No", "valid_skip")
)

# Convert standardized_tests_retention to factor with "valid_skip" for value 95
df$standardized_tests_retention <- factor(
  ifelse(df$standardized_tests_retention == 95, "valid_skip", 
         ifelse(df$standardized_tests_retention == 1, "Yes", 
                ifelse(df$standardized_tests_retention == 2, "No", NA))),
  levels = c("Yes", "No", "valid_skip"),
  labels = c("Yes", "No", "valid_skip")
)

# Convert teacher_tests_retention to factor with "valid_skip" for value 95
df$teacher_tests_retention <- factor(
  ifelse(df$teacher_tests_retention == 95, "valid_skip", 
         ifelse(df$teacher_tests_retention == 1, "Yes", 
                ifelse(df$teacher_tests_retention == 2, "No", NA))),
  levels = c("Yes", "No", "valid_skip"),
  labels = c("Yes", "No", "valid_skip")
)

# Convert standardized_tests_grouping to factor with "valid_skip" for value 95
df$standardized_tests_grouping <- factor(
  ifelse(df$standardized_tests_grouping == 95, "valid_skip", 
         ifelse(df$standardized_tests_grouping == 1, "Yes", 
                ifelse(df$standardized_tests_grouping == 2, "No", NA))),
  levels = c("Yes", "No", "valid_skip"),
  labels = c("Yes", "No", "valid_skip")
)

# Convert teacher_tests_grouping to factor with "valid_skip" for value 95
df$teacher_tests_grouping <- factor(
  ifelse(df$teacher_tests_grouping == 95, "valid_skip", 
         ifelse(df$teacher_tests_grouping == 1, "Yes", 
                ifelse(df$teacher_tests_grouping == 2, "No", NA))),
  levels = c("Yes", "No", "valid_skip"),
  labels = c("Yes", "No", "valid_skip")
)

# Convert standardized_tests_school_comparison to factor with "valid_skip" for value 95
df$standardized_tests_school_comparison <- factor(
  ifelse(df$standardized_tests_school_comparison == 95, "valid_skip", 
         ifelse(df$standardized_tests_school_comparison == 1, "Yes", 
                ifelse(df$standardized_tests_school_comparison == 2, "No", NA))),
  levels = c("Yes", "No", "valid_skip"),
  labels = c("Yes", "No", "valid_skip")
)

# Convert teacher_tests_school_comparison to factor with "valid_skip" for value 95
df$teacher_tests_school_comparison <- factor(
  ifelse(df$teacher_tests_school_comparison == 95, "valid_skip", 
         ifelse(df$teacher_tests_school_comparison == 1, "Yes", 
                ifelse(df$teacher_tests_school_comparison == 2, "No", NA))),
  levels = c("Yes", "No", "valid_skip"),
  labels = c("Yes", "No", "valid_skip")
)

# Convert standardized_tests_monitor_school to factor with "valid_skip" for value 95
df$standardized_tests_monitor_school <- factor(
  ifelse(df$standardized_tests_monitor_school == 95, "valid_skip", 
         ifelse(df$standardized_tests_monitor_school == 1, "Yes", 
                ifelse(df$standardized_tests_monitor_school == 2, "No", NA))),
  levels = c("Yes", "No", "valid_skip"),
  labels = c("Yes", "No", "valid_skip")
)

# Convert teacher_tests_monitor_school to factor with "valid_skip" for value 95
df$teacher_tests_monitor_school <- factor(
  ifelse(df$teacher_tests_monitor_school == 95, "valid_skip", 
         ifelse(df$teacher_tests_monitor_school == 1, "Yes", 
                ifelse(df$teacher_tests_monitor_school == 2, "No", NA))),
  levels = c("Yes", "No", "valid_skip"),
  labels = c("Yes", "No", "valid_skip")
)

# Convert standardized_tests_teacher_effectiveness to factor with "valid_skip" for value 95
df$standardized_tests_teacher_effectiveness <- factor(
  ifelse(df$standardized_tests_teacher_effectiveness == 95, "valid_skip", 
         ifelse(df$standardized_tests_teacher_effectiveness == 1, "Yes", 
                ifelse(df$standardized_tests_teacher_effectiveness == 2, "No", NA))),
  levels = c("Yes", "No", "valid_skip"),
  labels = c("Yes", "No", "valid_skip")
)

# Convert teacher_tests_teacher_effectiveness to factor with "valid_skip" for value 95
df$teacher_tests_teacher_effectiveness <- factor(
  ifelse(df$teacher_tests_teacher_effectiveness == 95, "valid_skip", 
         ifelse(df$teacher_tests_teacher_effectiveness == 1, "Yes", 
                ifelse(df$teacher_tests_teacher_effectiveness == 2, "No", NA))),
  levels = c("Yes", "No", "valid_skip"),
  labels = c("Yes", "No", "valid_skip")
)

# Convert standardized_tests_instruction_improvement to factor with "valid_skip" for value 95
df$standardized_tests_instruction_improvement <- factor(
  ifelse(df$standardized_tests_instruction_improvement == 95, "valid_skip", 
         ifelse(df$standardized_tests_instruction_improvement == 1, "Yes", 
                ifelse(df$standardized_tests_instruction_improvement == 2, "No", NA))),
  levels = c("Yes", "No", "valid_skip"),
  labels = c("Yes", "No", "valid_skip")
)

# Convert teacher_tests_instruction_improvement to factor with "valid_skip" for value 95
df$teacher_tests_instruction_improvement <- factor(
  ifelse(df$teacher_tests_instruction_improvement == 95, "valid_skip", 
         ifelse(df$teacher_tests_instruction_improvement == 1, "Yes", 
                ifelse(df$teacher_tests_instruction_improvement == 2, "No", NA))),
  levels = c("Yes", "No", "valid_skip"),
  labels = c("Yes", "No", "valid_skip")
)

# Convert standardized_tests_adapt_needs to factor with "valid_skip" for value 95
df$standardized_tests_adapt_needs <- factor(
  ifelse(df$standardized_tests_adapt_needs == 95, "valid_skip", 
         ifelse(df$standardized_tests_adapt_needs == 1, "Yes", 
                ifelse(df$standardized_tests_adapt_needs == 2, "No", NA))),
  levels = c("Yes", "No", "valid_skip"),
  labels = c("Yes", "No", "valid_skip")
)

# Convert teacher_tests_adapt_needs to factor with "valid_skip" for value 95
df$teacher_tests_adapt_needs <- factor(
  ifelse(df$teacher_tests_adapt_needs == 95, "valid_skip", 
         ifelse(df$teacher_tests_adapt_needs == 1, "Yes", 
                ifelse(df$teacher_tests_adapt_needs == 2, "No", NA))),
  levels = c("Yes", "No", "valid_skip"),
  labels = c("Yes", "No", "valid_skip")
)

# Convert standardized_tests_compare_schools to factor with "valid_skip" for value 95
df$standardized_tests_compare_schools <- factor(
  ifelse(df$standardized_tests_compare_schools == 95, "valid_skip", 
         ifelse(df$standardized_tests_compare_schools == 1, "Yes", 
                ifelse(df$standardized_tests_compare_schools == 2, "No", NA))),
  levels = c("Yes", "No", "valid_skip"),
  labels = c("Yes", "No", "valid_skip")
)

# Convert teacher_tests_compare_schools to factor with "valid_skip" for value 95
df$teacher_tests_compare_schools <- factor(
  ifelse(df$teacher_tests_compare_schools == 95, "valid_skip", 
         ifelse(df$teacher_tests_compare_schools == 1, "Yes", 
                ifelse(df$teacher_tests_compare_schools == 2, "No", NA))),
  levels = c("Yes", "No", "valid_skip"),
  labels = c("Yes", "No", "valid_skip")
)

# Convert standardized_tests_award_certificates to factor with "valid_skip" for value 95
df$standardized_tests_award_certificates <- factor(
  ifelse(df$standardized_tests_award_certificates == 95, "valid_skip", 
         ifelse(df$standardized_tests_award_certificates == 1, "Yes", 
                ifelse(df$standardized_tests_award_certificates == 2, "No", NA))),
  levels = c("Yes", "No", "valid_skip"),
  labels = c("Yes", "No", "valid_skip")
)

# Convert teacher_tests_award_certificates to factor with "valid_skip" for value 95
df$teacher_tests_award_certificates <- factor(
  ifelse(df$teacher_tests_award_certificates == 95, "valid_skip", 
         ifelse(df$teacher_tests_award_certificates == 1, "Yes", 
                ifelse(df$teacher_tests_award_certificates == 2, "No", NA))),
  levels = c("Yes", "No", "valid_skip"),
  labels = c("Yes", "No", "valid_skip")
)

# Example for one variable: Convert student_grouped_by_ability to ordered factor
df$student_grouped_by_ability <- factor(df$student_grouped_by_ability, 
       levels = c(1, 2, 3), 
       labels = c("For all subjects", 
                  "For some subjects", 
                  "Not for any subjects"), 
       ordered = TRUE)

# Convert student_grouped_within_classes to ordered factor
df$student_grouped_within_classes <- factor(df$student_grouped_within_classes, 
       levels = c(1, 2, 3), 
       labels = c("For all subjects", 
                  "For some subjects", 
                  "Not for any subjects"), 
       ordered = TRUE)

# Convert math_classes_similar_content to ordered factor
df$math_classes_similar_content <- factor(df$math_classes_similar_content, 
       levels = c(1, 2, 3), 
       labels = c("For all subjects", 
                  "For some subjects", 
                  "Not for any subjects"), 
       ordered = TRUE)

# Convert math_classes_different_content to ordered factor
df$math_classes_different_content <- factor(df$math_classes_different_content, 
       levels = c(1, 2, 3), 
       labels = c("For all subjects", 
                  "For some subjects", 
                  "Not for any subjects"), 
       ordered = TRUE)

# Convert math_classes_grouped_ability to ordered factor
df$math_classes_grouped_ability <- factor(df$math_classes_grouped_ability, 
       levels = c(1, 2, 3), 
       labels = c("For all subjects", 
                  "For some subjects", 
                  "Not for any subjects"), 
       ordered = TRUE)

# Convert math_classes_mixed_ability to ordered factor
df$math_classes_mixed_ability <- factor(df$math_classes_mixed_ability, 
       levels = c(1, 2, 3), 
       labels = c("For all subjects", 
                  "For some subjects", 
                  "Not for any subjects"), 
       ordered = TRUE)

# Convert math_programme_local_standards to ordered factor
df$math_programme_local_standards <- factor(df$math_programme_local_standards, 
       levels = c(1, 2, 3, 4), 
       labels = c("Not at all", "Small extent", "Moderate extent", "Large extent"), 
       ordered = TRUE)

# Convert math_programme_regional_standards to ordered factor
df$math_programme_regional_standards <- factor(df$math_programme_regional_standards, 
       levels = c(1, 2, 3, 4), 
       labels = c("Not at all", "Small extent", "Moderate extent", "Large extent"), 
       ordered = TRUE)

# Convert math_programme_national_standards to ordered factor
df$math_programme_national_standards <- factor(df$math_programme_national_standards, 
       levels = c(1, 2, 3, 4), 
       labels = c("Not at all", "Small extent", "Moderate extent", "Large extent"), 
       ordered = TRUE)

# Convert math_programme_in_school_standards to ordered factor
df$math_programme_in_school_standards <- factor(df$math_programme_in_school_standards, 
       levels = c(1, 2, 3, 4), 
       labels = c("Not at all", "Small extent", "Moderate extent", "Large extent"), 
       ordered = TRUE)

# Convert math_programme_local_assessments to ordered factor
df$math_programme_local_assessments <- factor(df$math_programme_local_assessments, 
       levels = c(1, 2, 3, 4), 
       labels = c("Not at all", "Small extent", "Moderate extent", "Large extent"), 
       ordered = TRUE)

# Convert math_programme_regional_assessments to ordered factor
df$math_programme_regional_assessments <- factor(df$math_programme_regional_assessments, 
       levels = c(1, 2, 3, 4), 
       labels = c("Not at all", "Small extent", "Moderate extent", "Large extent"), 
       ordered = TRUE)

# Convert math_programme_national_assessments to ordered factor
df$math_programme_national_assessments <- factor(df$math_programme_national_assessments, 
       levels = c(1, 2, 3, 4), 
       labels = c("Not at all", "Small extent", "Moderate extent", "Large extent"), 
       ordered = TRUE)

# Convert math_programme_math_department to ordered factor
df$math_programme_math_department <- factor(df$math_programme_math_department, 
       levels = c(1, 2, 3, 4), 
       labels = c("Not at all", "Small extent", "Moderate extent", "Large extent"), 
       ordered = TRUE)

# Convert math_programme_teacher_discretion to ordered factor
df$math_programme_teacher_discretion <- factor(df$math_programme_teacher_discretion, 
       levels = c(1, 2, 3, 4), 
       labels = c("Not at all", "Small extent", "Moderate extent", "Large extent"), 
       ordered = TRUE)

# Convert math_programme_school_assessments to ordered factor
df$math_programme_school_assessments <- factor(df$math_programme_school_assessments, 
       levels = c(1, 2, 3, 4), 
       labels = c("Not at all", "Small extent", "Moderate extent", "Large extent"), 
       ordered = TRUE)

# Convert math_programme_commercial_programmes to ordered factor
df$math_programme_commercial_programmes <- factor(df$math_programme_commercial_programmes, 
       levels = c(1, 2, 3, 4), 
       labels = c("Not at all", "Small extent", "Moderate extent", "Large extent"), 
       ordered = TRUE)

# Example for one variable: Convert assessment_mandatory_math_tests to ordered factor
df$assessment_mandatory_math_tests <- factor(df$assessment_mandatory_math_tests, 
       levels = c(1, 2, 3, 4, 5), 
       labels = c("Never", "1-2 times a year", "3-5 times a year", "Monthly", "More than once a month"), 
       ordered = TRUE)

# Convert assessment_nonmandatory_math_tests to ordered factor
df$assessment_nonmandatory_math_tests <- factor(df$assessment_nonmandatory_math_tests, 
       levels = c(1, 2, 3, 4, 5), 
       labels = c("Never", "1-2 times a year", "3-5 times a year", "Monthly", "More than once a month"), 
       ordered = TRUE)

# Convert assessment_teacher_math_tests to ordered factor
df$assessment_teacher_math_tests <- factor(df$assessment_teacher_math_tests, 
       levels = c(1, 2, 3, 4, 5), 
       labels = c("Never", "1-2 times a year", "3-5 times a year", "Monthly", "More than once a month"), 
       ordered = TRUE)

# Convert assessment_teacher_math_ratings to ordered factor
df$assessment_teacher_math_ratings <- factor(df$assessment_teacher_math_ratings, 
       levels = c(1, 2, 3, 4, 5), 
       labels = c("Never", "1-2 times a year", "3-5 times a year", "Monthly", "More than once a month"), 
       ordered = TRUE)

# Convert math_achievement_public_post to factor (Yes/No)
df$math_achievement_public_post <- factor(df$math_achievement_public_post, 
       levels = c(1, 2), 
       labels = c("Yes", "No"))

# Convert math_achievement_tracked_admin to factor (Yes/No)
df$math_achievement_tracked_admin <- factor(df$math_achievement_tracked_admin, 
       levels = c(1, 2), 
       labels = c("Yes", "No"))

# Convert math_achievement_provided_parents to factor (Yes/No)
df$math_achievement_provided_parents <- factor(df$math_achievement_provided_parents, 
       levels = c(1, 2), 
       labels = c("Yes", "No"))

# Convert math_marks_above_pass to numeric
df$math_marks_above_pass <- as.numeric(df$math_marks_above_pass)

# Convert math_marks_below_pass to numeric
df$math_marks_below_pass <- as.numeric(df$math_marks_below_pass)

# Convert additional_math_lessons_offered to factor (Yes/No)
df$additional_math_lessons_offered <- factor(df$additional_math_lessons_offered, 
       levels = c(1, 2), 
       labels = c("Yes", "No"))

# Convert math_lessons_enrichment to factor with "valid_skip" for value 95
df$math_lessons_enrichment <- factor(
  ifelse(df$math_lessons_enrichment == 95, "valid_skip", 
         ifelse(df$math_lessons_enrichment == 1, "Yes", 
                ifelse(df$math_lessons_enrichment == 2, "No", NA))),
  levels = c("Yes", "No", "valid_skip"),
  labels = c("Yes", "No", "valid_skip")
)

# Convert math_lessons_remedial to factor with "valid_skip" for value 95
df$math_lessons_remedial <- factor(
  ifelse(df$math_lessons_remedial == 95, "valid_skip", 
         ifelse(df$math_lessons_remedial == 1, "Yes", 
                ifelse(df$math_lessons_remedial == 2, "No", NA))),
  levels = c("Yes", "No", "valid_skip"),
  labels = c("Yes", "No", "valid_skip")
)

# Convert policy_digital_devices_math_instruction to factor (Yes/No)
df$policy_digital_devices_math_instruction <- factor(df$policy_digital_devices_math_instruction, 
       levels = c(1, 2), 
       labels = c("Yes", "No"))

# Convert policy_standardized_math_curriculum to factor (Yes/No)
df$policy_standardized_math_curriculum <- factor(df$policy_standardized_math_curriculum, 
       levels = c(1, 2), 
       labels = c("Yes", "No"))

# Convert policy_social_emotional_skills_teaching to factor (Yes/No)
df$policy_social_emotional_skills_teaching <- factor(df$policy_social_emotional_skills_teaching, 
       levels = c(1, 2), 
       labels = c("Yes", "No"))

# Convert mission_social_emotional_skills to factor (Yes/No)
df$mission_social_emotional_skills <- factor(df$mission_social_emotional_skills, 
       levels = c(1, 2), 
       labels = c("Yes", "No"))

# Convert grading_social_emotional_skills to factor (Yes/No)
df$grading_social_emotional_skills <- factor(df$grading_social_emotional_skills, 
       levels = c(1, 2), 
       labels = c("Yes", "No"))

# Convert career_guidance_offered to factor (Yes/No)
df$career_guidance_offered <- factor(df$career_guidance_offered, 
       levels = c(1, 2), 
       labels = c("Yes", "No"))

# Convert student_situation_description to factor with "valid_skip" for value 95
df$student_situation_description <- factor(
  ifelse(df$student_situation_description == 95, "valid_skip", 
         ifelse(df$student_situation_description == 1, "Career guidance sought voluntarily", 
                ifelse(df$student_situation_description == 2, "Career guidance is formally scheduled into students’ time", NA))),
  levels = c("Career guidance sought voluntarily", "Career guidance is formally scheduled into students’ time", "valid_skip"),
  labels = c("Career guidance sought voluntarily", "Career guidance is formally scheduled into students’ time", "valid_skip"))

# Example for one variable: Convert info_internships to factor (Yes/No)
df$info_internships <- factor(df$info_internships, levels = c(1, 2), labels = c("Yes", "No"))

# Convert info_future_careers to factor (Yes/No)
df$info_future_careers <- factor(df$info_future_careers, levels = c(1, 2), labels = c("Yes", "No"))

# Convert info_edu_opportunities to factor (Yes/No)
df$info_edu_opportunities <- factor(df$info_edu_opportunities, levels = c(1, 2), labels = c("Yes", "No"))

# Convert school_days_closed_covid to numeric
df$school_days_closed_covid <- as.numeric(df$school_days_closed_covid)

# Convert school_days_closed_other to numeric
df$school_days_closed_other <- as.numeric(df$school_days_closed_other)

# Convert covid_remote_classes to ordered factor
df$covid_remote_classes <- factor(
  ifelse(df$covid_remote_classes == 95, "valid_skip", as.character(df$covid_remote_classes)),
  levels = c("1", "2", "3", "4", "5", "valid_skip"), 
  labels = c("None of the classes", "Less than half of the classes", "About half of the classes", 
             "More than half of the classes", "All or almost all of the classes", "valid_skip"),
  ordered = TRUE
)

# Convert covid_self_study_material to ordered factor
df$covid_self_study_material <- factor(
  ifelse(df$covid_self_study_material == 95, "valid_skip", as.character(df$covid_self_study_material)),
  levels = c("1", "2", "3", "4", "5", "valid_skip"),
  labels = c("None of the classes", "Less than half of the classes", "About half of the classes",
             "More than half of the classes", "All or almost all of the classes", "valid_skip"),
  ordered = TRUE
)

# Convert covid_classes_cancelled to ordered factor
df$covid_classes_cancelled <- factor(
  ifelse(df$covid_classes_cancelled == 95, "valid_skip", as.character(df$covid_classes_cancelled)),
  levels = c("1", "2", "3", "4", "5", "valid_skip"),
  labels = c("None of the classes", "Less than half of the classes", "About half of the classes",
             "More than half of the classes", "All or almost all of the classes", "valid_skip"),
  ordered = TRUE
)

# Convert covid_offer_digital_materials to factor (Yes/No)
df$covid_offer_digital_materials <- factor(
  ifelse(df$covid_offer_digital_materials == 95, "valid_skip", as.character(df$covid_offer_digital_materials)),
  levels = c("1", "2", "valid_skip"),
  labels = c("Yes", "No", "valid_skip")
)

# Convert covid_offer_real_time_lessons to factor (Yes/No)
df$covid_offer_real_time_lessons <- factor(
  ifelse(df$covid_offer_real_time_lessons == 95, "valid_skip", as.character(df$covid_offer_real_time_lessons)),
  levels = c("1", "2", "valid_skip"),
  labels = c("Yes", "No", "valid_skip")
)

# Convert covid_offer_recorded_lessons_school to factor (Yes/No)
df$covid_offer_recorded_lessons_school <- factor(
  ifelse(df$covid_offer_recorded_lessons_school == 95, "valid_skip", as.character(df$covid_offer_recorded_lessons_school)),
  levels = c("1", "2", "valid_skip"),
  labels = c("Yes", "No", "valid_skip")
)

# Convert covid_offer_recorded_lessons_outside to factor (Yes/No)
df$covid_offer_recorded_lessons_outside <- factor(
  ifelse(df$covid_offer_recorded_lessons_outside == 95, "valid_skip", as.character(df$covid_offer_recorded_lessons_outside)),
  levels = c("1", "2", "valid_skip"),
  labels = c("Yes", "No", "valid_skip")
)

# Convert covid_offer_special_resources to factor (Yes/No)
df$covid_offer_special_resources <- factor(
  ifelse(df$covid_offer_special_resources == 95, "valid_skip", as.character(df$covid_offer_special_resources)),
  levels = c("1", "2", "valid_skip"),
  labels = c("Yes", "No", "valid_skip")
)

# Convert covid_offer_heritage_language_resources to factor (Yes/No)
df$covid_offer_heritage_language_resources <- factor(
  ifelse(df$covid_offer_heritage_language_resources == 95, "valid_skip", as.character(df$covid_offer_heritage_language_resources)),
  levels = c("1", "2", "valid_skip"),
  labels = c("Yes", "No", "valid_skip")
)

# Convert covid_offer_tv_radio_lessons to factor (Yes/No)
df$covid_offer_tv_radio_lessons <- factor(
  ifelse(df$covid_offer_tv_radio_lessons == 95, "valid_skip", as.character(df$covid_offer_tv_radio_lessons)),
  levels = c("1", "2", "valid_skip"),
  labels = c("Yes", "No", "valid_skip")
)

# Convert covid_hindered_lack_student_devices to factor (Yes/No)
df$covid_hindered_lack_student_devices <- factor(
  ifelse(df$covid_hindered_lack_student_devices == 95, "valid_skip", as.character(df$covid_hindered_lack_student_devices)),
  levels = c("1", "2", "3", "4", "valid_skip"),
  labels = c("Not at all", "Very little", "To some extent", "A lot", "valid_skip"),
  ordered = TRUE
)

# Convert covid_hindered_lack_teacher_devices to ordered factor
df$covid_hindered_lack_teacher_devices <- factor(
  ifelse(df$covid_hindered_lack_teacher_devices == 95, "valid_skip", as.character(df$covid_hindered_lack_teacher_devices)),
  levels = c("1", "2", "3", "4", "valid_skip"),
  labels = c("Not at all", "Very little", "To some extent", "A lot", "valid_skip"),
  ordered = TRUE
)

# Convert covid_hindered_lack_student_internet to ordered factor
df$covid_hindered_lack_student_internet <- factor(
  ifelse(df$covid_hindered_lack_student_internet == 95, "valid_skip", as.character(df$covid_hindered_lack_student_internet)),
  levels = c("1", "2", "3", "4", "valid_skip"),
  labels = c("Not at all", "Very little", "To some extent", "A lot", "valid_skip"),
  ordered = TRUE
)

# Convert covid_hindered_lack_teacher_internet to ordered factor
df$covid_hindered_lack_teacher_internet <- factor(
  ifelse(df$covid_hindered_lack_teacher_internet == 95, "valid_skip", as.character(df$covid_hindered_lack_teacher_internet)),
  levels = c("1", "2", "3", "4", "valid_skip"),
  labels = c("Not at all", "Very little", "To some extent", "A lot", "valid_skip"),
  ordered = TRUE
)

# Convert covid_hindered_lack_lms to ordered factor
df$covid_hindered_lack_lms <- factor(
  ifelse(df$covid_hindered_lack_lms == 95, "valid_skip", as.character(df$covid_hindered_lack_lms)),
  levels = c("1", "2", "3", "4", "valid_skip"),
  labels = c("Not at all", "Very little", "To some extent", "A lot", "valid_skip"),
  ordered = TRUE
)

# Convert covid_hindered_lack_materials to ordered factor
df$covid_hindered_lack_materials <- factor(
  ifelse(df$covid_hindered_lack_materials == 95, "valid_skip", 
         ifelse(df$covid_hindered_lack_materials == 1, "Not at all", 
                ifelse(df$covid_hindered_lack_materials == 2, "Very little", 
                       ifelse(df$covid_hindered_lack_materials == 3, "To some extent", 
                              ifelse(df$covid_hindered_lack_materials == 4, "A lot", NA))))),
  levels = c("Not at all", "Very little", "To some extent", "A lot", "valid_skip"),
  labels = c("Not at all", "Very little", "To some extent", "A lot", "valid_skip"),
  ordered = TRUE
)

# Convert covid_hindered_communication_students to ordered factor
df$covid_hindered_communication_students <- factor(
  ifelse(df$covid_hindered_communication_students == 95, "valid_skip", 
         ifelse(df$covid_hindered_communication_students == 1, "Not at all", 
                ifelse(df$covid_hindered_communication_students == 2, "Very little", 
                       ifelse(df$covid_hindered_communication_students == 3, "To some extent", 
                              ifelse(df$covid_hindered_communication_students == 4, "A lot", NA))))),
  levels = c("Not at all", "Very little", "To some extent", "A lot", "valid_skip"),
  labels = c("Not at all", "Very little", "To some extent", "A lot", "valid_skip"),
  ordered = TRUE
)

# Convert covid_hindered_teacher_experience to ordered factor
df$covid_hindered_teacher_experience <- factor(
  ifelse(df$covid_hindered_teacher_experience == 95, "valid_skip", 
         ifelse(df$covid_hindered_teacher_experience == 1, "Not at all", 
                ifelse(df$covid_hindered_teacher_experience == 2, "Very little", 
                       ifelse(df$covid_hindered_teacher_experience == 3, "To some extent", 
                              ifelse(df$covid_hindered_teacher_experience == 4, "A lot", NA))))),
  levels = c("Not at all", "Very little", "To some extent", "A lot", "valid_skip"),
  labels = c("Not at all", "Very little", "To some extent", "A lot", "valid_skip"),
  ordered = TRUE
)

# Convert covid_teacher_email to factor (Yes/No)
df$covid_teacher_email <- factor(
  ifelse(df$covid_teacher_email == 95, "valid_skip", as.character(df$covid_teacher_email)),
  levels = c("1", "2", "valid_skip"),
  labels = c("Yes", "No", "valid_skip")
)

# Convert covid_teacher_virtual_meetings to factor (Yes/No)
df$covid_teacher_virtual_meetings <- factor(
  ifelse(as.character(df$covid_teacher_virtual_meetings) == "95", "valid_skip", 
         ifelse(as.character(df$covid_teacher_virtual_meetings) == "1", "Yes", 
                ifelse(as.character(df$covid_teacher_virtual_meetings) == "2", "No", NA))),
  levels = c("Yes", "No", "valid_skip"),
  labels = c("Yes", "No", "valid_skip")
)

# Convert covid_teacher_parent_help to factor (Yes/No)
df$covid_teacher_parent_help <- factor(
  ifelse(as.character(df$covid_teacher_parent_help) == "95", "valid_skip", 
         ifelse(as.character(df$covid_teacher_parent_help) == "1", "Yes", 
                ifelse(as.character(df$covid_teacher_parent_help) == "2", "No", NA))),
  levels = c("Yes", "No", "valid_skip"),
  labels = c("Yes", "No", "valid_skip")
)

# Convert covid_teacher_contact_parents to factor (Yes/No)
df$covid_teacher_contact_parents <- factor(
  ifelse(as.character(df$covid_teacher_contact_parents) == "95", "valid_skip", 
         ifelse(as.character(df$covid_teacher_contact_parents) == "1", "Yes", 
                ifelse(as.character(df$covid_teacher_contact_parents) == "2", "No", NA))),
  levels = c("Yes", "No", "valid_skip"),
  labels = c("Yes", "No", "valid_skip")
)

# Convert covid_teacher_info_parents to factor (Yes/No)
df$covid_teacher_info_parents <- factor(
  ifelse(as.character(df$covid_teacher_info_parents) == "95", "valid_skip", 
         ifelse(as.character(df$covid_teacher_info_parents) == "1", "Yes", 
                ifelse(as.character(df$covid_teacher_info_parents) == "2", "No", NA))),
  levels = c("Yes", "No", "valid_skip"),
  labels = c("Yes", "No", "valid_skip")
)

# Convert covid_teacher_track_assignments to factor (Yes/No)
df$covid_teacher_track_assignments <- factor(
  ifelse(as.character(df$covid_teacher_track_assignments) == "95", "valid_skip", 
         ifelse(as.character(df$covid_teacher_track_assignments) == "1", "Yes", 
                ifelse(as.character(df$covid_teacher_track_assignments) == "2", "No", NA))),
  levels = c("Yes", "No", "valid_skip"),
  labels = c("Yes", "No", "valid_skip")
)

# Convert covid_track_student_attendance to factor (Yes/No)
df$covid_track_student_attendance <- factor(
  ifelse(df$covid_track_student_attendance == 95, "valid_skip", 
         ifelse(df$covid_track_student_attendance == 1, "Yes", 
                ifelse(df$covid_track_student_attendance == 2, "No", NA))),
  levels = c("Yes", "No", "valid_skip"),
  labels = c("Yes", "No", "valid_skip")
)

# Convert covid_percent_attend_distance_learning to ordered factor (percentage ranges)
df$covid_percent_attend_distance_learning <- as.character(df$covid_percent_attend_distance_learning)
df$covid_percent_attend_distance_learning <- ifelse(df$covid_percent_attend_distance_learning == "95", "valid_skip", df$covid_percent_attend_distance_learning)
df$covid_percent_attend_distance_learning <- factor(
  df$covid_percent_attend_distance_learning,
  levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "valid_skip"),  # Remove 95 from the levels
  labels = c("0%", "1% to 10%", "11% to 20%", "21% to 30%", "31% to 40%", 
             "41% to 50%", "51% to 60%", "61% to 70%", "71% to 80%", 
             "81% to 90%", "91% to 100%", "valid_skip"),
  ordered = TRUE
)

# Convert covid_percent_no_attendance_distance_learning to ordered factor (percentage ranges)
df$covid_percent_no_attendance_distance_learning <- factor(
  ifelse(df$covid_percent_no_attendance_distance_learning == 95, "valid_skip", 
         as.character(df$covid_percent_no_attendance_distance_learning)),
  levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "valid_skip"),
  labels = c("0%", "1% to 10%", "11% to 20%", "21% to 30%", "31% to 40%", 
             "41% to 50%", "51% to 60%", "61% to 70%", "71% to 80%", 
             "81% to 90%", "91% to 100%", "valid_skip"),
  ordered = TRUE
)

# Convert covid_teacher_resources_meetings to factor (Yes/No)
df$covid_teacher_resources_meetings <- factor(
  ifelse(df$covid_teacher_resources_meetings == 95, "valid_skip", 
         ifelse(df$covid_teacher_resources_meetings == 1, "Yes", 
                ifelse(df$covid_teacher_resources_meetings == 2, "No", NA))),
  levels = c("Yes", "No", "valid_skip"),
  labels = c("Yes", "No", "valid_skip")
)

# Convert covid_teacher_resources_online_tools to factor (Yes/No)
df$covid_teacher_resources_online_tools <- factor(
  ifelse(df$covid_teacher_resources_online_tools == 95, "valid_skip", 
         ifelse(df$covid_teacher_resources_online_tools == 1, "Yes", 
                ifelse(df$covid_teacher_resources_online_tools == 2, "No", NA))),
  levels = c("Yes", "No", "valid_skip"),
  labels = c("Yes", "No", "valid_skip")
)

# Convert covid_teacher_resources_plan_tools to factor (Yes/No)
df$covid_teacher_resources_plan_tools <- factor(
  ifelse(df$covid_teacher_resources_plan_tools == 95, "valid_skip", 
         ifelse(df$covid_teacher_resources_plan_tools == 1, "Yes", 
                ifelse(df$covid_teacher_resources_plan_tools == 2, "No", NA))),
  levels = c("Yes", "No", "valid_skip"),
  labels = c("Yes", "No", "valid_skip")
)

# Convert covid_teacher_resources_prof_dev to factor (Yes/No)
df$covid_teacher_resources_prof_dev <- factor(
  ifelse(df$covid_teacher_resources_prof_dev == 95, "valid_skip", 
         ifelse(df$covid_teacher_resources_prof_dev == 1, "Yes", 
                ifelse(df$covid_teacher_resources_prof_dev == 2, "No", NA))),
  levels = c("Yes", "No", "valid_skip"),
  labels = c("Yes", "No", "valid_skip")
)

# Convert covid_support_national_authority to ordered factor
df$covid_support_national_authority <- factor(
  ifelse(df$covid_support_national_authority == 95, "valid_skip", 
         ifelse(df$covid_support_national_authority == 1, "Not at all", 
                ifelse(df$covid_support_national_authority == 2, "Very little", 
                       ifelse(df$covid_support_national_authority == 3, "To some extent", 
                              ifelse(df$covid_support_national_authority == 4, "A lot", NA))))),
  levels = c("Not at all", "Very little", "To some extent", "A lot", "valid_skip"),
  labels = c("Not at all", "Very little", "To some extent", "A lot", "valid_skip"),
  ordered = TRUE
)

# Convert covid_support_regional_authority to ordered factor
df$covid_support_regional_authority <- factor(
  ifelse(df$covid_support_regional_authority == 95, "valid_skip", 
         ifelse(df$covid_support_regional_authority == 1, "Not at all", 
                ifelse(df$covid_support_regional_authority == 2, "Very little", 
                       ifelse(df$covid_support_regional_authority == 3, "To some extent", 
                              ifelse(df$covid_support_regional_authority == 4, "A lot", NA))))),
  levels = c("Not at all", "Very little", "To some extent", "A lot", "valid_skip"),
  labels = c("Not at all", "Very little", "To some extent", "A lot", "valid_skip"),
  ordered = TRUE
)

# Convert covid_support_local_authority to ordered factor
df$covid_support_local_authority <- factor(
  ifelse(df$covid_support_local_authority == 95, "valid_skip", 
         ifelse(df$covid_support_local_authority == 1, "Not at all", 
                ifelse(df$covid_support_local_authority == 2, "Very little", 
                       ifelse(df$covid_support_local_authority == 3, "To some extent", 
                              ifelse(df$covid_support_local_authority == 4, "A lot", NA))))),
  levels = c("Not at all", "Very little", "To some extent", "A lot", "valid_skip"),
  labels = c("Not at all", "Very little", "To some extent", "A lot", "valid_skip"),
  ordered = TRUE
)

# Convert covid_support_parents to ordered factor
df$covid_support_parents <- factor(
  ifelse(df$covid_support_parents == 95, "valid_skip", 
         ifelse(df$covid_support_parents == 1, "Not at all", 
                ifelse(df$covid_support_parents == 2, "Very little", 
                       ifelse(df$covid_support_parents == 3, "To some extent", 
                              ifelse(df$covid_support_parents == 4, "A lot", NA))))),
  levels = c("Not at all", "Very little", "To some extent", "A lot", "valid_skip"),
  labels = c("Not at all", "Very little", "To some extent", "A lot", "valid_skip"),
  ordered = TRUE
)

# Convert covid_support_private_donors to ordered factor
df$covid_support_private_donors <- factor(
  ifelse(df$covid_support_private_donors == 95, "valid_skip", 
         ifelse(df$covid_support_private_donors == 1, "Not at all", 
                ifelse(df$covid_support_private_donors == 2, "Very little", 
                       ifelse(df$covid_support_private_donors == 3, "To some extent", 
                              ifelse(df$covid_support_private_donors == 4, "A lot", NA))))),
  levels = c("Not at all", "Very little", "To some extent", "A lot", "valid_skip"),
  labels = c("Not at all", "Very little", "To some extent", "A lot", "valid_skip"),
  ordered = TRUE
)

# Convert covid_prepare_teacher_training_video_tools to factor
df$covid_prepare_teacher_training_video_tools <- factor(
  ifelse(df$covid_prepare_teacher_training_video_tools == 95, "valid_skip", 
         ifelse(df$covid_prepare_teacher_training_video_tools == 1, "Yes, as a standard practice before COVID-19", 
                ifelse(df$covid_prepare_teacher_training_video_tools == 2, "Yes, in response to COVID-19", 
                       ifelse(df$covid_prepare_teacher_training_video_tools == 3, "No", NA)))),
  levels = c("Yes, as a standard practice before COVID-19", "Yes, in response to COVID-19", "No", "valid_skip"),
  labels = c("Yes, as a standard practice before COVID-19", "Yes, in response to COVID-19", "No", "valid_skip")
)

# Convert covid_prepare_student_training_video_tools to factor
df$covid_prepare_student_training_video_tools <- factor(
  ifelse(df$covid_prepare_student_training_video_tools == 95, "valid_skip", 
         ifelse(df$covid_prepare_student_training_video_tools == 1, "Yes, as a standard practice before COVID-19", 
                ifelse(df$covid_prepare_student_training_video_tools == 2, "Yes, in response to COVID-19", 
                       ifelse(df$covid_prepare_student_training_video_tools == 3, "No", NA)))),
  levels = c("Yes, as a standard practice before COVID-19", "Yes, in response to COVID-19", "No", "valid_skip"),
  labels = c("Yes, as a standard practice before COVID-19", "Yes, in response to COVID-19", "No", "valid_skip")
)

# Convert covid_prepare_digital_materials to factor
df$covid_prepare_digital_materials <- factor(
  ifelse(df$covid_prepare_digital_materials == 95, "valid_skip", 
         ifelse(df$covid_prepare_digital_materials == 1, "Yes, as a standard practice before COVID-19", 
                ifelse(df$covid_prepare_digital_materials == 2, "Yes, in response to COVID-19", 
                       ifelse(df$covid_prepare_digital_materials == 3, "No", NA)))),
  levels = c("Yes, as a standard practice before COVID-19", "Yes, in response to COVID-19", "No", "valid_skip"),
  labels = c("Yes, as a standard practice before COVID-19", "Yes, in response to COVID-19", "No", "valid_skip")
)

# Convert covid_prepare_paper_materials to factor
df$covid_prepare_paper_materials <- factor(
  ifelse(df$covid_prepare_paper_materials == 95, "valid_skip", 
         ifelse(df$covid_prepare_paper_materials == 1, "Yes, as a standard practice before COVID-19", 
                ifelse(df$covid_prepare_paper_materials == 2, "Yes, in response to COVID-19", 
                       ifelse(df$covid_prepare_paper_materials == 3, "No", NA)))),
  levels = c("Yes, as a standard practice before COVID-19", "Yes, in response to COVID-19", "No", "valid_skip"),
  labels = c("Yes, as a standard practice before COVID-19", "Yes, in response to COVID-19", "No", "valid_skip")
)

# Convert covid_prepare_adapt_curriculum to factor
df$covid_prepare_adapt_curriculum <- factor(
  ifelse(df$covid_prepare_adapt_curriculum == 95, "valid_skip", 
         ifelse(df$covid_prepare_adapt_curriculum == 1, "Yes, as a standard practice before COVID-19", 
                ifelse(df$covid_prepare_adapt_curriculum == 2, "Yes, in response to COVID-19", 
                       ifelse(df$covid_prepare_adapt_curriculum == 3, "No", NA)))),
  levels = c("Yes, as a standard practice before COVID-19", "Yes, in response to COVID-19", "No", "valid_skip"),
  labels = c("Yes, as a standard practice before COVID-19", "Yes, in response to COVID-19", "No", "valid_skip")
)

# Convert prepare_digital_assessments to factor
df$prepare_digital_assessments <- factor(
  ifelse(df$prepare_digital_assessments == 95, "valid_skip", 
         ifelse(df$prepare_digital_assessments == 1, "Yes, as a standard practice before COVID-19", 
                ifelse(df$prepare_digital_assessments == 2, "Yes, in response to COVID-19", 
                       ifelse(df$prepare_digital_assessments == 3, "No", NA)))),
  levels = c("Yes, as a standard practice before COVID-19", "Yes, in response to COVID-19", "No", "valid_skip"),
  labels = c("Yes, as a standard practice before COVID-19", "Yes, in response to COVID-19", "No", "valid_skip")
)

# Convert prepare_resources_for_parents to  factor
df$prepare_resources_for_parents <- factor(
  ifelse(df$prepare_resources_for_parents == 95, "valid_skip", 
         ifelse(df$prepare_resources_for_parents == 1, "Yes, as a standard practice before COVID-19", 
                ifelse(df$prepare_resources_for_parents == 2, "Yes, in response to COVID-19", 
                       ifelse(df$prepare_resources_for_parents == 3, "No", NA)))),
  levels = c("Yes, as a standard practice before COVID-19", "Yes, in response to COVID-19", "No", "valid_skip"),
  labels = c("Yes, as a standard practice before COVID-19", "Yes, in response to COVID-19", "No", "valid_skip")
)

# Convert prepare_student_access_devices to factor
df$prepare_student_access_devices <- factor(
  ifelse(df$prepare_student_access_devices == 95, "valid_skip", 
         ifelse(df$prepare_student_access_devices == 1, "Yes, as a standard practice before COVID-19", 
                ifelse(df$prepare_student_access_devices == 2, "Yes, in response to COVID-19", 
                       ifelse(df$prepare_student_access_devices == 3, "No", NA)))),
  levels = c("Yes, as a standard practice before COVID-19", "Yes, in response to COVID-19", "No", "valid_skip"),
  labels = c("Yes, as a standard practice before COVID-19", "Yes, in response to COVID-19", "No", "valid_skip")
)

# Convert prepare_teacher_access_resources to factor
df$prepare_teacher_access_resources <- factor(
  ifelse(df$prepare_teacher_access_resources == 95, "valid_skip", 
         ifelse(df$prepare_teacher_access_resources == 1, "Yes, as a standard practice before COVID-19", 
                ifelse(df$prepare_teacher_access_resources == 2, "Yes, in response to COVID-19", 
                       ifelse(df$prepare_teacher_access_resources == 3, "No", NA)))),
  levels = c("Yes, as a standard practice before COVID-19", "Yes, in response to COVID-19", "No", "valid_skip"),
  labels = c("Yes, as a standard practice before COVID-19", "Yes, in response to COVID-19", "No", "valid_skip")
)

# Convert prepare_plan_transition_remote to factor
df$prepare_plan_transition_remote <- factor(
  ifelse(df$prepare_plan_transition_remote == 95, "valid_skip", 
         ifelse(df$prepare_plan_transition_remote == 1, "Yes, as a standard practice before COVID-19", 
                ifelse(df$prepare_plan_transition_remote == 2, "Yes, in response to COVID-19", 
                       ifelse(df$prepare_plan_transition_remote == 3, "No", NA)))),
  levels = c("Yes, as a standard practice before COVID-19", "Yes, in response to COVID-19", "No", "valid_skip"),
  labels = c("Yes, as a standard practice before COVID-19", "Yes, in response to COVID-19", "No", "valid_skip")
)

# Example for one variable: Convert teacher_skills_digital_integration to ordered factor
df$teacher_skills_digital_integration <- factor(df$teacher_skills_digital_integration, 
      levels = c(1, 2, 3, 4), 
      labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
      ordered = TRUE)

# Convert teacher_time_digital_preparation to ordered factor
df$teacher_time_digital_preparation <- factor(df$teacher_time_digital_preparation, 
      levels = c(1, 2, 3, 4), 
      labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
      ordered = TRUE)

# Convert resources_prof_dev_digital to ordered factor
df$resources_prof_dev_digital <- factor(df$resources_prof_dev_digital, 
      levels = c(1, 2, 3, 4), 
      labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
      ordered = TRUE)

# Convert online_learning_support_availability to ordered factor
df$online_learning_support_availability <- factor(df$online_learning_support_availability, 
      levels = c(1, 2, 3, 4), 
      labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
      ordered = TRUE)

# Convert teacher_incentives_digital_integration to ordered factor
df$teacher_incentives_digital_integration <- factor(df$teacher_incentives_digital_integration, 
      levels = c(1, 2, 3, 4), 
      labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
      ordered = TRUE)

# Convert tech_support_availability to ordered factor
df$tech_support_availability <- factor(df$tech_support_availability, 
      levels = c(1, 2, 3, 4), 
      labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
      ordered = TRUE)

# Convert preparedness_remote_instruction to ordered factor
df$preparedness_remote_instruction <- factor(df$preparedness_remote_instruction, 
      levels = c(1, 2, 3, 4), 
      labels = c("Not prepared at all", "Not very prepared", "Well prepared", "Very well prepared"), 
      ordered = TRUE)

# ----------------------------
# Debugging Analysis (After Cleaning)
# ----------------------------
debug_after <- data.frame(
  Dataset = "After Cleaning",
  `95` = sum(df[, school_vars] == 95, na.rm = TRUE),  # Should be 0 if cleaned correctly
  valid_skip = sum(df[, school_vars] == "valid_skip", na.rm = TRUE),  # Count transformed valid_skip
  `97` = sum(df[, school_vars] == 97, na.rm = TRUE),  # Should be 0 if cleaned correctly
  `98` = sum(df[, school_vars] == 98, na.rm = TRUE),  # Should be 0 after transformation
  `99998` = sum(df[, school_vars] == 99998, na.rm = TRUE),  # Count raw 99998 values
  `99` = sum(df[, school_vars] == 99, na.rm = TRUE),  # Should be 0 after transformation
  Total_NA = sum(is.na(df[, school_vars]))  # Count total NA values
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
cat("✅ School Questionnaire Variable Cleaning Complete!\n")
cat("Cleaned data saved to:", output_path, "\n")
cat("Debugging analysis saved to:", debug_path, "\n")



# --------------------------------------
# PISA 2022 ICT Questionnaire Cleaning
# --------------------------------------
# File: 03_scripts/01_preparation/19_ict_questionnaire_cleaning.R
# Purpose:
# This script applies the implemented cleaning and transformation workflow used
# for PISA 2022 ICT questionnaire variables in the published Thailand random
# forest analysis.
#
# Main tasks:
# 1. Load the prior-step cleaned dataset.
# 2. Identify the ICT questionnaire variable block.
# 3. Apply variable-specific recoding, factor conversion, and selected mode imputation.
# 4. Produce debugging summaries before and after cleaning.
# 5. Save the cleaned ICT questionnaire dataset and debugging output.
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
input_path <- "PATH_TO_LOCAL_INPUT/pisa2022_cleaned_9_student_questionnaire_cleaning.rds"
output_path <- "PATH_TO_LOCAL_OUTPUT/pisa2022_cleaned_10_ict_questionnaire_cleaning.csv"
debug_path <- "PATH_TO_LOCAL_OUTPUT/pisa2022_cleaned_10_debugging_analysis.csv"

# Load dataset
df <- readRDS(input_path)

# ----------------------------
# Automatically Identify ICT Questionnaire Variables
# ----------------------------

# The implemented workflow identifies the ICT questionnaire block using
# the first and last renamed variables in the prepared dataset.
# Identify first and last ICT questionnaire variables
first_ict_var <- "school_use_desktop_laptop"  
last_ict_var <- "can_represent_solution_steps"  

# Get all column names between the first and last student questionnaire variable
if (first_ict_var %in% names(df) & last_ict_var %in% names(df)) {
  ict_vars <- names(df)[which(names(df) == first_ict_var):which(names(df) == last_ict_var)]
} else {
  ict_vars <- intersect(names(df), c(first_ict_var, last_ict_var)) # Ensure it doesn't break if one variable is missing
}

# Ensure that only variables that exist in df are processed
ict_vars <- intersect(ict_vars, names(df))

# ----------------------------
# Debugging Analysis (Before Cleaning)
# ----------------------------
debug_before <- data.frame(
  Dataset = "Before Cleaning",
  `95` = sum(df[, ict_vars] == 95, na.rm = TRUE),  # Count raw 95 values
  valid_skip = 0,  # To be filled after cleaning
  `97` = sum(df[, ict_vars] == 97, na.rm = TRUE),  # Count raw 97 values
  random_skip = 0,  # To be filled after cleaning
  `98` = sum(df[, ict_vars] == 98, na.rm = TRUE),  # Count raw 98 values
  `99` = sum(df[, ict_vars] == 99, na.rm = TRUE),  # Count raw 99 values
  Total_NA = sum(is.na(df[, ict_vars]))  # Count total NA values
)

# -----------------------------------------------------
# Manual Transformations for ICT Questionnaire Data
# -----------------------------------------------------

# Transform 'school_use_desktop_laptop': How often use at school: Desktop or laptop computer
df$school_use_desktop_laptop <- factor(ifelse(df$school_use_desktop_laptop == 99, NA, df$school_use_desktop_laptop), 
    levels = c(1, 2, 3, 4, 5, 6), 
    labels = c("Never", "Once/Twice a month", "Once/Twice a week", "Every day", "Several times a day", "Not available"), 
    ordered = TRUE)

# Transform 'school_use_smartphone': How often use at school: Smartphone
df$school_use_smartphone <- factor(ifelse(df$school_use_smartphone == 99, NA, df$school_use_smartphone), 
    levels = c(1, 2, 3, 4, 5, 6), 
    labels = c("Never", "Once/Twice a month", "Once/Twice a week", "Every day", "Several times a day", "Not available"), 
    ordered = TRUE)

# Transform 'school_use_tablet_ebook': How often use at school: Tablet device or e-book reader
df$school_use_tablet_ebook <- factor(ifelse(df$school_use_tablet_ebook == 99, NA, df$school_use_tablet_ebook), 
    levels = c(1, 2, 3, 4, 5, 6), 
    labels = c("Never", "Once/Twice a month", "Once/Twice a week", "Every day", "Several times a day", "Not available"), 
    ordered = TRUE)

# Transform 'school_use_internet': How often use at school: Internet access (except on smartphones)
df$school_use_internet <- factor(ifelse(df$school_use_internet == 99, NA, df$school_use_internet), 
    levels = c(1, 2, 3, 4, 5, 6), 
    labels = c("Never", "Once/Twice a month", "Once/Twice a week", "Every day", "Several times a day", "Not available"), 
    ordered = TRUE)

# Transform 'school_use_portal': How often use at school: School portal (to consult timetable, absences, etc.)
df$school_use_portal <- factor(ifelse(df$school_use_portal == 99, NA, df$school_use_portal), 
    levels = c(1, 2, 3, 4, 5, 6), 
    labels = c("Never", "Once/Twice a month", "Once/Twice a week", "Every day", "Several times a day", "Not available"), 
    ordered = TRUE)

# Transform 'school_use_educational_software': How often use at school: Educational software, games, or apps
df$school_use_educational_software <- factor(ifelse(df$school_use_educational_software == 99, NA, df$school_use_educational_software), 
    levels = c(1, 2, 3, 4, 5, 6), 
    labels = c("Never", "Once/Twice a month", "Once/Twice a week", "Every day", "Several times a day", "Not available"), 
    ordered = TRUE)

# Transform 'school_use_learning_platform': How often use at school: A learning management system or school learning platform
df$school_use_learning_platform <- factor(ifelse(df$school_use_learning_platform == 99, NA, df$school_use_learning_platform), 
    levels = c(1, 2, 3, 4, 5, 6), 
    labels = c("Never", "Once/Twice a month", "Once/Twice a week", "Every day", "Several times a day", "Not available"), 
    ordered = TRUE)

# Transform 'out_school_use_desktop_laptop': How often use out of school: Desktop or laptop computer
df$out_school_use_desktop_laptop <- factor(ifelse(df$out_school_use_desktop_laptop == 99, NA, df$out_school_use_desktop_laptop), 
    levels = c(1, 2, 3, 4, 5, 6), 
    labels = c("Never", "Once/Twice a month", "Once/Twice a week", "Every day", "Several times a day", "Not available"), 
    ordered = TRUE)

# Transform 'out_school_use_smartphone': How often use out of school: Smartphone
df$out_school_use_smartphone <- factor(ifelse(df$out_school_use_smartphone == 99, NA, df$out_school_use_smartphone), 
    levels = c(1, 2, 3, 4, 5, 6), 
    labels = c("Never", "Once/Twice a month", "Once/Twice a week", "Every day", "Several times a day", "Not available"), 
    ordered = TRUE)

# Transform 'out_school_use_tablet_ebook': How often use out of school: Tablet device or e-book reader
df$out_school_use_tablet_ebook <- factor(ifelse(df$out_school_use_tablet_ebook == 99, NA, df$out_school_use_tablet_ebook), 
    levels = c(1, 2, 3, 4, 5, 6), 
    labels = c("Never", "Once/Twice a month", "Once/Twice a week", "Every day", "Several times a day", "Not available"), 
    ordered = TRUE)

# Transform 'out_school_use_internet': How often use out of school: Internet access (except on smartphones) with mode imputation
mode_value <- as.integer(names(sort(table(df$out_school_use_internet), decreasing = TRUE)[1])) 
df$out_school_use_internet <- factor(ifelse(df$out_school_use_internet == 99, mode_value, df$out_school_use_internet), 
    levels = c(1, 2, 3, 4, 5, 6), 
    labels = c("Never", "Once/Twice a month", "Once/Twice a week", "Every day", "Several times a day", "Not available"), 
    ordered = TRUE)

# Transform 'out_school_use_educational_software': How often use out of school: Educational software, games, or apps
df$out_school_use_educational_software <- factor(ifelse(df$out_school_use_educational_software == 99, NA, df$out_school_use_educational_software), 
    levels = c(1, 2, 3, 4, 5, 6), 
    labels = c("Never", "Once/Twice a month", "Once/Twice a week", "Every day", "Several times a day", "Not available"), 
    ordered = TRUE)

# Transform 'out_school_use_video_games': How often use out of school: Video or online games
df$out_school_use_video_games <- factor(ifelse(df$out_school_use_video_games == 99, NA, df$out_school_use_video_games), 
    levels = c(1, 2, 3, 4, 5, 6), 
    labels = c("Never", "Once/Twice a month", "Once/Twice a week", "Every day", "Several times a day", "Not available"), 
    ordered = TRUE)

# Transform 'school_digital_resources_available': There are enough digital resources for every student at my school
df$school_digital_resources_available <- factor(ifelse(df$school_digital_resources_available == 99, NA, df$school_digital_resources_available), 
    levels = c(1, 2, 3, 4), 
    labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
    ordered = TRUE)

# Transform 'school_digital_devices_internet': There are enough digital devices with access to the Internet at my school
df$school_digital_devices_internet <- factor(ifelse(df$school_digital_devices_internet == 99, NA, df$school_digital_devices_internet), 
    levels = c(1, 2, 3, 4), 
    labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
    ordered = TRUE)

# Transform 'school_internet_speed': The school's Internet speed is sufficient
df$school_internet_speed <- factor(ifelse(df$school_internet_speed == 99, NA, df$school_internet_speed), 
    levels = c(1, 2, 3, 4), 
    labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
    ordered = TRUE)

# Transform 'school_digital_resources_functioning': Digital resources function properly at my school
df$school_digital_resources_functioning <- factor(ifelse(df$school_digital_resources_functioning == 99, NA, df$school_digital_resources_functioning), 
    levels = c(1, 2, 3, 4), 
    labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
    ordered = TRUE)

# Transform 'classroom_digital_accessibility': Digital resources are easily accessible within the classroom
df$classroom_digital_accessibility <- factor(ifelse(df$classroom_digital_accessibility == 99, NA, df$classroom_digital_accessibility), 
    levels = c(1, 2, 3, 4), 
    labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
    ordered = TRUE)

# Transform 'school_learning_resources_engagement': Digital learning resources available at my school make learning interesting
df$school_learning_resources_engagement <- factor(ifelse(df$school_learning_resources_engagement == 99, NA, df$school_learning_resources_engagement), 
    levels = c(1, 2, 3, 4), 
    labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
    ordered = TRUE)

# Transform 'school_technical_support': The school provides sufficient technical support to help students use digital resources
df$school_technical_support <- factor(ifelse(df$school_technical_support == 99, NA, df$school_technical_support), 
    levels = c(1, 2, 3, 4), 
    labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
    ordered = TRUE)

# Transform 'teacher_digital_skills': Teachers at my school have the necessary skills to use digital devices during instruction
df$teacher_digital_skills <- factor(ifelse(df$teacher_digital_skills == 99, NA, df$teacher_digital_skills), 
    levels = c(1, 2, 3, 4), 
    labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
    ordered = TRUE)

# Transform 'teacher_willingness_digital_resources': Teachers at my school are willing to use digital resources for teaching
df$teacher_willingness_digital_resources <- factor(ifelse(df$teacher_willingness_digital_resources == 99, NA, df$teacher_willingness_digital_resources), 
    levels = c(1, 2, 3, 4), 
    labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
    ordered = TRUE)

# Transform 'use_digital_language_subject': How often use digital resources in lessons in: Test language
df$use_digital_language_subject <- factor(ifelse(df$use_digital_language_subject == 99, NA, df$use_digital_language_subject), 
    levels = c(1, 2, 3, 4, 5, 6), 
    labels = c("Never", "Less than half", "About half", "More than half", "Every lesson", "No subject"), 
    ordered = TRUE)

# Transform 'use_digital_mathematics_subject': How often use digital resources in lessons in: Mathematics
df$use_digital_mathematics_subject <- factor(ifelse(df$use_digital_mathematics_subject == 99, NA, df$use_digital_mathematics_subject), 
    levels = c(1, 2, 3, 4, 5, 6), 
    labels = c("Never", "Less than half", "About half", "More than half", "Every lesson", "No subject"), 
    ordered = TRUE)

# Transform 'use_digital_science_subject': How often use digital resources in lessons in: Science
df$use_digital_science_subject <- factor(ifelse(df$use_digital_science_subject == 99, NA, df$use_digital_science_subject), 
    levels = c(1, 2, 3, 4, 5, 6), 
    labels = c("Never", "Less than half", "About half", "More than half", "Every lesson", "No subject"), 
    ordered = TRUE)

# Transform 'use_digital_computing_subject': How often use digital resources in lessons in: Computing
df$use_digital_computing_subject <- factor(ifelse(df$use_digital_computing_subject == 99, NA, df$use_digital_computing_subject), 
    levels = c(1, 2, 3, 4, 5, 6), 
    labels = c("Never", "Less than half", "About half", "More than half", "Every lesson", "No subject"), 
    ordered = TRUE)

# Transform 'digital_create_multimedia_presentation': How often used digital resources to create a multi-media presentation with mode imputation
mode_value <- as.integer(names(sort(table(df$digital_create_multimedia_presentation), decreasing = TRUE)[1])) 
df$digital_create_multimedia_presentation <- factor(ifelse(df$digital_create_multimedia_presentation == 99, mode_value, df$digital_create_multimedia_presentation), 
    levels = c(1, 2, 3, 4, 5), 
    labels = c("Never", "Once/Twice a year", "Once/Twice a month", "Once/Twice a week", "Every day"), 
    ordered = TRUE)

# Transform 'digital_write_edit_text': How often used digital resources to write or edit text for a school assignment
df$digital_write_edit_text <- factor(ifelse(df$digital_write_edit_text == 99, NA, df$digital_write_edit_text), 
    levels = c(1, 2, 3, 4, 5), 
    labels = c("Never", "Once/Twice a year", "Once/Twice a month", "Once/Twice a week", "Every day"), 
    ordered = TRUE)

# Transform 'digital_find_info_real_world': How often used digital resources to find information online about real-world problems
df$digital_find_info_real_world <- factor(ifelse(df$digital_find_info_real_world == 99, NA, df$digital_find_info_real_world), 
    levels = c(1, 2, 3, 4, 5), 
    labels = c("Never", "Once/Twice a year", "Once/Twice a month", "Once/Twice a week", "Every day"), 
    ordered = TRUE)

# Transform 'digital_collect_record_data': How often used digital resources to collect and record data
df$digital_collect_record_data <- factor(ifelse(df$digital_collect_record_data == 99, NA, df$digital_collect_record_data), 
    levels = c(1, 2, 3, 4, 5), 
    labels = c("Never", "Once/Twice a year", "Once/Twice a month", "Once/Twice a week", "Every day"), 
    ordered = TRUE)

# Transform 'digital_analyze_data': How often used digital resources to analyze data collected with mode imputation
mode_value <- as.integer(names(sort(table(df$digital_analyze_data), decreasing = TRUE)[1])) 
df$digital_analyze_data <- factor(ifelse(df$digital_analyze_data == 99, mode_value, df$digital_analyze_data), 
    levels = c(1, 2, 3, 4, 5), 
    labels = c("Never", "Once/Twice a year", "Once/Twice a month", "Once/Twice a week", "Every day"), 
    ordered = TRUE)

# Transform 'digital_report_share_results': How often used digital resources to report or share results from experiments
df$digital_report_share_results <- factor(ifelse(df$digital_report_share_results == 99, NA, df$digital_report_share_results), 
    levels = c(1, 2, 3, 4, 5), 
    labels = c("Never", "Once/Twice a year", "Once/Twice a month", "Once/Twice a week", "Every day"), 
    ordered = TRUE)

# Transform 'digital_plan_manage_projects': How often used digital resources to plan and manage work or projects
df$digital_plan_manage_projects <- factor(ifelse(df$digital_plan_manage_projects == 99, NA, df$digital_plan_manage_projects), 
    levels = c(1, 2, 3, 4, 5), 
    labels = c("Never", "Once/Twice a year", "Once/Twice a month", "Once/Twice a week", "Every day"), 
    ordered = TRUE)

# Transform 'digital_track_progress': How often used digital resources to track the progress of work projects
df$digital_track_progress <- factor(ifelse(df$digital_track_progress == 99, NA, df$digital_track_progress), 
    levels = c(1, 2, 3, 4, 5), 
    labels = c("Never", "Once/Twice a year", "Once/Twice a month", "Once/Twice a week", "Every day"), 
    ordered = TRUE)

# Transform 'digital_collaborate_create_content': How often used digital resources to collaborate with other students to create digital content
df$digital_collaborate_create_content <- factor(ifelse(df$digital_collaborate_create_content == 99, NA, df$digital_collaborate_create_content), 
    levels = c(1, 2, 3, 4, 5), 
    labels = c("Never", "Once/Twice a year", "Once/Twice a month", "Once/Twice a week", "Every day"), 
    ordered = TRUE)

# Transform 'digital_play_learning_games': How often used digital resources to play digital learning games
df$digital_play_learning_games <- factor(ifelse(df$digital_play_learning_games == 99, NA, df$digital_play_learning_games), 
    levels = c(1, 2, 3, 4, 5), 
    labels = c("Never", "Once/Twice a year", "Once/Twice a month", "Once/Twice a week", "Every day"), 
    ordered = TRUE)

# Transform 'digital_feedback_teacher': How often used digital resources to read or listen to feedback sent by teachers
df$digital_feedback_teacher <- factor(ifelse(df$digital_feedback_teacher == 99, NA, df$digital_feedback_teacher), 
    levels = c(1, 2, 3, 4, 5), 
    labels = c("Never", "Once/Twice a year", "Once/Twice a month", "Once/Twice a week", "Every day"), 
    ordered = TRUE)

# Transform 'digital_feedback_peers': How often used digital resources to read or listen to feedback sent by other students
df$digital_feedback_peers <- factor(ifelse(df$digital_feedback_peers == 99, NA, df$digital_feedback_peers), 
    levels = c(1, 2, 3, 4, 5), 
    labels = c("Never", "Once/Twice a year", "Once/Twice a month", "Once/Twice a week", "Every day"), 
    ordered = TRUE)

# Transform 'digital_feedback_auto_generated': How often used digital resources to read or listen to feedback automatically generated by software
df$digital_feedback_auto_generated <- factor(ifelse(df$digital_feedback_auto_generated == 99, NA, df$digital_feedback_auto_generated), 
    levels = c(1, 2, 3, 4, 5), 
    labels = c("Never", "Once/Twice a year", "Once/Twice a month", "Once/Twice a week", "Every day"), 
    ordered = TRUE)

# Transform 'digital_practice_exercises_apps': How often used digital resources to practice exercises using educational apps
df$digital_practice_exercises_apps <- factor(ifelse(df$digital_practice_exercises_apps == 99, NA, df$digital_practice_exercises_apps), 
    levels = c(1, 2, 3, 4, 5), 
    labels = c("Never", "Once/Twice a year", "Once/Twice a month", "Once/Twice a week", "Every day"), 
    ordered = TRUE)

# Transform 'digital_view_grades_results': How often used digital resources to see grades or results from assignments
df$digital_view_grades_results <- factor(ifelse(df$digital_view_grades_results == 99, NA, df$digital_view_grades_results), 
    levels = c(1, 2, 3, 4, 5), 
    labels = c("Never", "Once/Twice a year", "Once/Twice a month", "Once/Twice a week", "Every day"), 
    ordered = TRUE)

# Transform 'digital_browse_schoolwork_info': How often used digital resources to browse the Internet for schoolwork
df$digital_browse_schoolwork_info <- factor(ifelse(df$digital_browse_schoolwork_info == 99, NA, df$digital_browse_schoolwork_info), 
    levels = c(1, 2, 3, 4, 5), 
    labels = c("Never", "Once/Twice a year", "Once/Twice a month", "Once/Twice a week", "Every day"), 
    ordered = TRUE)

# Transform 'digital_browse_lesson_followup': How often used digital resources to browse the Internet to follow up on lessons with mode imputation
mode_value <- as.integer(names(sort(table(df$digital_browse_lesson_followup), decreasing = TRUE)[1])) 
df$digital_browse_lesson_followup <- factor(ifelse(df$digital_browse_lesson_followup == 99, mode_value, df$digital_browse_lesson_followup), 
    levels = c(1, 2, 3, 4, 5), 
    labels = c("Never", "Once/Twice a year", "Once/Twice a month", "Once/Twice a week", "Every day"), 
    ordered = TRUE)

# Transform 'digital_receive_assignments_teacher': How often used digital resources to receive or download assignments
df$digital_receive_assignments_teacher <- factor(ifelse(df$digital_receive_assignments_teacher == 99, NA, df$digital_receive_assignments_teacher), 
    levels = c(1, 2, 3, 4, 5), 
    labels = c("Never", "Once/Twice a year", "Once/Twice a month", "Once/Twice a week", "Every day"), 
    ordered = TRUE)

# Transform 'digital_upload_work': How often used digital resources to upload work to a portal or platform
df$digital_upload_work <- factor(ifelse(df$digital_upload_work == 99, NA, df$digital_upload_work), 
    levels = c(1, 2, 3, 4, 5), 
    labels = c("Never", "Once/Twice a year", "Once/Twice a month", "Once/Twice a week", "Every day"), 
    ordered = TRUE)

# Transform 'digital_communicate_teacher': How often used digital resources to communicate with teachers with mode imputation
mode_value <- as.integer(names(sort(table(df$digital_communicate_teacher), decreasing = TRUE)[1])) 
df$digital_communicate_teacher <- factor(ifelse(df$digital_communicate_teacher == 99, mode_value, df$digital_communicate_teacher), 
    levels = c(1, 2, 3, 4, 5), 
    labels = c("Never", "Once/Twice a year", "Once/Twice a month", "Once/Twice a week", "Every day"), 
    ordered = TRUE)

# Transform 'digital_communicate_peers': How often used digital resources to communicate with other students
df$digital_communicate_peers <- factor(ifelse(df$digital_communicate_peers == 99, NA, df$digital_communicate_peers), 
    levels = c(1, 2, 3, 4, 5), 
    labels = c("Never", "Once/Twice a year", "Once/Twice a month", "Once/Twice a week", "Every day"), 
    ordered = TRUE)

# Transform 'digital_search_assignment_info': How often used digital resources to search for information on assignments
df$digital_search_assignment_info <- factor(ifelse(df$digital_search_assignment_info == 99, NA, df$digital_search_assignment_info), 
    levels = c(1, 2, 3, 4, 5), 
    labels = c("Never", "Once/Twice a year", "Once/Twice a month", "Once/Twice a week", "Every day"), 
    ordered = TRUE)
    
# Transform 'math_resource_simple_calculations': How often used digital resources for simple calculations
df$math_resource_simple_calculations <- factor(ifelse(df$math_resource_simple_calculations == 99, NA, df$math_resource_simple_calculations), 
    levels = c(1, 2, 3, 4, 5, 6), 
    labels = c("Never", "Once/Twice a year", "Once/Twice a month", "Once/Twice a week", "Every day", "Does not apply"), 
    ordered = TRUE)

# Transform 'math_resource_solve_equations': How often used digital resources to solve equations
df$math_resource_solve_equations <- factor(ifelse(df$math_resource_solve_equations == 99, NA, df$math_resource_solve_equations), 
    levels = c(1, 2, 3, 4, 5, 6), 
    labels = c("Never", "Once/Twice a year", "Once/Twice a month", "Once/Twice a week", "Every day", "Does not apply"), 
    ordered = TRUE)

# Transform 'digital_use_simulations_modelling': How often used digital resources for simulations and modelling (e.g., GeoGebra, NetLogo)
df$digital_use_simulations_modelling <- factor(ifelse(df$digital_use_simulations_modelling == 99, NA, df$digital_use_simulations_modelling), 
    levels = c(1, 2, 3, 4, 5, 6), 
    labels = c("Never", "Once/Twice a year", "Once/Twice a month", "Once/Twice a week", "Every day", "Does not apply"), 
    ordered = TRUE)

# Transform 'digital_use_coding_algorithms': How often used digital resources for coding or algorithm activities (e.g., using Scratch, etc.)
df$digital_use_coding_algorithms <- factor(ifelse(df$digital_use_coding_algorithms == 99, NA, df$digital_use_coding_algorithms), 
    levels = c(1, 2, 3, 4, 5, 6), 
    labels = c("Never", "Once/Twice a year", "Once/Twice a month", "Once/Twice a week", "Every day", "Does not apply"), 
    ordered = TRUE)

# Transform 'time_play_video_games': How much time spent playing video-games (smartphone, gaming console, or online platform/apps)
df$time_play_video_games <- factor(ifelse(df$time_play_video_games == 99, NA, df$time_play_video_games), 
    levels = c(1, 2, 3, 4, 5, 6), 
    labels = c("No time", "Less than 1 hour", "1-3 hours", "3-5 hours", "5-7 hours", "More than 7 hours"), 
    ordered = TRUE)

# Transform 'time_browse_social_networks': How much time spent browsing social networks (e.g., Instagram, Facebook) with mode imputation
mode_value <- as.integer(names(sort(table(df$time_browse_social_networks), decreasing = TRUE)[1])) 
df$time_browse_social_networks <- factor(ifelse(df$time_browse_social_networks == 99, mode_value, df$time_browse_social_networks), 
    levels = c(1, 2, 3, 4, 5, 6), 
    labels = c("No time", "Less than 1 hour", "1-3 hours", "3-5 hours", "5-7 hours", "More than 7 hours"), 
    ordered = TRUE)

# Transform 'time_browse_internet_for_fun': How much time spent browsing the Internet for fun (e.g., reading news, listening to podcasts, etc.)
df$time_browse_internet_for_fun <- factor(ifelse(df$time_browse_internet_for_fun == 99, NA, df$time_browse_internet_for_fun), 
    levels = c(1, 2, 3, 4, 5, 6), 
    labels = c("No time", "Less than 1 hour", "1-3 hours", "3-5 hours", "5-7 hours", "More than 7 hours"), 
    ordered = TRUE)

# Transform 'time_practical_info_online': How much time spent looking for practical information online (e.g., find a place, book tickets, etc.) with mode imputation
mode_value <- as.integer(names(sort(table(df$time_practical_info_online), decreasing = TRUE)[1])) 
df$time_practical_info_online <- factor(ifelse(df$time_practical_info_online == 99, mode_value, df$time_practical_info_online), 
    levels = c(1, 2, 3, 4, 5, 6), 
    labels = c("No time", "Less than 1 hour", "1-3 hours", "3-5 hours", "5-7 hours", "More than 7 hours"), 
    ordered = TRUE)

# Transform 'time_communicate_share_digital_content': How much time spent communicating and sharing digital content on social networks or platforms
df$time_communicate_share_digital_content <- factor(ifelse(df$time_communicate_share_digital_content == 99, NA, df$time_communicate_share_digital_content), 
    levels = c(1, 2, 3, 4, 5, 6), 
    labels = c("No time", "Less than 1 hour", "1-3 hours", "3-5 hours", "5-7 hours", "More than 7 hours"), 
    ordered = TRUE)

# Transform 'time_read_tutorials': How much time spent reading, listening to, or viewing informational materials (e.g., tutorials, podcasts)
df$time_read_tutorials <- factor(ifelse(df$time_read_tutorials == 99, NA, df$time_read_tutorials), 
    levels = c(1, 2, 3, 4, 5, 6), 
    labels = c("No time", "Less than 1 hour", "1-3 hours", "3-5 hours", "5-7 hours", "More than 7 hours"), 
    ordered = TRUE)

# Transform 'time_create_edit_digital_content': How much time spent creating or editing digital content (pictures, videos, music, programs)
df$time_create_edit_digital_content <- factor(ifelse(df$time_create_edit_digital_content == 99, NA, df$time_create_edit_digital_content), 
    levels = c(1, 2, 3, 4, 5, 6), 
    labels = c("No time", "Less than 1 hour", "1-3 hours", "3-5 hours", "5-7 hours", "More than 7 hours"), 
    ordered = TRUE)

# Transform 'digital_use_video_games': How much time using digital resources to play video-games with mode imputation
mode_value <- as.integer(names(sort(table(df$digital_use_video_games), decreasing = TRUE)[1])) 
df$digital_use_video_games <- factor(ifelse(df$digital_use_video_games == 99, mode_value, df$digital_use_video_games), 
    levels = c(1, 2, 3, 4, 5, 6), 
    labels = c("No time", "Less than 1 hour", "1-3 hours", "3-5 hours", "5-7 hours", "More than 7 hours"), 
    ordered = TRUE)

# Transform 'digital_use_social_networks': How much time using digital resources to browse social networks with mode imputation
mode_value <- as.integer(names(sort(table(df$digital_use_social_networks), decreasing = TRUE)[1])) 
df$digital_use_social_networks <- factor(ifelse(df$digital_use_social_networks == 99, mode_value, df$digital_use_social_networks), 
    levels = c(1, 2, 3, 4, 5, 6), 
    labels = c("No time", "Less than 1 hour", "1-3 hours", "3-5 hours", "5-7 hours", "More than 7 hours"), 
    ordered = TRUE)

# Transform 'digital_use_internet_for_fun': How much time using digital resources to browse the Internet for fun with mode imputation
mode_value <- as.integer(names(sort(table(df$digital_use_internet_for_fun), decreasing = TRUE)[1])) 
df$digital_use_internet_for_fun <- factor(ifelse(df$digital_use_internet_for_fun == 99, mode_value, df$digital_use_internet_for_fun), 
    levels = c(1, 2, 3, 4, 5, 6), 
    labels = c("No time", "Less than 1 hour", "1-3 hours", "3-5 hours", "5-7 hours", "More than 7 hours"), 
    ordered = TRUE)

# Transform 'digital_use_practical_info_online': How much time using digital resources to look for practical information online with mode imputation
mode_value <- as.integer(names(sort(table(df$digital_use_practical_info_online), decreasing = TRUE)[1])) 
df$digital_use_practical_info_online <- factor(ifelse(df$digital_use_practical_info_online == 99, mode_value, df$digital_use_practical_info_online), 
    levels = c(1, 2, 3, 4, 5, 6), 
    labels = c("No time", "Less than 1 hour", "1-3 hours", "3-5 hours", "5-7 hours", "More than 7 hours"), 
    ordered = TRUE)

# Transform 'digital_use_share_digital_content': How much time using digital resources to communicate and share digital content with mode imputation
mode_value <- as.integer(names(sort(table(df$digital_use_share_digital_content), decreasing = TRUE)[1])) 
df$digital_use_share_digital_content <- factor(ifelse(df$digital_use_share_digital_content == 99, mode_value, df$digital_use_share_digital_content), 
    levels = c(1, 2, 3, 4, 5, 6), 
    labels = c("No time", "Less than 1 hour", "1-3 hours", "3-5 hours", "5-7 hours", "More than 7 hours"), 
    ordered = TRUE)

# Transform 'digital_use_informational_materials': How much time using digital resources to read or view informational materials with mode imputation
mode_value <- as.integer(names(sort(table(df$digital_use_informational_materials), decreasing = TRUE)[1])) 
df$digital_use_informational_materials <- factor(ifelse(df$digital_use_informational_materials == 99, mode_value, df$digital_use_informational_materials), 
    levels = c(1, 2, 3, 4, 5, 6), 
    labels = c("No time", "Less than 1 hour", "1-3 hours", "3-5 hours", "5-7 hours", "More than 7 hours"), 
    ordered = TRUE)

# Transform 'digital_use_create_edit_content': How much time using digital resources to create or edit digital content with mode imputation
mode_value <- as.integer(names(sort(table(df$digital_use_create_edit_content), decreasing = TRUE)[1])) 
df$digital_use_create_edit_content <- factor(ifelse(df$digital_use_create_edit_content == 99, mode_value, df$digital_use_create_edit_content), 
    levels = c(1, 2, 3, 4, 5, 6), 
    labels = c("No time", "Less than 1 hour", "1-3 hours", "3-5 hours", "5-7 hours", "More than 7 hours"), 
    ordered = TRUE)

# Transform 'rule_no_mobile_class': Agree/disagree: Students should not bring mobile phones to class
df$rule_no_mobile_class <- factor(ifelse(df$rule_no_mobile_class == 99, NA, df$rule_no_mobile_class), 
    levels = c(1, 2, 3, 4), 
    labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
    ordered = TRUE)

# Transform 'rule_no_laptops_class': Agree/disagree: Students should not bring laptops to class
df$rule_no_laptops_class <- factor(ifelse(df$rule_no_laptops_class == 99, NA, df$rule_no_laptops_class), 
    levels = c(1, 2, 3, 4), 
    labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
    ordered = TRUE)

# Transform 'rule_collaborate_device_use': Agree/disagree: Students should collaborate with teachers on rules for device use with mode imputation
mode_value <- as.integer(names(sort(table(df$rule_collaborate_device_use), decreasing = TRUE)[1])) 
df$rule_collaborate_device_use <- factor(ifelse(df$rule_collaborate_device_use == 99, mode_value, df$rule_collaborate_device_use), 
    levels = c(1, 2, 3, 4), 
    labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
    ordered = TRUE)

# Transform 'rule_filter_social_media_school': Agree/disagree: Schools should filter access to social media with mode imputation
mode_value <- as.integer(names(sort(table(df$rule_filter_social_media_school), decreasing = TRUE)[1])) 
df$rule_filter_social_media_school <- factor(ifelse(df$rule_filter_social_media_school == 99, mode_value, df$rule_filter_social_media_school), 
    levels = c(1, 2, 3, 4), 
    labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
    ordered = TRUE)

# Transform 'rule_filter_online_games_school': Agree/disagree: Schools should filter access to online games with mode imputation
mode_value <- as.integer(names(sort(table(df$rule_filter_online_games_school), decreasing = TRUE)[1])) 
df$rule_filter_online_games_school <- factor(ifelse(df$rule_filter_online_games_school == 99, mode_value, df$rule_filter_online_games_school), 
    levels = c(1, 2, 3, 4), 
    labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
    ordered = TRUE)

# Transform 'rule_monitor_student_devices': Agree/disagree: Teachers should monitor student laptop use with mode imputation
mode_value <- as.integer(names(sort(table(df$rule_monitor_student_devices), decreasing = TRUE)[1])) 
df$rule_monitor_student_devices <- factor(ifelse(df$rule_monitor_student_devices == 99, mode_value, df$rule_monitor_student_devices), 
    levels = c(1, 2, 3, 4), 
    labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
    ordered = TRUE)

# Transform 'trust_read_online': Agree/disagree: I trust what I read online with mode imputation
mode_value <- as.integer(names(sort(table(df$trust_read_online), decreasing = TRUE)[1])) 
df$trust_read_online <- factor(ifelse(df$trust_read_online == 99, mode_value, df$trust_read_online), 
    levels = c(1, 2, 3, 4), 
    labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
    ordered = TRUE)

# Transform 'compare_sources_online': Agree/disagree: I compare sources when searching for information online with mode imputation
mode_value <- as.integer(names(sort(table(df$compare_sources_online), decreasing = TRUE)[1])) 
df$compare_sources_online <- factor(ifelse(df$compare_sources_online == 99, mode_value, df$compare_sources_online), 
    levels = c(1, 2, 3, 4), 
    labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
    ordered = TRUE)

# Transform 'check_online_info_accuracy': Agree/disagree: I check the accuracy of online information before sharing with mode imputation
mode_value <- as.integer(names(sort(table(df$check_online_info_accuracy), decreasing = TRUE)[1])) 
df$check_online_info_accuracy <- factor(ifelse(df$check_online_info_accuracy == 99, mode_value, df$check_online_info_accuracy), 
    levels = c(1, 2, 3, 4), 
    labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
    ordered = TRUE)

# Transform 'discuss_info_with_teachers': Agree/disagree: I discuss online information accuracy with teachers with mode imputation
mode_value <- as.integer(names(sort(table(df$discuss_info_with_teachers), decreasing = TRUE)[1])) 
df$discuss_info_with_teachers <- factor(ifelse(df$discuss_info_with_teachers == 99, mode_value, df$discuss_info_with_teachers), 
    levels = c(1, 2, 3, 4), 
    labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
    ordered = TRUE)

# Transform 'discuss_info_with_peers': Agree/disagree: I discuss online information accuracy with peers with mode imputation
mode_value <- as.integer(names(sort(table(df$discuss_info_with_peers), decreasing = TRUE)[1])) 
df$discuss_info_with_peers <- factor(ifelse(df$discuss_info_with_peers == 99, mode_value, df$discuss_info_with_peers), 
    levels = c(1, 2, 3, 4), 
    labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
    ordered = TRUE)

# Transform 'discuss_info_with_parents': Agree/disagree: I discuss online information accuracy with parents with mode imputation
mode_value <- as.integer(names(sort(table(df$discuss_info_with_parents), decreasing = TRUE)[1])) 
df$discuss_info_with_parents <- factor(ifelse(df$discuss_info_with_parents == 99, mode_value, df$discuss_info_with_parents), 
    levels = c(1, 2, 3, 4), 
    labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
    ordered = TRUE)

# Transform 'flag_wrong_info_online': Agree/disagree: I flag wrong information online with mode imputation
mode_value <- as.integer(names(sort(table(df$flag_wrong_info_online), decreasing = TRUE)[1])) 
df$flag_wrong_info_online <- factor(ifelse(df$flag_wrong_info_online == 99, mode_value, df$flag_wrong_info_online), 
    levels = c(1, 2, 3, 4), 
    labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
    ordered = TRUE)

# Transform 'share_madeup_info_online': Agree/disagree: I share made-up information online without flagging it with mode imputation
mode_value <- as.integer(names(sort(table(df$share_madeup_info_online), decreasing = TRUE)[1])) 
df$share_madeup_info_online <- factor(ifelse(df$share_madeup_info_online == 99, mode_value, df$share_madeup_info_online), 
    levels = c(1, 2, 3, 4), 
    labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
    ordered = TRUE)

# Transform 'upset_inappropriate_content': How upset were you when encountering inappropriate content for my age with mode imputation
mode_value <- as.integer(names(sort(table(df$upset_inappropriate_content), decreasing = TRUE)[1])) 
df$upset_inappropriate_content <- factor(ifelse(df$upset_inappropriate_content == 99, mode_value, df$upset_inappropriate_content), 
    levels = c(1, 2, 3, 4, 5), 
    labels = c("This did not happen to me", "Not at all upset", "A little upset", "Quite upset", "Very upset"), 
    ordered = TRUE)

# Transform 'upset_discriminatory_content': How upset were you when encountering discriminatory content online with mode imputation
mode_value <- as.integer(names(sort(table(df$upset_discriminatory_content), decreasing = TRUE)[1])) 
df$upset_discriminatory_content <- factor(ifelse(df$upset_discriminatory_content == 99, mode_value, df$upset_discriminatory_content), 
    levels = c(1, 2, 3, 4, 5), 
    labels = c("This did not happen to me", "Not at all upset", "A little upset", "Quite upset", "Very upset"), 
    ordered = TRUE)

# Transform 'upset_offensive_messages': How upset were you when receiving offensive messages, comments, or videos (no imputation needed)
df$upset_offensive_messages <- factor(ifelse(df$upset_offensive_messages == 99, NA, df$upset_offensive_messages), 
    levels = c(1, 2, 3, 4, 5), 
    labels = c("This did not happen to me", "Not at all upset", "A little upset", "Quite upset", "Very upset"), 
    ordered = TRUE)

# Transform 'upset_info_public_without_consent': How upset were you when information about you was displayed online without your consent with mode imputation
mode_value <- as.integer(names(sort(table(df$upset_info_public_without_consent), decreasing = TRUE)[1])) 
df$upset_info_public_without_consent <- factor(ifelse(df$upset_info_public_without_consent == 99, mode_value, df$upset_info_public_without_consent), 
    levels = c(1, 2, 3, 4, 5), 
    labels = c("This did not happen to me", "Not at all upset", "A little upset", "Quite upset", "Very upset"), 
    ordered = TRUE)

# Transform 'interest_learn_digital_resources': Agree/disagree: I am interested in learning about digital resources with mode imputation
mode_value <- as.integer(names(sort(table(df$interest_learn_digital_resources), decreasing = TRUE)[1])) 
df$interest_learn_digital_resources <- factor(ifelse(df$interest_learn_digital_resources == 99, mode_value, df$interest_learn_digital_resources), 
    levels = c(1, 2, 3, 4), 
    labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
    ordered = TRUE)

# Transform 'interest_learn_programming': Agree/disagree: I am interested in learning computer programming with mode imputation
mode_value <- as.integer(names(sort(table(df$interest_learn_programming), decreasing = TRUE)[1])) 
df$interest_learn_programming <- factor(ifelse(df$interest_learn_programming == 99, mode_value, df$interest_learn_programming), 
    levels = c(1, 2, 3, 4), 
    labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
    ordered = TRUE)

# Transform 'interest_digital_useful_job': Agree/disagree: I am interested in digital resources for job purposes (no imputation needed)
df$interest_digital_useful_job <- factor(ifelse(df$interest_digital_useful_job == 99, NA, df$interest_digital_useful_job), 
    levels = c(1, 2, 3, 4), 
    labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
    ordered = TRUE)

# Transform 'can_search_find_relevant_info': Can do with digital resources: Search and find relevant information online with mode imputation
mode_value <- as.integer(names(sort(table(df$can_search_find_relevant_info), decreasing = TRUE)[1])) 
df$can_search_find_relevant_info <- factor(ifelse(df$can_search_find_relevant_info == 99, mode_value, df$can_search_find_relevant_info), 
    levels = c(1, 2, 3, 4, 5), 
    labels = c("I cannot do this", "I struggle to do this on my own", "I can do with a bit of effort", "I can easily do this", "I don't know what this is"), 
    ordered = TRUE)

# Transform 'can_assess_info_quality': Can do with digital resources: Assess the quality of online information with mode imputation
mode_value <- as.integer(names(sort(table(df$can_assess_info_quality), decreasing = TRUE)[1])) 
df$can_assess_info_quality <- factor(ifelse(df$can_assess_info_quality == 99, mode_value, df$can_assess_info_quality), 
    levels = c(1, 2, 3, 4, 5), 
    labels = c("I cannot do this", "I struggle to do this on my own", "I can do with a bit of effort", "I can easily do this", "I don't know what this is"), 
    ordered = TRUE)

# Transform 'can_share_info_group': Can do with digital resources: Share practical information with a group with mode imputation
mode_value <- as.integer(names(sort(table(df$can_share_info_group), decreasing = TRUE)[1])) 
df$can_share_info_group <- factor(ifelse(df$can_share_info_group == 99, mode_value, df$can_share_info_group), 
    levels = c(1, 2, 3, 4, 5), 
    labels = c("I cannot do this", "I struggle to do this on my own", "I can do with a bit of effort", "I can easily do this", "I don't know what this is"), 
    ordered = TRUE)

# Transform 'can_collaborate_group_assignment': Can do with digital resources: Collaborate on group assignments with mode imputation
mode_value <- as.integer(names(sort(table(df$can_collaborate_group_assignment), decreasing = TRUE)[1])) 
df$can_collaborate_group_assignment <- factor(ifelse(df$can_collaborate_group_assignment == 99, mode_value, df$can_collaborate_group_assignment), 
    levels = c(1, 2, 3, 4, 5), 
    labels = c("I cannot do this", "I struggle to do this on my own", "I can do with a bit of effort", "I can easily do this", "I don't know what this is"), 
    ordered = TRUE)

# Transform 'can_explain_share_content_online': Can do with digital resources: Explain how to share digital content with mode imputation
mode_value <- as.integer(names(sort(table(df$can_explain_share_content_online), decreasing = TRUE)[1])) 
df$can_explain_share_content_online <- factor(ifelse(df$can_explain_share_content_online == 99, mode_value, df$can_explain_share_content_online), 
    levels = c(1, 2, 3, 4, 5), 
    labels = c("I cannot do this", "I struggle to do this on my own", "I can do with a bit of effort", "I can easily do this", "I don't know what this is"), 
    ordered = TRUE)

# Transform 'can_write_edit_text': Can do with digital resources: Write or edit text for assignments (no imputation needed)
df$can_write_edit_text <- factor(ifelse(df$can_write_edit_text == 99, NA, df$can_write_edit_text), 
    levels = c(1, 2, 3, 4, 5), 
    labels = c("I cannot do this", "I struggle to do this on my own", "I can do with a bit of effort", "I can easily do this", "I don't know what this is"), 
    ordered = TRUE)

# Transform 'can_collect_record_data': Can do with digital resources: Collect and record data with mode imputation
mode_value <- as.integer(names(sort(table(df$can_collect_record_data), decreasing = TRUE)[1])) 
df$can_collect_record_data <- factor(ifelse(df$can_collect_record_data == 99, mode_value, df$can_collect_record_data), 
    levels = c(1, 2, 3, 4, 5), 
    labels = c("I cannot do this", "I struggle to do this on my own", "I can do with a bit of effort", "I can easily do this", "I don't know what this is"), 
    ordered = TRUE)

# Transform 'can_create_multimedia_presentation': Can do with digital resources: Create multimedia presentations with mode imputation
mode_value <- as.integer(names(sort(table(df$can_create_multimedia_presentation), decreasing = TRUE)[1])) 
df$can_create_multimedia_presentation <- factor(ifelse(df$can_create_multimedia_presentation == 99, mode_value, df$can_create_multimedia_presentation), 
    levels = c(1, 2, 3, 4, 5), 
    labels = c("I cannot do this", "I struggle to do this on my own", "I can do with a bit of effort", "I can easily do this", "I don't know what this is"), 
    ordered = TRUE)

# Transform 'can_create_update_blog': Can do with digital resources: Create, update, or maintain a blog with mode imputation
mode_value <- as.integer(names(sort(table(df$can_create_update_blog), decreasing = TRUE)[1])) 
df$can_create_update_blog <- factor(ifelse(df$can_create_update_blog == 99, mode_value, df$can_create_update_blog), 
    levels = c(1, 2, 3, 4, 5), 
    labels = c("I cannot do this", "I struggle to do this on my own", "I can do with a bit of effort", "I can easily do this", "I don't know what this is"), 
    ordered = TRUE)

# Transform 'can_protect_data_privacy': Can do with digital resources: Change device settings to protect privacy with mode imputation
mode_value <- as.integer(names(sort(table(df$can_protect_data_privacy), decreasing = TRUE)[1])) 
df$can_protect_data_privacy <- factor(ifelse(df$can_protect_data_privacy == 99, mode_value, df$can_protect_data_privacy), 
    levels = c(1, 2, 3, 4, 5), 
    labels = c("I cannot do this", "I struggle to do this on my own", "I can do with a bit of effort", "I can easily do this", "I don't know what this is"), 
    ordered = TRUE)

# Transform 'can_select_efficient_app': Can do with digital resources: Select the best app for tasks with mode imputation
mode_value <- as.integer(names(sort(table(df$can_select_efficient_app), decreasing = TRUE)[1])) 
df$can_select_efficient_app <- factor(ifelse(df$can_select_efficient_app == 99, mode_value, df$can_select_efficient_app), 
    levels = c(1, 2, 3, 4, 5), 
    labels = c("I cannot do this", "I struggle to do this on my own", "I can do with a bit of effort", "I can easily do this", "I don't know what this is"), 
    ordered = TRUE)

# Transform 'can_create_program_code': Can do with digital resources: Create a computer program with mode imputation
mode_value <- as.integer(names(sort(table(df$can_create_program_code), decreasing = TRUE)[1])) 
df$can_create_program_code <- factor(ifelse(df$can_create_program_code == 99, mode_value, df$can_create_program_code), 
    levels = c(1, 2, 3, 4, 5), 
    labels = c("I cannot do this", "I struggle to do this on my own", "I can do with a bit of effort", "I can easily do this", "I don't know what this is"), 
    ordered = TRUE)

# Transform 'can_identify_software_error': Can do with digital resources: Identify the source of software errors with mode imputation
mode_value <- as.integer(names(sort(table(df$can_identify_software_error), decreasing = TRUE)[1])) 
df$can_identify_software_error <- factor(ifelse(df$can_identify_software_error == 99, mode_value, df$can_identify_software_error), 
    levels = c(1, 2, 3, 4, 5), 
    labels = c("I cannot do this", "I struggle to do this on my own", "I can do with a bit of effort", "I can easily do this", "I don't know what this is"), 
    ordered = TRUE)

# Transform 'can_represent_solution_steps': Can do with digital resources: Break down a problem and represent a solution as a series of logical steps with mode imputation
mode_value <- as.integer(names(sort(table(df$can_represent_solution_steps), decreasing = TRUE)[1])) 
df$can_represent_solution_steps <- factor(ifelse(df$can_represent_solution_steps == 99, mode_value, df$can_represent_solution_steps), 
    levels = c(1, 2, 3, 4, 5), 
    labels = c("I cannot do this", "I struggle to do this on my own", "I can do with a bit of effort", "I can easily do this", "I don't know what this is"), 
    ordered = TRUE)

# ----------------------------
# Debugging Analysis (After Cleaning)
# ----------------------------
debug_after <- data.frame(
  Dataset = "After Cleaning",
  `95` = sum(df[, ict_vars] == 95, na.rm = TRUE),  # Should be 0 if cleaned correctly
  valid_skip = sum(df[, ict_vars] == "valid_skip", na.rm = TRUE),  # Count transformed valid_skip
  `97` = sum(df[, ict_vars] == 97, na.rm = TRUE),  # Should be 0 if cleaned correctly
  random_skip = sum(df[, ict_vars] == "random_skip", na.rm = TRUE),  # Count transformed random_skip
  `98` = sum(df[, ict_vars] == 98, na.rm = TRUE),  # Should be 0 after transformation
  `99` = sum(df[, ict_vars] == 99, na.rm = TRUE),  # Should be 0 after transformation
  Total_NA = sum(is.na(df[, ict_vars]))  # Count total NA values
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
save_cleaned(df, output_path)

# Save debugging summary (CSV only)
write_csv(debug_summary, debug_path)

# Completion Message
cat("✅ ICT Questionnaire Cleaning Complete!\n")
cat("✅ Cleaned data saved to:", output_path, "\n")
cat("✅ Debugging analysis saved to:", debug_path, "\n")


# --------------------------------------
# PISA 2022 Student Questionnaire Cleaning
# --------------------------------------
# File: 03_scripts/01_preparation/18_student_questionnaire_cleaning.R
# Purpose:
# This script applies the implemented cleaning and transformation workflow used
# for PISA 2022 student questionnaire variables in the published Thailand random
# forest analysis.
#
# Main tasks:
# 1. Load the prior-step cleaned dataset.
# 2. Identify the student questionnaire variable block.
# 3. Apply variable-specific recoding and transformation rules.
# 4. Produce debugging summaries before and after cleaning.
# 5. Save the cleaned student questionnaire dataset and debugging output.
#
# Public repository note:
# This script is shared to support transparency regarding the published analysis.
# It reflects the implemented workflow used in the study, but some intermediate
# inputs and broader private project resources are not publicly released.

# Load necessary libraries
library(dplyr)
library(readr)

# Define local file paths
# Note: update these paths to match your own local environment and available inputs.
input_path <- "PATH_TO_LOCAL_INPUT/pisa2022_cleaned_8_recalculated_ict_distress_online.csv"
output_path <- "PATH_TO_LOCAL_OUTPUT/pisa2022_cleaned_9_student_questionnaire_cleaning.csv"
debug_path <- "PATH_TO_LOCAL_OUTPUT/pisa2022_cleaned_9_debugging_analysis.csv"

# Load dataset
df <- read_csv(input_path, show_col_types = FALSE)

# ----------------------------
# Automatically Identify Student Questionnaire Variables
# ----------------------------

# The implemented workflow identifies the student questionnaire block using
# the first and last renamed variables in the prepared dataset.
# Identify first and last student questionnaire variables
first_student_var <- "student_grade_level"  
last_student_var <- "effort_accurate_pisa"  

# Get all column names between the first and last student questionnaire variable
if (first_student_var %in% names(df) & last_student_var %in% names(df)) {
  student_vars <- names(df)[which(names(df) == first_student_var):which(names(df) == last_student_var)]
} else {
  student_vars <- intersect(names(df), c(first_student_var, last_student_var)) # Ensure it doesn't break if one variable is missing
}

# Ensure that only variables that exist in df are processed
student_vars <- intersect(student_vars, names(df))

# ----------------------------
# Debugging Analysis (Before Cleaning)
# ----------------------------
debug_before <- data.frame(
  Dataset = "Before Cleaning",
  `95` = sum(df[, student_vars] == 95, na.rm = TRUE),  # Count raw 95 values
  valid_skip = 0,  # To be filled after cleaning
  `97` = sum(df[, student_vars] == 97, na.rm = TRUE),  # Count raw 97 values
  random_skip = 0,  # To be filled after cleaning
  `98` = sum(df[, student_vars] == 98, na.rm = TRUE),  # Count raw 98 values
  `99` = sum(df[, student_vars] == 99, na.rm = TRUE),  # Count raw 99 values
  Total_NA = sum(is.na(df[, student_vars]))  # Count total NA values
)

# -----------------------------------------------------
# Manual Transformations for Student Questionnaire Data
# -----------------------------------------------------

# Convert student grade level to an ordered factor
df$student_grade_level <- factor(df$student_grade_level, 
    levels = c(7, 8, 9, 10, 11), 
    labels = c("Grade 7", "Grade 8", "Grade 9", "Grade 10", "Grade 11"), 
    ordered = TRUE)

# Keep student birth year as numeric
df$student_birth_year <- as.numeric(df$student_birth_year)

# Convert student gender to a factor (Female/Male)
df$student_gender <- factor(df$student_gender, 
    levels = c(1, 2), 
    labels = c("Female", "Male"))

# Convert home devices variables (Yes/No) and handle missing (99 -> NA)
df$home_room <- factor(ifelse(df$home_room == 99, NA, df$home_room), 
    levels = c(1, 2), 
    labels = c("Yes", "No"))
        
df$home_computer <- factor(ifelse(df$home_computer == 99, NA, df$home_computer), 
    levels = c(1, 2), 
    labels = c("Yes", "No"))

df$home_educational_software <- factor(ifelse(df$home_educational_software == 99, NA, df$home_educational_software), 
    levels = c(1, 2), 
    labels = c("Yes", "No"))

df$home_smartphone <- factor(ifelse(df$home_smartphone == 99, NA, df$home_smartphone), 
    levels = c(1, 2), 
    labels = c("Yes", "No"))

df$home_internet <- factor(ifelse(df$home_internet == 99, NA, df$home_internet), 
    levels = c(1, 2), 
    labels = c("Yes", "No"))

df$home_smart_television <- factor(ifelse(df$home_smart_television == 99, NA, df$home_smart_television), 
    levels = c(1, 2), 
    labels = c("Yes", "No"))

df$home_air_conditioner <- factor(ifelse(df$home_air_conditioner == 99, NA, df$home_air_conditioner), 
    levels = c(1, 2, 3, 4), 
    labels = c("None", "One", "Two", "Three or more"), 
    ordered = TRUE)

# Convert ordered categorical variables (None to More than 10) and handle missing values (99 -> NA)
df$home_digital_devices <- factor(ifelse(df$home_digital_devices == 99, NA, df$home_digital_devices), 
                            levels = c(1, 2, 3, 4, 5, 6, 7, 8), 
                            labels = c("None", "One", "Two", "Three", "Four", "Five", "Six to ten", "More than ten"), 
                            ordered = TRUE)

# Convert ordered categorical variables (None to More than 5) and handle missing values (95 -> "valid_skip", 99 -> NA)
df$home_desktop_computers <- factor(ifelse(df$home_desktop_computers == 99, NA, 
                                           ifelse(df$home_desktop_computers == 95, "valid_skip", df$home_desktop_computers)), 
                            levels = c("valid_skip", 1, 2, 3, 4, 5), 
                            labels = c("valid_skip", "None", "One to two", "Three to five", "More than five", "I don’t know"), 
                            ordered = TRUE)

df$home_laptops <- factor(ifelse(df$home_laptops == 99, NA, 
                                 ifelse(df$home_laptops == 95, "valid_skip", df$home_laptops)), 
                            levels = c("valid_skip", 1, 2, 3, 4, 5), 
                            labels = c("valid_skip", "None", "One to two", "Three to five", "More than five", "I don’t know"), 
                            ordered = TRUE)

df$home_tablets <- factor(ifelse(df$home_tablets == 99, NA, 
                                 ifelse(df$home_tablets == 95, "valid_skip", df$home_tablets)), 
                            levels = c("valid_skip", 1, 2, 3, 4, 5), 
                            labels = c("valid_skip", "None", "One to two", "Three to five", "More than five", "I don’t know"), 
                            ordered = TRUE)

df$home_smartphones <- factor(ifelse(df$home_smartphones == 99, NA, 
                                     ifelse(df$home_smartphones == 95, "valid_skip", df$home_smartphones)), 
                              levels = c("valid_skip", 1, 2, 3, 4, 5), 
                              labels = c("valid_skip", "None", "One to two", "Three to five", "More than five", "I don’t know"), 
                              ordered = TRUE)

# Convert ordered categorical variable for total number of books
df$home_books_total <- factor(ifelse(df$home_books_total == 99, NA, df$home_books_total), 
                              levels = c(1, 2, 3, 4, 5, 6, 7), 
                              labels = c("There are no books.", "1-10 books", "11-25 books", 
                                         "26-100 books", "101-200 books", "201-500 books", 
                                         "More than 500 books"), 
                              ordered = TRUE)

# Convert ordered categorical variables for specific book categories
df$home_books_science <- factor(ifelse(df$home_books_science == 99, NA, 
                                       ifelse(df$home_books_science == 95, "valid_skip", df$home_books_science)), 
                                levels = c("valid_skip", 1, 2, 3, 4, 5), 
                                labels = c("valid_skip", "None", "One to five", "Six to ten", 
                                           "More than ten", "I don’t know"), 
                                ordered = TRUE)

df$home_books_technical <- factor(ifelse(df$home_books_technical == 99, NA, 
                                         ifelse(df$home_books_technical == 95, "valid_skip", df$home_books_technical)), 
                                  levels = c("valid_skip", 1, 2, 3, 4, 5), 
                                  labels = c("valid_skip", "None", "One to five", "Six to ten", 
                                             "More than ten", "I don’t know"), 
                                  ordered = TRUE)

df$home_books_dictionaries <- factor(ifelse(df$home_books_dictionaries == 99, NA, 
                                            ifelse(df$home_books_dictionaries == 95, "valid_skip", df$home_books_dictionaries)), 
                                     levels = c("valid_skip", 1, 2, 3, 4, 5), 
                                     labels = c("valid_skip", "None", "One to five", "Six to ten", 
                                                "More than ten", "I don’t know"), 
                                     ordered = TRUE)

df$home_books_school <- factor(ifelse(df$home_books_school == 99, NA, 
                                      ifelse(df$home_books_school == 95, "valid_skip", df$home_books_school)), 
                               levels = c("valid_skip", 1, 2, 3, 4, 5), 
                               labels = c("valid_skip", "None", "One to five", "Six to ten", 
                                          "More than ten", "I don’t know"), 
                               ordered = TRUE)

# Convert ordered categorical variable for mother's highest education level
df$mother_highest_education <- factor(ifelse(df$mother_highest_education == 99, "Don't know", df$mother_highest_education), 
                                      levels = c(5, 4, 3, 2, 1, "Don't know"), 
                                      labels = c("She did not complete ISCED level 1.", "ISCED level 1", "ISCED level 2", 
                                                 "ISCED level 3.3", "ISCED level 3.4", "Don't know"), 
                                      ordered = TRUE)

# Convert categorical binary variables for mother's ISCED qualifications (Levels 8, 7, 6, 5, 4)
df$mother_qualification_8 <- factor(ifelse(df$mother_qualification_8 == 99, "Don't know", df$mother_qualification_8), 
                                    levels = c(1, 2, "Don't know"), 
                                    labels = c("Yes", "No", "Don't know"))

df$mother_qualification_7 <- factor(ifelse(df$mother_qualification_7 == 99, "Don't know", df$mother_qualification_7), 
                                    levels = c(1, 2, "Don't know"), 
                                    labels = c("Yes", "No", "Don't know"))

df$mother_qualification_6 <- factor(ifelse(df$mother_qualification_6 == 99, "Don't know", df$mother_qualification_6), 
                                    levels = c(1, 2, "Don't know"), 
                                    labels = c("Yes", "No", "Don't know"))

df$mother_qualification_5 <- factor(ifelse(df$mother_qualification_5 == 99, "Don't know", df$mother_qualification_5), 
                                    levels = c(1, 2, "Don't know"), 
                                    labels = c("Yes", "No", "Don't know"))

# Convert ordered categorical variable for father's highest education level
df$father_highest_education <- factor(ifelse(df$father_highest_education == 99, "Don't know", df$father_highest_education), 
                                      levels = c(5, 4, 3, 2, 1, "Don't know"), 
                                      labels = c("He did not complete ISCED level 1.", "ISCED level 1", "ISCED level 2", 
                                                 "ISCED level 3.3", "ISCED level 3.4", "Don't know"), 
                                      ordered = TRUE)

# Convert categorical binary variables for father's ISCED qualifications (Levels 8, 7, 6, 5, 4)
df$father_qualification_8 <- factor(ifelse(df$father_qualification_8 == 99, "Don't know", df$father_qualification_8), 
                                    levels = c(1, 2, "Don't know"), 
                                    labels = c("Yes", "No", "Don't know"))

df$father_qualification_7 <- factor(ifelse(df$father_qualification_7 == 99, "Don't know", df$father_qualification_7), 
                                    levels = c(1, 2, "Don't know"), 
                                    labels = c("Yes", "No", "Don't know"))

df$father_qualification_6 <- factor(ifelse(df$father_qualification_6 == 99, "Don't know", df$father_qualification_6), 
                                    levels = c(1, 2, "Don't know"), 
                                    labels = c("Yes", "No", "Don't know"))

df$father_qualification_5 <- factor(ifelse(df$father_qualification_5 == 99, "Don't know", df$father_qualification_5), 
                                    levels = c(1, 2, "Don't know"), 
                                    labels = c("Yes", "No", "Don't know"))

# Convert family food insecurity to an ordered factor
df$family_food_insecurity <- factor(ifelse(df$family_food_insecurity == 99, NA, df$family_food_insecurity), 
                                    levels = c(1, 2, 3, 4, 5), 
                                    labels = c("Never or almost never", "About once a week", "2 to 3 times a week", 
                                               "4 to 5 times a week", "Every day or almost every day"), 
                                    ordered = TRUE)

# Convert family social status (now & future) to ordered factors (1-10 scale)
df$family_social_status_now <- factor(ifelse(df$family_social_status_now == 99, NA, df$family_social_status_now), 
                                      levels = 1:10, labels = as.character(1:10), ordered = TRUE)

df$family_social_status_future <- factor(ifelse(df$family_social_status_future == 99, NA, df$family_social_status_future), 
                                         levels = 1:10, labels = as.character(1:10), ordered = TRUE)

# Convert home language to categorical (Test language / Other language)
df$home_language <- factor(ifelse(df$home_language == 99, NA, df$home_language), 
                           levels = c(1, 2), 
                           labels = c("Language of the test", "Other language"))

# Convert school enrollment duration to an ordered factor
df$school_enrollment_duration <- factor(ifelse(df$school_enrollment_duration == 99, NA, df$school_enrollment_duration), 
                                        levels = c(1, 2, 3, 4, 5), 
                                        labels = c("Three or more school years, not including this school year", 
                                                   "Two school years, not including this school year", 
                                                   "One school year, not including this school year", 
                                                   "I came to this school at the start of this school year.", 
                                                   "I came to this school after the start of this school year."), 
                                        ordered = TRUE)

# Convert early education start age to a categorical factor
df$start_age_early_education <- factor(ifelse(df$start_age_early_education == 99, NA, df$start_age_early_education), 
                                       levels = c(1, 2, 3, 4, 5, 6, 7, 8), 
                                       labels = c("1 year or younger", "2 years", "3 years", "4 years", "5 years", 
                                                  "6 years or older", "I did not attend ISCED 0", "I do not remember"), 
                                       ordered = FALSE)

# Convert primary education start age to an ordered factor
df$start_age_primary_education <- factor(ifelse(df$start_age_primary_education == 99, NA, df$start_age_primary_education), 
                                         levels = c(1, 2, 3, 4, 5, 6, 7), 
                                         labels = c("3 or younger", "4", "5", "6", "7", "8", "9 or older"), 
                                         ordered = TRUE)

# Student repeated a grade (primary)
df$repeated_grade_primary <- factor(ifelse(df$repeated_grade_primary %in% c(98, 99), NA, df$repeated_grade_primary), 
                                    levels = c(1, 2, 3), 
                                    labels = c("No, never", "Yes, once", "Yes, twice or more"), 
                                    ordered = TRUE)

# Student repeated a grade (lower secondary)
df$repeated_grade_lower_secondary <- factor(ifelse(df$repeated_grade_lower_secondary %in% c(98, 99), NA, df$repeated_grade_lower_secondary), 
                                            levels = c(1, 2, 3), 
                                            labels = c("No, never", "Yes, once", "Yes, twice or more"), 
                                            ordered = TRUE)

# Student repeated a grade (upper secondary)
df$repeated_grade_upper_secondary <- factor(ifelse(df$repeated_grade_upper_secondary %in% c(98, 99), NA, df$repeated_grade_upper_secondary), 
                                            levels = c(1, 2, 3), 
                                            labels = c("No, never", "Yes, once", "Yes, twice or more"), 
                                            ordered = TRUE)

# Student missed school for 3+ months (primary)
df$missed_school_primary <- factor(ifelse(df$missed_school_primary == 99, NA, df$missed_school_primary), 
                                   levels = c(1, 2, 3), 
                                   labels = c("No, never", "Yes, once", "Yes, twice or more"), 
                                   ordered = TRUE)

# Student missed school for 3+ months (lower secondary)
df$missed_school_lower_secondary <- factor(ifelse(df$missed_school_lower_secondary == 99, NA, df$missed_school_lower_secondary), 
                                           levels = c(1, 2, 3), 
                                           labels = c("No, never", "Yes, once", "Yes, twice or more"), 
                                           ordered = TRUE)

# Student missed school for 3+ months (upper secondary)
df$missed_school_upper_secondary <- factor(ifelse(df$missed_school_upper_secondary == 99, NA, df$missed_school_upper_secondary), 
                                           levels = c(1, 2, 3), 
                                           labels = c("No, never", "Yes, once", "Yes, twice or more"), 
                                           ordered = TRUE)

# Transformations for school absence reasons
df$reason_school_missed_bored <- factor(ifelse(df$reason_school_missed_bored == 99, NA, df$reason_school_missed_bored), 
                                        levels = c(1, 2, 95), 
                                        labels = c("Yes", "No", "valid_skip"))

df$reason_school_missed_suspended <- factor(ifelse(df$reason_school_missed_suspended == 99, NA, df$reason_school_missed_suspended), 
                                            levels = c(1, 2, 95), 
                                            labels = c("Yes", "No", "valid_skip"))

df$reason_school_missed_transport <- factor(ifelse(df$reason_school_missed_transport == 99, NA, df$reason_school_missed_transport), 
                                            levels = c(1, 2, 95), 
                                            labels = c("Yes", "No", "valid_skip"))

df$reason_school_missed_family_care <- factor(ifelse(df$reason_school_missed_family_care == 99, NA, df$reason_school_missed_family_care), 
                                              levels = c(1, 2, 95), 
                                              labels = c("Yes", "No", "valid_skip"))

df$reason_school_missed_family_work <- factor(ifelse(df$reason_school_missed_family_work == 99, NA, df$reason_school_missed_family_work), 
                                              levels = c(1, 2, 95), 
                                              labels = c("Yes", "No", "valid_skip"))

df$reason_school_missed_earning <- factor(ifelse(df$reason_school_missed_earning == 99, NA, df$reason_school_missed_earning), 
                                          levels = c(1, 2, 95), 
                                          labels = c("Yes", "No", "valid_skip"))

df$reason_school_missed_sick <- factor(ifelse(df$reason_school_missed_sick == 99, NA, df$reason_school_missed_sick), 
                                       levels = c(1, 2, 95), 
                                       labels = c("Yes", "No", "valid_skip"))

df$reason_school_missed_safety <- factor(ifelse(df$reason_school_missed_safety == 99, NA, df$reason_school_missed_safety), 
                                         levels = c(1, 2, 95), 
                                         labels = c("Yes", "No", "valid_skip"))

df$reason_school_missed_fees <- factor(ifelse(df$reason_school_missed_fees == 99, NA, df$reason_school_missed_fees), 
                                       levels = c(1, 2, 95), 
                                       labels = c("Yes", "No", "valid_skip"))

df$reason_school_missed_disaster <- factor(ifelse(df$reason_school_missed_disaster == 99, NA, df$reason_school_missed_disaster), 
                                           levels = c(1, 2, 95), 
                                           labels = c("Yes", "No", "valid_skip"))

# Transform 'skipped_whole_day'
df$skipped_whole_day <- factor(ifelse(df$skipped_whole_day == 99, NA, df$skipped_whole_day), 
                               levels = c(1, 2, 3, 4), 
                               labels = c("Never", "One or two times", "Three or four times", "Five or more times"), 
                               ordered = TRUE)

# Transform 'skipped_classes'
df$skipped_classes <- factor(ifelse(df$skipped_classes == 99, NA, df$skipped_classes), 
                             levels = c(1, 2, 3, 4), 
                             labels = c("Never", "One or two times", "Three or four times", "Five or more times"), 
                             ordered = TRUE)

# Transform 'late_for_school'
df$late_for_school <- factor(ifelse(df$late_for_school == 99, NA, df$late_for_school), 
                             levels = c(1, 2, 3, 4), 
                             labels = c("Never", "One or two times", "Three or four times", "Five or more times"), 
                             ordered = TRUE)

# Transform 'teacher_respectful'
df$teacher_respectful <- factor(ifelse(df$teacher_respectful == 99, NA, 
                                       ifelse(df$teacher_respectful == 97, "random_skip", df$teacher_respectful)), 
                                levels = c("random_skip", 1, 2, 3, 4), 
                                labels = c("random_skip", "Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
                                ordered = TRUE)

# Transform 'teacher_concerned'
df$teacher_concerned <- factor(ifelse(df$teacher_concerned == 99, NA, 
                                      ifelse(df$teacher_concerned == 97, "random_skip", df$teacher_concerned)), 
                               levels = c("random_skip", 1, 2, 3, 4), 
                               labels = c("random_skip", "Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
                               ordered = TRUE)

# Transform 'teacher_receptive'
df$teacher_receptive <- factor(ifelse(df$teacher_receptive == 99, NA, 
                                      ifelse(df$teacher_receptive == 97, "random_skip", df$teacher_receptive)), 
                               levels = c("random_skip", 1, 2, 3, 4), 
                               labels = c("random_skip", "Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
                               ordered = TRUE)

# Transform 'teacher_intimidating'
df$teacher_intimidating <- factor(ifelse(df$teacher_intimidating == 99, NA, 
                                         ifelse(df$teacher_intimidating == 97, "random_skip", df$teacher_intimidating)), 
                                  levels = c("random_skip", 1, 2, 3, 4), 
                                  labels = c("random_skip", "Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
                                  ordered = TRUE)

# Transform 'teacher_inquisitive'
df$teacher_inquisitive <- factor(ifelse(df$teacher_inquisitive == 99, NA, 
                                        ifelse(df$teacher_inquisitive == 97, "random_skip", df$teacher_inquisitive)), 
                                 levels = c("random_skip", 1, 2, 3, 4), 
                                 labels = c("random_skip", "Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
                                 ordered = TRUE)

# Transform 'teacher_friendly'
df$teacher_friendly <- factor(ifelse(df$teacher_friendly == 99, NA, 
                                     ifelse(df$teacher_friendly == 97, "random_skip", df$teacher_friendly)), 
                              levels = c("random_skip", 1, 2, 3, 4), 
                              labels = c("random_skip", "Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
                              ordered = TRUE)

# Transform 'teacher_wellbeing_interest'
df$teacher_wellbeing_interest <- factor(ifelse(df$teacher_wellbeing_interest == 99, NA, 
                                               ifelse(df$teacher_wellbeing_interest == 97, "random_skip", df$teacher_wellbeing_interest)), 
                                        levels = c("random_skip", 1, 2, 3, 4), 
                                        labels = c("random_skip", "Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
                                        ordered = TRUE)

# Transform 'teacher_mean'
df$teacher_mean <- factor(ifelse(df$teacher_mean == 99, NA, 
                                 ifelse(df$teacher_mean == 97, "random_skip", df$teacher_mean)), 
                          levels = c("random_skip", 1, 2, 3, 4), 
                          labels = c("random_skip", "Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
                          ordered = TRUE)

# Convert student_feels_outsider: Agreement on feeling like an outsider at school
df$student_feels_outsider <- factor(
  ifelse(df$student_feels_outsider == 99, NA, 
         ifelse(df$student_feels_outsider == 97, "random_skip", df$student_feels_outsider)),
  levels = c("random_skip", 1, 2, 3, 4),
  labels = c("random_skip", "Strongly agree", "Agree", "Disagree", "Strongly disagree"),
  ordered = TRUE)

# Convert student_makes_friends: Agreement on ease of making friends at school
df$student_makes_friends <- factor(
  ifelse(df$student_makes_friends == 99, NA, 
         ifelse(df$student_makes_friends == 97, "random_skip", df$student_makes_friends)),
  levels = c("random_skip", 1, 2, 3, 4),
  labels = c("random_skip", "Strongly agree", "Agree", "Disagree", "Strongly disagree"),
  ordered = TRUE)

# Convert student_belongs: Agreement on feeling of belonging at school
df$student_belongs <- factor(
  ifelse(df$student_belongs == 99, NA, 
         ifelse(df$student_belongs == 97, "random_skip", df$student_belongs)),
  levels = c("random_skip", 1, 2, 3, 4),
  labels = c("random_skip", "Strongly agree", "Agree", "Disagree", "Strongly disagree"),
  ordered = TRUE)

# Convert student_feels_awkward: Agreement on feeling awkward or out of place at school
df$student_feels_awkward <- factor(
  ifelse(df$student_feels_awkward == 99, NA, 
         ifelse(df$student_feels_awkward == 97, "random_skip", df$student_feels_awkward)),
  levels = c("random_skip", 1, 2, 3, 4),
  labels = c("random_skip", "Strongly agree", "Agree", "Disagree", "Strongly disagree"),
  ordered = TRUE)

# Convert student_liked_by_others: Agreement on being liked by other students
df$student_liked_by_others <- factor(
  ifelse(df$student_liked_by_others == 99, NA, 
         ifelse(df$student_liked_by_others == 97, "random_skip", df$student_liked_by_others)),
  levels = c("random_skip", 1, 2, 3, 4),
  labels = c("random_skip", "Strongly agree", "Agree", "Disagree", "Strongly disagree"),
  ordered = TRUE)

# Convert student_feels_lonely: Agreement on feeling lonely at school
df$student_feels_lonely <- factor(
  ifelse(df$student_feels_lonely == 99, NA, 
         ifelse(df$student_feels_lonely == 97, "random_skip", df$student_feels_lonely)),
  levels = c("random_skip", 1, 2, 3, 4),
  labels = c("random_skip", "Strongly agree", "Agree", "Disagree", "Strongly disagree"),
  ordered = TRUE)

# Transform 'excluded_by_students': Frequency of being excluded by other students
df$excluded_by_students <- factor(ifelse(df$excluded_by_students == 99, NA, df$excluded_by_students), 
                                  levels = c(1, 2, 3, 4), 
                                  labels = c("Never or almost never", "A few times a year", "A few times a month", "Once a week or more"), 
                                  ordered = TRUE)

# Transform 'mocked_by_students': Frequency of being mocked by other students
df$mocked_by_students <- factor(ifelse(df$mocked_by_students == 99, NA, df$mocked_by_students), 
                                levels = c(1, 2, 3, 4), 
                                labels = c("Never or almost never", "A few times a year", "A few times a month", "Once a week or more"), 
                                ordered = TRUE)

# Transform 'threatened_by_students': Frequency of being threatened by other students
df$threatened_by_students <- factor(ifelse(df$threatened_by_students == 99, NA, df$threatened_by_students), 
                                    levels = c(1, 2, 3, 4), 
                                    labels = c("Never or almost never", "A few times a year", "A few times a month", "Once a week or more"), 
                                    ordered = TRUE)

# Transform 'belongings_destroyed': Frequency of belongings being taken or destroyed by students
df$belongings_destroyed <- factor(ifelse(df$belongings_destroyed == 99, NA, df$belongings_destroyed), 
                                  levels = c(1, 2, 3, 4), 
                                  labels = c("Never or almost never", "A few times a year", "A few times a month", "Once a week or more"), 
                                  ordered = TRUE)

# Transform 'physically_hurt': Frequency of being physically hurt by other students
df$physically_hurt <- factor(ifelse(df$physically_hurt == 99, NA, df$physically_hurt), 
                             levels = c(1, 2, 3, 4), 
                             labels = c("Never or almost never", "A few times a year", "A few times a month", "Once a week or more"), 
                             ordered = TRUE)

# Transform 'rumors_spread': Frequency of nasty rumors being spread about the student
df$rumors_spread <- factor(ifelse(df$rumors_spread == 99, NA, df$rumors_spread), 
                           levels = c(1, 2, 3, 4), 
                           labels = c("Never or almost never", "A few times a year", "A few times a month", "Once a week or more"), 
                           ordered = TRUE)

# Transform 'physical_fights_on_property': Frequency of student being in a physical fight on school property
df$physical_fights_on_property <- factor(ifelse(df$physical_fights_on_property == 99, NA, df$physical_fights_on_property), 
                                         levels = c(1, 2, 3, 4), 
                                         labels = c("Never or almost never", "A few times a year", "A few times a month", "Once a week or more"), 
                                         ordered = TRUE)

# Transform 'stayed_home_feeling_unsafe': Frequency of student staying home due to feeling unsafe
df$stayed_home_feeling_unsafe <- factor(ifelse(df$stayed_home_feeling_unsafe == 99, NA, df$stayed_home_feeling_unsafe), 
                                        levels = c(1, 2, 3, 4), 
                                        labels = c("Never or almost never", "A few times a year", "A few times a month", "Once a week or more"), 
                                        ordered = TRUE)

# Transform 'gave_money_threat': Frequency of giving money to someone at school due to threats
df$gave_money_threat <- factor(ifelse(df$gave_money_threat == 99, NA, df$gave_money_threat), 
                               levels = c(1, 2, 3, 4), 
                               labels = c("Never or almost never", "A few times a year", "A few times a month", "Once a week or more"), 
                               ordered = TRUE)

# Transform 'feel_safe_way_school': Agreement on feeling safe on the way to school
df$feel_safe_way_school <- factor(ifelse(df$feel_safe_way_school == 99, NA, df$feel_safe_way_school), 
                                  levels = c(1, 2, 3, 4), 
                                  labels = c("Strongly agree", "Agree", "Disagree", "Strongly disagree"), 
                                  ordered = TRUE)

# Transform 'feel_safe_way_home': Agreement on feeling safe on the way home from school
df$feel_safe_way_home <- factor(ifelse(df$feel_safe_way_home == 99, NA, df$feel_safe_way_home), 
                                levels = c(1, 2, 3, 4), 
                                labels = c("Strongly agree", "Agree", "Disagree", "Strongly disagree"), 
                                ordered = TRUE)

# Transform 'feel_safe_classroom': Agreement on feeling safe in classrooms at school
df$feel_safe_classroom <- factor(ifelse(df$feel_safe_classroom == 99, NA, df$feel_safe_classroom), 
                                 levels = c(1, 2, 3, 4), 
                                 labels = c("Strongly agree", "Agree", "Disagree", "Strongly disagree"), 
                                 ordered = TRUE)

# Transform 'feel_safe_other_places_school': Agreement on feeling safe in other places at school
df$feel_safe_other_places_school <- factor(ifelse(df$feel_safe_other_places_school == 99, NA, df$feel_safe_other_places_school), 
                                           levels = c(1, 2, 3, 4), 
                                           labels = c("Strongly agree", "Agree", "Disagree", "Strongly disagree"), 
                                           ordered = TRUE)

# Transform 'vandalism_school_4weeks': Occurrence of school vandalism in the past four weeks
df$vandalism_school_4weeks <- factor(ifelse(df$vandalism_school_4weeks == 99, NA, df$vandalism_school_4weeks), 
                                     levels = c(1, 2), 
                                     labels = c("Yes", "No"))

# Transform 'witnessed_fight_4weeks': Witnessed a fight on school property in the past four weeks
df$witnessed_fight_4weeks <- factor(ifelse(df$witnessed_fight_4weeks == 99, NA, df$witnessed_fight_4weeks), 
                                    levels = c(1, 2), 
                                    labels = c("Yes", "No"))

# Transform 'saw_gangs_school_4weeks': Saw gangs at school in the past four weeks
df$saw_gangs_school_4weeks <- factor(ifelse(df$saw_gangs_school_4weeks == 99, NA, df$saw_gangs_school_4weeks), 
                                     levels = c(1, 2), 
                                     labels = c("Yes", "No"))

# Transform 'student_threat_4weeks': Heard a student threaten to hurt another student in the past four weeks
df$student_threat_4weeks <- factor(ifelse(df$student_threat_4weeks == 99, NA, df$student_threat_4weeks), 
                                   levels = c(1, 2), 
                                   labels = c("Yes", "No"))

# Transform 'saw_weapon_4weeks': Saw a student carrying a gun or knife at school in the past four weeks
df$saw_weapon_4weeks <- factor(ifelse(df$saw_weapon_4weeks == 99, NA, df$saw_weapon_4weeks), 
                               levels = c(1, 2), 
                               labels = c("Yes", "No"))

# Transform 'study_before_school_days': Number of days per week studying before school
df$study_before_school_days <- factor(ifelse(df$study_before_school_days == 99, NA, df$study_before_school_days), 
                                      levels = c(1, 2, 3, 4, 5, 6), 
                                      labels = c("0 days", "1 day", "2 days", "3 days", "4 days", "5 or more days"), 
                                      ordered = TRUE)

# Transform 'work_household_before_school_days': Number of days per week working in the household before school
df$work_household_before_school_days <- factor(ifelse(df$work_household_before_school_days == 99, NA, df$work_household_before_school_days), 
                                               levels = c(1, 2, 3, 4, 5, 6), 
                                               labels = c("0 days", "1 day", "2 days", "3 days", "4 days", "5 or more days"), 
                                               ordered = TRUE)

# Transform 'work_for_pay_before_school_days': Number of days per week working for pay before school
df$work_for_pay_before_school_days <- factor(ifelse(df$work_for_pay_before_school_days == 99, NA, df$work_for_pay_before_school_days), 
                                             levels = c(1, 2, 3, 4, 5, 6), 
                                             labels = c("0 days", "1 day", "2 days", "3 days", "4 days", "5 or more days"), 
                                             ordered = TRUE)

# Transform 'study_after_school_days': Number of days per week studying after school
df$study_after_school_days <- factor(ifelse(df$study_after_school_days == 99, NA, df$study_after_school_days), 
                                     levels = c(1, 2, 3, 4, 5, 6), 
                                     labels = c("0 days", "1 day", "2 days", "3 days", "4 days", "5 or more days"), 
                                     ordered = TRUE)

# Transform 'work_household_after_school_days': Number of days per week working in the household after school
df$work_household_after_school_days <- factor(ifelse(df$work_household_after_school_days == 99, NA, df$work_household_after_school_days), 
                                              levels = c(1, 2, 3, 4, 5, 6), 
                                              labels = c("0 days", "1 day", "2 days", "3 days", "4 days", "5 or more days"), 
                                              ordered = TRUE)

# Transform 'work_for_pay_after_school_days': Number of days per week working for pay after school
df$work_for_pay_after_school_days <- factor(ifelse(df$work_for_pay_after_school_days == 99, NA, df$work_for_pay_after_school_days), 
                                            levels = c(1, 2, 3, 4, 5, 6), 
                                            labels = c("0 days", "1 day", "2 days", "3 days", "4 days", "5 or more days"), 
                                            ordered = TRUE)

# Transform 'digital_learning_school_hours': Hours per day using digital resources for learning at school
df$digital_learning_school_hours <- factor(ifelse(df$digital_learning_school_hours == 99, NA, df$digital_learning_school_hours), 
                                           levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9), 
                                           labels = c("None", "Up to 1 hour", "More than 1 hour and up to 2 hours", 
                                                      "More than 2 hours and up to 3 hours", "More than 3 hours and up to 4 hours", 
                                                      "More than 4 hours and up to 5 hours", "More than 5 hours and up to 6 hours", 
                                                      "More than 6 hours and up to 7 hours", "More than 7 hours"), 
                                           ordered = TRUE)

# Transform 'digital_learning_before_after_hours': Hours per day using digital resources for learning before and after school
df$digital_learning_before_after_hours <- factor(ifelse(df$digital_learning_before_after_hours == 99, NA, df$digital_learning_before_after_hours), 
                                                 levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9), 
                                                 labels = c("None", "Up to 1 hour", "More than 1 hour and up to 2 hours", 
                                                            "More than 2 hours and up to 3 hours", "More than 3 hours and up to 4 hours", 
                                                            "More than 4 hours and up to 5 hours", "More than 5 hours and up to 6 hours", 
                                                            "More than 6 hours and up to 7 hours", "More than 7 hours"), 
                                                 ordered = TRUE)

# Transform 'digital_learning_weekend_hours': Hours per day using digital resources for learning on weekends
df$digital_learning_weekend_hours <- factor(ifelse(df$digital_learning_weekend_hours == 99, NA, df$digital_learning_weekend_hours), 
                                            levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9), 
                                            labels = c("None", "Up to 1 hour", "More than 1 hour and up to 2 hours", 
                                                       "More than 2 hours and up to 3 hours", "More than 3 hours and up to 4 hours", 
                                                       "More than 4 hours and up to 5 hours", "More than 5 hours and up to 6 hours", 
                                                       "More than 6 hours and up to 7 hours", "More than 7 hours"), 
                                            ordered = TRUE)

# Transform 'digital_leisure_school_hours': Hours per day using digital resources for leisure at school
df$digital_leisure_school_hours <- factor(ifelse(df$digital_leisure_school_hours == 99, NA, df$digital_leisure_school_hours), 
                                          levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9), 
                                          labels = c("None", "Up to 1 hour", "More than 1 hour and up to 2 hours", 
                                                     "More than 2 hours and up to 3 hours", "More than 3 hours and up to 4 hours", 
                                                     "More than 4 hours and up to 5 hours", "More than 5 hours and up to 6 hours", 
                                                     "More than 6 hours and up to 7 hours", "More than 7 hours"), 
                                          ordered = TRUE)

# Transform 'digital_leisure_before_after_hours': Hours per day using digital resources for leisure before and after school
df$digital_leisure_before_after_hours <- factor(ifelse(df$digital_leisure_before_after_hours == 99, NA, df$digital_leisure_before_after_hours), 
                                                levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9), 
                                                labels = c("None", "Up to 1 hour", "More than 1 hour and up to 2 hours", 
                                                           "More than 2 hours and up to 3 hours", "More than 3 hours and up to 4 hours", 
                                                           "More than 4 hours and up to 5 hours", "More than 5 hours and up to 6 hours", 
                                                           "More than 6 hours and up to 7 hours", "More than 7 hours"), 
                                                ordered = TRUE)

# Transform 'digital_leisure_weekend_hours': Hours per day using digital resources for leisure on weekends
df$digital_leisure_weekend_hours <- factor(ifelse(df$digital_leisure_weekend_hours == 99, NA, df$digital_leisure_weekend_hours), 
                                           levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9), 
                                           labels = c("None", "Up to 1 hour", "More than 1 hour and up to 2 hours", 
                                                      "More than 2 hours and up to 3 hours", "More than 3 hours and up to 4 hours", 
                                                      "More than 4 hours and up to 5 hours", "More than 5 hours and up to 6 hours", 
                                                      "More than 6 hours and up to 7 hours", "More than 7 hours"), 
                                           ordered = TRUE)

# Transform 'notifications_off_class': How often student turns off notifications during class
df$notifications_off_class <- factor(ifelse(df$notifications_off_class %in% c(98, 99), NA, 
                                            ifelse(df$notifications_off_class == 97, "random_skip", df$notifications_off_class)), 
                                     levels = c("random_skip", 1, 2, 3, 4, 5), 
                                     labels = c("random_skip", "Never or almost never", "Less than half of the time", 
                                                "About half of the time", "More than half of the time", "All or almost all of the time"), 
                                     ordered = TRUE)

# Transform 'notifications_off_sleep': How often student turns off notifications when going to sleep
df$notifications_off_sleep <- factor(ifelse(df$notifications_off_sleep %in% c(98, 99), NA, 
                                            ifelse(df$notifications_off_sleep == 97, "random_skip", df$notifications_off_sleep)), 
                                     levels = c("random_skip", 1, 2, 3, 4, 5), 
                                     labels = c("random_skip", "Never or almost never", "Less than half of the time", 
                                                "About half of the time", "More than half of the time", "All or almost all of the time"), 
                                     ordered = TRUE)

# Transform 'device_near_home': How often student keeps their device near to answer messages at home
df$device_near_home <- factor(ifelse(df$device_near_home %in% c(98, 99), NA, 
                                     ifelse(df$device_near_home == 97, "random_skip", df$device_near_home)), 
                              levels = c("random_skip", 1, 2, 3, 4, 5), 
                              labels = c("random_skip", "Never or almost never", "Less than half of the time", 
                                         "About half of the time", "More than half of the time", "All or almost all of the time"), 
                              ordered = TRUE)

# Transform 'device_open_class': How often student has their device open in class for note-taking/searching
df$device_open_class <- factor(ifelse(df$device_open_class %in% c(98, 99), NA, 
                                      ifelse(df$device_open_class == 97, "random_skip", df$device_open_class)), 
                               levels = c("random_skip", 1, 2, 3, 4, 5), 
                               labels = c("random_skip", "Never or almost never", "Less than half of the time", 
                                          "About half of the time", "More than half of the time", "All or almost all of the time"), 
                               ordered = TRUE)

# Transform 'pressure_online_class': How often student feels pressured to be online and answer messages in class
df$pressure_online_class <- factor(ifelse(df$pressure_online_class %in% c(98, 99), NA, 
                                          ifelse(df$pressure_online_class == 97, "random_skip", df$pressure_online_class)), 
                                   levels = c("random_skip", 1, 2, 3, 4, 5), 
                                   labels = c("random_skip", "Never or almost never", "Less than half of the time", 
                                              "About half of the time", "More than half of the time", "All or almost all of the time"), 
                                   ordered = TRUE)

# Transform 'anxious_no_device': How often student feels nervous/anxious when their device is not near
df$anxious_no_device <- factor(ifelse(df$anxious_no_device %in% c(98, 99), NA, 
                                      ifelse(df$anxious_no_device == 97, "random_skip", df$anxious_no_device)), 
                               levels = c("random_skip", 1, 2, 3, 4, 5), 
                               labels = c("random_skip", "Never or almost never", "Less than half of the time", 
                                          "About half of the time", "More than half of the time", "All or almost all of the time"), 
                               ordered = TRUE)

# Transform 'persistence_task_finished': Agreement on keeping working on a task until it is finished
df$persistence_task_finished <- factor(ifelse(df$persistence_task_finished %in% c(98, 99), NA, 
                                              ifelse(df$persistence_task_finished == 97, "random_skip", df$persistence_task_finished)), 
                                       levels = c("random_skip", 1, 2, 3, 4, 5), 
                                       labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                  "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                       ordered = TRUE)

# Transform 'extra_effort_challenging': Agreement on applying additional effort when work becomes challenging
df$extra_effort_challenging <- factor(ifelse(df$extra_effort_challenging %in% c(98, 99), NA, 
                                             ifelse(df$extra_effort_challenging == 97, "random_skip", df$extra_effort_challenging)), 
                                      levels = c("random_skip", 1, 2, 3, 4, 5), 
                                      labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                 "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                      ordered = TRUE)

# Transform 'persistence_boring_task': Agreement on finishing tasks even when they become boring
df$persistence_boring_task <- factor(ifelse(df$persistence_boring_task %in% c(98, 99), NA, 
                                            ifelse(df$persistence_boring_task == 97, "random_skip", df$persistence_boring_task)), 
                                     levels = c("random_skip", 1, 2, 3, 4, 5), 
                                     labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                     ordered = TRUE)

# Transform 'stop_difficult_task': Agreement on stopping when work becomes too difficult
df$stop_difficult_task <- factor(ifelse(df$stop_difficult_task %in% c(98, 99), NA, 
                                        ifelse(df$stop_difficult_task == 97, "random_skip", df$stop_difficult_task)), 
                                 levels = c("random_skip", 1, 2, 3, 4, 5), 
                                 labels = c("random_skip", "Strongly disagree", "Disagree", 
                                            "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                 ordered = TRUE)

# Transform 'more_persistent_than_others': Agreement on being more persistent than most people
df$more_persistent_than_others <- factor(ifelse(df$more_persistent_than_others %in% c(98, 99), NA, 
                                                ifelse(df$more_persistent_than_others == 97, "random_skip", df$more_persistent_than_others)), 
                                         levels = c("random_skip", 1, 2, 3, 4, 5), 
                                         labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                    "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                         ordered = TRUE)

# Transform 'give_up_after_mistakes': Agreement on giving up after making mistakes
df$give_up_after_mistakes <- factor(ifelse(df$give_up_after_mistakes %in% c(98, 99), NA, 
                                           ifelse(df$give_up_after_mistakes == 97, "random_skip", df$give_up_after_mistakes)), 
                                    levels = c("random_skip", 1, 2, 3, 4, 5), 
                                    labels = c("random_skip", "Strongly disagree", "Disagree", 
                                               "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                    ordered = TRUE)

# Transform 'quit_long_homework': Agreement on quitting homework if it is too long
df$quit_long_homework <- factor(ifelse(df$quit_long_homework %in% c(98, 99), NA, 
                                       ifelse(df$quit_long_homework == 97, "random_skip", df$quit_long_homework)), 
                                levels = c("random_skip", 1, 2, 3, 4, 5), 
                                labels = c("random_skip", "Strongly disagree", "Disagree", 
                                           "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                ordered = TRUE)

# Transform 'persistence_difficult_task': Agreement on completing tasks even when they become more difficult
df$persistence_difficult_task <- factor(ifelse(df$persistence_difficult_task %in% c(98, 99), NA, 
                                               ifelse(df$persistence_difficult_task == 97, "random_skip", df$persistence_difficult_task)), 
                                        levels = c("random_skip", 1, 2, 3, 4, 5), 
                                        labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                   "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                        ordered = TRUE)

# Transform 'finish_what_start': Agreement on finishing what is started
df$finish_what_start <- factor(ifelse(df$finish_what_start %in% c(98, 99), NA, 
                                      ifelse(df$finish_what_start == 97, "random_skip", df$finish_what_start)), 
                               levels = c("random_skip", 1, 2, 3, 4, 5), 
                               labels = c("random_skip", "Strongly disagree", "Disagree", 
                                          "Neither agree nor disagree", "Agree", "Strongly agree"), 
                               ordered = TRUE)

# Transform 'give_up_easily': Agreement on giving up easily
df$give_up_easily <- factor(ifelse(df$give_up_easily %in% c(98, 99), NA, 
                                   ifelse(df$give_up_easily == 97, "random_skip", df$give_up_easily)), 
                            levels = c("random_skip", 1, 2, 3, 4, 5), 
                            labels = c("random_skip", "Strongly disagree", "Disagree", 
                                       "Neither agree nor disagree", "Agree", "Strongly agree"), 
                            ordered = TRUE)

# Transform 'easily_distracted': Agreement on getting easily distracted
df$easily_distracted <- factor(ifelse(df$easily_distracted == 99, NA, 
                                      ifelse(df$easily_distracted == 97, "random_skip", df$easily_distracted)), 
                               levels = c("random_skip", 1, 2, 3, 4, 5), 
                               labels = c("random_skip", "Strongly disagree", "Disagree", 
                                          "Neither agree nor disagree", "Agree", "Strongly agree"), 
                               ordered = TRUE)

# Transform 'say_first_thing': Agreement on saying the first thing that comes to mind
df$say_first_thing <- factor(ifelse(df$say_first_thing == 99, NA, 
                                    ifelse(df$say_first_thing == 97, "random_skip", df$say_first_thing)), 
                             levels = c("random_skip", 1, 2, 3, 4, 5), 
                             labels = c("random_skip", "Strongly disagree", "Disagree", 
                                        "Neither agree nor disagree", "Agree", "Strongly agree"), 
                             ordered = TRUE)

# Transform 'avoid_mistakes': Agreement on making sure there are no mistakes
df$avoid_mistakes <- factor(ifelse(df$avoid_mistakes == 99, NA, 
                                   ifelse(df$avoid_mistakes == 97, "random_skip", df$avoid_mistakes)), 
                            levels = c("random_skip", 1, 2, 3, 4, 5), 
                            labels = c("random_skip", "Strongly disagree", "Disagree", 
                                       "Neither agree nor disagree", "Agree", "Strongly agree"), 
                            ordered = TRUE)

# Transform 'check_homework_before_turning_in': Agreement on checking homework before turning it in
df$check_homework_before_turning_in <- factor(ifelse(df$check_homework_before_turning_in == 99, NA, 
                                                     ifelse(df$check_homework_before_turning_in == 97, "random_skip", df$check_homework_before_turning_in)), 
                                              levels = c("random_skip", 1, 2, 3, 4, 5), 
                                              labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                         "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                              ordered = TRUE)

# Transform 'think_before_acting': Agreement on stopping to think before acting
df$think_before_acting <- factor(ifelse(df$think_before_acting == 99, NA, 
                                        ifelse(df$think_before_acting == 97, "random_skip", df$think_before_acting)), 
                                 levels = c("random_skip", 1, 2, 3, 4, 5), 
                                 labels = c("random_skip", "Strongly disagree", "Disagree", 
                                            "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                 ordered = TRUE)

# Transform 'rush_into_activities': Agreement on rushing into activities without thinking
df$rush_into_activities <- factor(ifelse(df$rush_into_activities == 99, NA, 
                                         ifelse(df$rush_into_activities == 97, "random_skip", df$rush_into_activities)), 
                                  levels = c("random_skip", 1, 2, 3, 4, 5), 
                                  labels = c("random_skip", "Strongly disagree", "Disagree", 
                                             "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                  ordered = TRUE)

# Transform 'wait_turn_to_speak': Agreement on waiting for one's turn to speak in class
df$wait_turn_to_speak <- factor(ifelse(df$wait_turn_to_speak == 99, NA, 
                                       ifelse(df$wait_turn_to_speak == 97, "random_skip", df$wait_turn_to_speak)), 
                                levels = c("random_skip", 1, 2, 3, 4, 5), 
                                labels = c("random_skip", "Strongly disagree", "Disagree", 
                                           "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                ordered = TRUE)

# Transform 'impulsive_more_than_others': Agreement on being more impulsive than most people
df$impulsive_more_than_others <- factor(ifelse(df$impulsive_more_than_others == 99, NA, 
                                               ifelse(df$impulsive_more_than_others == 97, "random_skip", df$impulsive_more_than_others)), 
                                        levels = c("random_skip", 1, 2, 3, 4, 5), 
                                        labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                   "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                        ordered = TRUE)

# Transform 'think_carefully_before_acting': Agreement on thinking carefully before doing something
df$think_carefully_before_acting <- factor(ifelse(df$think_carefully_before_acting == 99, NA, 
                                                  ifelse(df$think_carefully_before_acting == 97, "random_skip", df$think_carefully_before_acting)), 
                                           levels = c("random_skip", 1, 2, 3, 4, 5), 
                                           labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                      "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                           ordered = TRUE)

# Transform 'curious_about_many_things': Agreement on being curious about many different things
df$curious_about_many_things <- factor(ifelse(df$curious_about_many_things == 99, NA, 
                                              ifelse(df$curious_about_many_things == 97, "random_skip", df$curious_about_many_things)), 
                                       levels = c("random_skip", 1, 2, 3, 4, 5), 
                                       labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                  "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                       ordered = TRUE)

# Transform 'like_to_ask_questions': Agreement on liking to ask questions
df$like_to_ask_questions <- factor(ifelse(df$like_to_ask_questions == 99, NA, 
                                          ifelse(df$like_to_ask_questions == 97, "random_skip", df$like_to_ask_questions)), 
                                   levels = c("random_skip", 1, 2, 3, 4, 5), 
                                   labels = c("random_skip", "Strongly disagree", "Disagree", 
                                              "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                   ordered = TRUE)

# Transform 'frustrated_learning_details': Agreement on getting frustrated when learning details of a topic
df$frustrated_learning_details <- factor(ifelse(df$frustrated_learning_details == 99, NA, 
                                                ifelse(df$frustrated_learning_details == 97, "random_skip", df$frustrated_learning_details)), 
                                         levels = c("random_skip", 1, 2, 3, 4, 5), 
                                         labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                    "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                         ordered = TRUE)

# Transform 'like_to_know_how_things_work': Agreement on liking to know how things work
df$like_to_know_how_things_work <- factor(ifelse(df$like_to_know_how_things_work == 99, NA, 
                                                 ifelse(df$like_to_know_how_things_work == 97, "random_skip", df$like_to_know_how_things_work)), 
                                          levels = c("random_skip", 1, 2, 3, 4, 5), 
                                          labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                     "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                          ordered = TRUE)

# Transform 'love_learning_new_things_school': Agreement on loving learning new things in school
df$love_learning_new_things_school <- factor(ifelse(df$love_learning_new_things_school == 99, NA, 
                                                    ifelse(df$love_learning_new_things_school == 97, "random_skip", df$love_learning_new_things_school)), 
                                             levels = c("random_skip", 1, 2, 3, 4, 5), 
                                             labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                        "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                             ordered = TRUE)

# Transform 'more_curious_than_others': Agreement on being more curious than most people
df$more_curious_than_others <- factor(ifelse(df$more_curious_than_others == 99, NA, 
                                             ifelse(df$more_curious_than_others == 97, "random_skip", df$more_curious_than_others)), 
                                      levels = c("random_skip", 1, 2, 3, 4, 5), 
                                      labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                 "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                      ordered = TRUE)

# Transform 'develop_hypotheses_check_observations': Agreement on developing hypotheses and checking them based on observations
df$develop_hypotheses_check_observations <- factor(ifelse(df$develop_hypotheses_check_observations == 99, NA, 
                                                          ifelse(df$develop_hypotheses_check_observations == 97, "random_skip", df$develop_hypotheses_check_observations)), 
                                                   levels = c("random_skip", 1, 2, 3, 4, 5), 
                                                   labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                              "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                                   ordered = TRUE)

# Transform 'find_learning_new_things_boring': Agreement on finding learning new things boring
df$find_learning_new_things_boring <- factor(ifelse(df$find_learning_new_things_boring == 99, NA, 
                                                    ifelse(df$find_learning_new_things_boring == 97, "random_skip", df$find_learning_new_things_boring)), 
                                             levels = c("random_skip", 1, 2, 3, 4, 5), 
                                             labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                        "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                             ordered = TRUE)

# Transform 'spend_time_finding_info_interest': Agreement on spending time to find more information about things of interest
df$spend_time_finding_info_interest <- factor(ifelse(df$spend_time_finding_info_interest == 99, NA, 
                                                     ifelse(df$spend_time_finding_info_interest == 97, "random_skip", df$spend_time_finding_info_interest)), 
                                              levels = c("random_skip", 1, 2, 3, 4, 5), 
                                              labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                         "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                              ordered = TRUE)

# Transform 'like_learning_new_things': Agreement on liking to learn new things
df$like_learning_new_things <- factor(ifelse(df$like_learning_new_things == 99, NA, 
                                             ifelse(df$like_learning_new_things == 97, "random_skip", df$like_learning_new_things)), 
                                      levels = c("random_skip", 1, 2, 3, 4, 5), 
                                      labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                 "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                      ordered = TRUE)

# Transform 'like_to_help_others': Agreement on liking to help others
df$like_to_help_others <- factor(ifelse(df$like_to_help_others == 99, NA, 
                                        ifelse(df$like_to_help_others == 97, "random_skip", df$like_to_help_others)), 
                                 levels = c("random_skip", 1, 2, 3, 4, 5), 
                                 labels = c("random_skip", "Strongly disagree", "Disagree", 
                                            "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                 ordered = TRUE)

# Transform 'annoyed_compromise_with_others': Agreement on getting annoyed when having to compromise with others
df$annoyed_compromise_with_others <- factor(ifelse(df$annoyed_compromise_with_others == 99, NA, 
                                                   ifelse(df$annoyed_compromise_with_others == 97, "random_skip", df$annoyed_compromise_with_others)), 
                                            levels = c("random_skip", 1, 2, 3, 4, 5), 
                                            labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                       "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                            ordered = TRUE)

# Transform 'work_well_with_others': Agreement on working well with others
df$work_well_with_others <- factor(ifelse(df$work_well_with_others == 99, NA, 
                                          ifelse(df$work_well_with_others == 97, "random_skip", df$work_well_with_others)), 
                                   levels = c("random_skip", 1, 2, 3, 4, 5), 
                                   labels = c("random_skip", "Strongly disagree", "Disagree", 
                                              "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                   ordered = TRUE)

# Transform 'start_arguments_with_others': Agreement on starting arguments with others
df$start_arguments_with_others <- factor(ifelse(df$start_arguments_with_others == 99, NA, 
                                                ifelse(df$start_arguments_with_others == 97, "random_skip", df$start_arguments_with_others)), 
                                         levels = c("random_skip", 1, 2, 3, 4, 5), 
                                         labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                    "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                         ordered = TRUE)

# Transform 'avoid_working_together_students': Agreement on avoiding working together with other students
df$avoid_working_together_students <- factor(ifelse(df$avoid_working_together_students == 99, NA, 
                                                    ifelse(df$avoid_working_together_students == 97, "random_skip", df$avoid_working_together_students)), 
                                             levels = c("random_skip", 1, 2, 3, 4, 5), 
                                             labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                        "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                             ordered = TRUE)

# Transform 'ready_to_help_anybody': Agreement on readiness to help anybody
df$ready_to_help_anybody <- factor(ifelse(df$ready_to_help_anybody == 99, NA, 
                                          ifelse(df$ready_to_help_anybody == 97, "random_skip", df$ready_to_help_anybody)), 
                                   levels = c("random_skip", 1, 2, 3, 4, 5), 
                                   labels = c("random_skip", "Strongly disagree", "Disagree", 
                                              "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                   ordered = TRUE)

# Transform 'tend_to_be_selfish': Agreement on tending to be selfish
df$tend_to_be_selfish <- factor(ifelse(df$tend_to_be_selfish == 99, NA, 
                                       ifelse(df$tend_to_be_selfish == 97, "random_skip", df$tend_to_be_selfish)), 
                                levels = c("random_skip", 1, 2, 3, 4, 5), 
                                labels = c("random_skip", "Strongly disagree", "Disagree", 
                                           "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                ordered = TRUE)

# Transform 'work_better_in_team': Agreement on working better as part of a team
df$work_better_in_team <- factor(ifelse(df$work_better_in_team == 99, NA, 
                                        ifelse(df$work_better_in_team == 97, "random_skip", df$work_better_in_team)), 
                                 levels = c("random_skip", 1, 2, 3, 4, 5), 
                                 labels = c("random_skip", "Strongly disagree", "Disagree", 
                                            "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                 ordered = TRUE)

# Transform 'enjoy_cooperating_classmates': Agreement on enjoying cooperating with classmates
df$enjoy_cooperating_classmates <- factor(ifelse(df$enjoy_cooperating_classmates == 99, NA, 
                                                 ifelse(df$enjoy_cooperating_classmates == 97, "random_skip", df$enjoy_cooperating_classmates)), 
                                          levels = c("random_skip", 1, 2, 3, 4, 5), 
                                          labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                     "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                          ordered = TRUE)

# Transform 'argue_a_lot': Agreement on arguing a lot
df$argue_a_lot <- factor(ifelse(df$argue_a_lot == 99, NA, 
                                ifelse(df$argue_a_lot == 97, "random_skip", df$argue_a_lot)), 
                         levels = c("random_skip", 1, 2, 3, 4, 5), 
                         labels = c("random_skip", "Strongly disagree", "Disagree", 
                                    "Neither agree nor disagree", "Agree", "Strongly agree"), 
                         ordered = TRUE)

# Transform 'dont_care_what_happens_to_others': Agreement on not caring what happens to other people
df$dont_care_what_happens_to_others <- factor(ifelse(df$dont_care_what_happens_to_others == 99, NA, 
                                                     ifelse(df$dont_care_what_happens_to_others == 97, "random_skip", df$dont_care_what_happens_to_others)), 
                                              levels = c("random_skip", 1, 2, 3, 4, 5), 
                                              labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                         "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                              ordered = TRUE)

# Transform 'can_sense_how_others_feel': Agreement on sensing how others feel
df$can_sense_how_others_feel <- factor(ifelse(df$can_sense_how_others_feel == 99, NA, 
                                              ifelse(df$can_sense_how_others_feel == 97, "random_skip", df$can_sense_how_others_feel)), 
                                       levels = c("random_skip", 1, 2, 3, 4, 5), 
                                       labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                  "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                       ordered = TRUE)

# Transform 'friends_wellbeing_important': Agreement on importance of friends' wellbeing
df$friends_wellbeing_important <- factor(ifelse(df$friends_wellbeing_important == 99, NA, 
                                                ifelse(df$friends_wellbeing_important == 97, "random_skip", df$friends_wellbeing_important)), 
                                         levels = c("random_skip", 1, 2, 3, 4, 5), 
                                         labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                    "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                         ordered = TRUE)

# Transform 'see_friends_perspectives': Agreement on seeing situations from friends' perspectives
df$see_friends_perspectives <- factor(ifelse(df$see_friends_perspectives == 99, NA, 
                                             ifelse(df$see_friends_perspectives == 97, "random_skip", df$see_friends_perspectives)), 
                                      levels = c("random_skip", 1, 2, 3, 4, 5), 
                                      labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                 "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                      ordered = TRUE)

# Transform 'ignore_feelings_of_others': Agreement on ignoring the feelings of others
df$ignore_feelings_of_others <- factor(ifelse(df$ignore_feelings_of_others == 99, NA, 
                                              ifelse(df$ignore_feelings_of_others == 97, "random_skip", df$ignore_feelings_of_others)), 
                                       levels = c("random_skip", 1, 2, 3, 4, 5), 
                                       labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                  "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                       ordered = TRUE)

# Transform 'more_compassionate_than_others': Agreement on being more compassionate than most people
df$more_compassionate_than_others <- factor(ifelse(df$more_compassionate_than_others == 99, NA, 
                                                   ifelse(df$more_compassionate_than_others == 97, "random_skip", df$more_compassionate_than_others)), 
                                            levels = c("random_skip", 1, 2, 3, 4, 5), 
                                            labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                       "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                            ordered = TRUE)

# Transform 'difficult_sense_what_others_think': Agreement on difficulty in sensing what others think
df$difficult_sense_what_others_think <- factor(ifelse(df$difficult_sense_what_others_think == 99, NA, 
                                                      ifelse(df$difficult_sense_what_others_think == 97, "random_skip", df$difficult_sense_what_others_think)), 
                                               levels = c("random_skip", 1, 2, 3, 4, 5), 
                                               labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                          "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                               ordered = TRUE)

# Transform 'predict_needs_of_others': Agreement on predicting the needs of others
df$predict_needs_of_others <- factor(ifelse(df$predict_needs_of_others == 99, NA, 
                                            ifelse(df$predict_needs_of_others == 97, "random_skip", df$predict_needs_of_others)), 
                                     levels = c("random_skip", 1, 2, 3, 4, 5), 
                                     labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                     ordered = TRUE)

# Transform 'upset_bad_things_happen_others': Agreement on getting upset if bad things happen to others
df$upset_bad_things_happen_others <- factor(ifelse(df$upset_bad_things_happen_others == 99, NA, 
                                                   ifelse(df$upset_bad_things_happen_others == 97, "random_skip", df$upset_bad_things_happen_others)), 
                                            levels = c("random_skip", 1, 2, 3, 4, 5), 
                                            labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                       "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                            ordered = TRUE)

# Transform 'understand_what_others_want': Agreement on understanding what others want
df$understand_what_others_want <- factor(ifelse(df$understand_what_others_want == 99, NA, 
                                                ifelse(df$understand_what_others_want == 97, "random_skip", df$understand_what_others_want)), 
                                         levels = c("random_skip", 1, 2, 3, 4, 5), 
                                         labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                    "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                         ordered = TRUE)

# Transform 'suspicious_of_others_intentions': Agreement on being suspicious of others' intentions
df$suspicious_of_others_intentions <- factor(ifelse(df$suspicious_of_others_intentions == 99, NA, 
                                                    ifelse(df$suspicious_of_others_intentions == 97, "random_skip", df$suspicious_of_others_intentions)), 
                                             levels = c("random_skip", 1, 2, 3, 4, 5), 
                                             labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                        "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                             ordered = TRUE)

# Transform 'classmates_keep_promises': Agreement on thinking most classmates keep their promises
df$classmates_keep_promises <- factor(ifelse(df$classmates_keep_promises == 99, NA, 
                                             ifelse(df$classmates_keep_promises == 97, "random_skip", df$classmates_keep_promises)), 
                                      levels = c("random_skip", 1, 2, 3, 4, 5), 
                                      labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                 "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                      ordered = TRUE)

# Transform 'believe_most_people_honest': Agreement on believing most people are honest
df$believe_most_people_honest <- factor(ifelse(df$believe_most_people_honest == 99, NA, 
                                               ifelse(df$believe_most_people_honest == 97, "random_skip", df$believe_most_people_honest)), 
                                        levels = c("random_skip", 1, 2, 3, 4, 5), 
                                        labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                   "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                        ordered = TRUE)

# Transform 'friends_can_keep_secrets': Agreement on believing friends can keep secrets
df$friends_can_keep_secrets <- factor(ifelse(df$friends_can_keep_secrets == 99, NA, 
                                             ifelse(df$friends_can_keep_secrets == 97, "random_skip", df$friends_can_keep_secrets)), 
                                      levels = c("random_skip", 1, 2, 3, 4, 5), 
                                      labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                 "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                      ordered = TRUE)

# Transform 'think_most_people_selfish': Agreement on thinking most people are selfish
df$think_most_people_selfish <- factor(ifelse(df$think_most_people_selfish == 99, NA, 
                                              ifelse(df$think_most_people_selfish == 97, "random_skip", df$think_most_people_selfish)), 
                                       levels = c("random_skip", 1, 2, 3, 4, 5), 
                                       labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                  "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                       ordered = TRUE)

# Transform 'trust_what_people_say': Agreement on trusting what people say
df$trust_what_people_say <- factor(ifelse(df$trust_what_people_say == 99, NA, 
                                          ifelse(df$trust_what_people_say == 97, "random_skip", df$trust_what_people_say)), 
                                   levels = c("random_skip", 1, 2, 3, 4, 5), 
                                   labels = c("random_skip", "Strongly disagree", "Disagree", 
                                              "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                   ordered = TRUE)

# Transform 'more_trusting_than_others': Agreement on being more trusting than most people
df$more_trusting_than_others <- factor(ifelse(df$more_trusting_than_others == 99, NA, 
                                              ifelse(df$more_trusting_than_others == 97, "random_skip", df$more_trusting_than_others)), 
                                       levels = c("random_skip", 1, 2, 3, 4, 5), 
                                       labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                  "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                       ordered = TRUE)

# Transform 'believe_most_people_kind': Agreement on believing most people are kind
df$believe_most_people_kind <- factor(ifelse(df$believe_most_people_kind == 99, NA, 
                                             ifelse(df$believe_most_people_kind == 97, "random_skip", df$believe_most_people_kind)), 
                                      levels = c("random_skip", 1, 2, 3, 4, 5), 
                                      labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                 "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                      ordered = TRUE)

# Transform 'willing_to_forgive_wrongdoers': Agreement on willingness to forgive those who have done wrong
df$willing_to_forgive_wrongdoers <- factor(ifelse(df$willing_to_forgive_wrongdoers == 99, NA, 
                                                  ifelse(df$willing_to_forgive_wrongdoers == 97, "random_skip", df$willing_to_forgive_wrongdoers)), 
                                           levels = c("random_skip", 1, 2, 3, 4, 5), 
                                           labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                      "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                           ordered = TRUE)

# Transform 'consider_all_perspectives': Agreement on trying to consider everyone's perspective before taking a position
df$consider_all_perspectives <- factor(ifelse(df$consider_all_perspectives == 99, NA, 
                                              ifelse(df$consider_all_perspectives == 97, "random_skip", df$consider_all_perspectives)), 
                                       levels = c("random_skip", 1, 2, 3, 4, 5), 
                                       labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                  "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                       ordered = TRUE)

# Transform 'understand_classmates_thinking': Agreement on wanting to understand classmates' ways of thinking
df$understand_classmates_thinking <- factor(ifelse(df$understand_classmates_thinking == 99, NA, 
                                                   ifelse(df$understand_classmates_thinking == 97, "random_skip", df$understand_classmates_thinking)), 
                                            levels = c("random_skip", 1, 2, 3, 4, 5), 
                                            labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                       "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                            ordered = TRUE)

# Transform 'view_things_different_angles': Agreement on viewing things from different angles
df$view_things_different_angles <- factor(ifelse(df$view_things_different_angles == 99, NA, 
                                                 ifelse(df$view_things_different_angles == 97, "random_skip", df$view_things_different_angles)), 
                                          levels = c("random_skip", 1, 2, 3, 4, 5), 
                                          labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                     "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                          ordered = TRUE)

# Transform 'imagine_others_feelings': Agreement on imagining how one would feel in someone else's place
df$imagine_others_feelings <- factor(ifelse(df$imagine_others_feelings == 99, NA, 
                                            ifelse(df$imagine_others_feelings == 97, "random_skip", df$imagine_others_feelings)), 
                                     levels = c("random_skip", 1, 2, 3, 4, 5), 
                                     labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                     ordered = TRUE)

# Transform 'understand_why_people_behave': Agreement on wanting to understand why people behave the way they do
df$understand_why_people_behave <- factor(ifelse(df$understand_why_people_behave == 99, NA, 
                                                 ifelse(df$understand_why_people_behave == 97, "random_skip", df$understand_why_people_behave)), 
                                          levels = c("random_skip", 1, 2, 3, 4, 5), 
                                          labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                     "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                          ordered = TRUE)

# Transform 'difficult_anticipate_thoughts_others': Agreement on finding it difficult to anticipate what others think
df$difficult_anticipate_thoughts_others <- factor(ifelse(df$difficult_anticipate_thoughts_others == 99, NA, 
                                                         ifelse(df$difficult_anticipate_thoughts_others == 97, "random_skip", df$difficult_anticipate_thoughts_others)), 
                                                  levels = c("random_skip", 1, 2, 3, 4, 5), 
                                                  labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                             "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                                  ordered = TRUE)

# Transform 'envision_friends_points_of_view': Agreement on trying to envision how things look from friends' points of view
df$envision_friends_points_of_view <- factor(ifelse(df$envision_friends_points_of_view == 99, NA, 
                                                    ifelse(df$envision_friends_points_of_view == 97, "random_skip", df$envision_friends_points_of_view)), 
                                             levels = c("random_skip", 1, 2, 3, 4, 5), 
                                             labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                        "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                             ordered = TRUE)

# Transform 'comfortable_taking_lead': Agreement on being comfortable with taking a lead role in a group
df$comfortable_taking_lead <- factor(ifelse(df$comfortable_taking_lead == 99, NA, 
                                            ifelse(df$comfortable_taking_lead == 97, "random_skip", df$comfortable_taking_lead)), 
                                     levels = c("random_skip", 1, 2, 3, 4, 5), 
                                     labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                     ordered = TRUE)

# Transform 'know_how_to_convince': Agreement on knowing how to convince others
df$know_how_to_convince <- factor(ifelse(df$know_how_to_convince == 99, NA, 
                                         ifelse(df$know_how_to_convince == 97, "random_skip", df$know_how_to_convince)), 
                                  levels = c("random_skip", 1, 2, 3, 4, 5), 
                                  labels = c("random_skip", "Strongly disagree", "Disagree", 
                                             "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                  ordered = TRUE)

# Transform 'enjoy_leading_others': Agreement on enjoying leading others
df$enjoy_leading_others <- factor(ifelse(df$enjoy_leading_others == 99, NA, 
                                         ifelse(df$enjoy_leading_others == 97, "random_skip", df$enjoy_leading_others)), 
                                  levels = c("random_skip", 1, 2, 3, 4, 5), 
                                  labels = c("random_skip", "Strongly disagree", "Disagree", 
                                             "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                  ordered = TRUE)

# Transform 'speak_up_about_important_things': Agreement on speaking up about things that matter
df$speak_up_about_important_things <- factor(ifelse(df$speak_up_about_important_things == 99, NA, 
                                                    ifelse(df$speak_up_about_important_things == 97, "random_skip", df$speak_up_about_important_things)), 
                                             levels = c("random_skip", 1, 2, 3, 4, 5), 
                                             labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                        "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                             ordered = TRUE)

# Transform 'take_initiative_with_classmates': Agreement on taking initiative when working with classmates
df$take_initiative_with_classmates <- factor(ifelse(df$take_initiative_with_classmates == 99, NA, 
                                                    ifelse(df$take_initiative_with_classmates == 97, "random_skip", df$take_initiative_with_classmates)), 
                                             levels = c("random_skip", 1, 2, 3, 4, 5), 
                                             labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                        "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                             ordered = TRUE)

# Transform 'wait_for_others_to_lead': Agreement on waiting for others to take a lead
df$wait_for_others_to_lead <- factor(ifelse(df$wait_for_others_to_lead == 99, NA, 
                                            ifelse(df$wait_for_others_to_lead == 97, "random_skip", df$wait_for_others_to_lead)), 
                                     levels = c("random_skip", 1, 2, 3, 4, 5), 
                                     labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                     ordered = TRUE)

# Transform 'want_to_be_in_charge': Agreement on wanting to be in charge
df$want_to_be_in_charge <- factor(ifelse(df$want_to_be_in_charge == 99, NA, 
                                         ifelse(df$want_to_be_in_charge == 97, "random_skip", df$want_to_be_in_charge)), 
                                  levels = c("random_skip", 1, 2, 3, 4, 5), 
                                  labels = c("random_skip", "Strongly disagree", "Disagree", 
                                             "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                  ordered = TRUE)

# Transform 'like_to_be_class_leader': Agreement on liking to be a leader in class
df$like_to_be_class_leader <- factor(ifelse(df$like_to_be_class_leader == 99, NA, 
                                            ifelse(df$like_to_be_class_leader == 97, "random_skip", df$like_to_be_class_leader)), 
                                     levels = c("random_skip", 1, 2, 3, 4, 5), 
                                     labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                     ordered = TRUE)

# Transform 'get_nervous_easily': Agreement on getting nervous easily
df$get_nervous_easily <- factor(ifelse(df$get_nervous_easily == 99, NA, 
                                       ifelse(df$get_nervous_easily == 97, "random_skip", df$get_nervous_easily)), 
                                levels = c("random_skip", 1, 2, 3, 4, 5), 
                                labels = c("random_skip", "Strongly disagree", "Disagree", 
                                           "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                ordered = TRUE)

# Transform 'more_relaxed_than_others': Agreement on being more relaxed than most people
df$more_relaxed_than_others <- factor(ifelse(df$more_relaxed_than_others == 99, NA, 
                                             ifelse(df$more_relaxed_than_others == 97, "random_skip", df$more_relaxed_than_others)), 
                                      levels = c("random_skip", 1, 2, 3, 4, 5), 
                                      labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                 "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                      ordered = TRUE)

# Transform 'worry_about_many_things': Agreement on worrying about many things
df$worry_about_many_things <- factor(ifelse(df$worry_about_many_things == 99, NA, 
                                            ifelse(df$worry_about_many_things == 97, "random_skip", df$worry_about_many_things)), 
                                     levels = c("random_skip", 1, 2, 3, 4, 5), 
                                     labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                     ordered = TRUE)

# Transform 'panic_easily': Agreement on panicking easily
df$panic_easily <- factor(ifelse(df$panic_easily == 99, NA, 
                                 ifelse(df$panic_easily == 97, "random_skip", df$panic_easily)), 
                          levels = c("random_skip", 1, 2, 3, 4, 5), 
                          labels = c("random_skip", "Strongly disagree", "Disagree", 
                                     "Neither agree nor disagree", "Agree", "Strongly agree"), 
                          ordered = TRUE)

# Transform 'able_to_work_under_pressure': Agreement on being able to work under pressure
df$able_to_work_under_pressure <- factor(ifelse(df$able_to_work_under_pressure == 99, NA, 
                                                ifelse(df$able_to_work_under_pressure == 97, "random_skip", df$able_to_work_under_pressure)), 
                                         levels = c("random_skip", 1, 2, 3, 4, 5), 
                                         labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                    "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                         ordered = TRUE)

# Transform 'remain_calm_under_stress': Agreement on remaining calm under stress
df$remain_calm_under_stress <- factor(ifelse(df$remain_calm_under_stress == 99, NA, 
                                             ifelse(df$remain_calm_under_stress == 97, "random_skip", df$remain_calm_under_stress)), 
                                      levels = c("random_skip", 1, 2, 3, 4, 5), 
                                      labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                 "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                      ordered = TRUE)

# Transform 'nervous_about_exams': Agreement on feeling nervous about approaching exams
df$nervous_about_exams <- factor(ifelse(df$nervous_about_exams == 99, NA, 
                                        ifelse(df$nervous_about_exams == 97, "random_skip", df$nervous_about_exams)), 
                                 levels = c("random_skip", 1, 2, 3, 4, 5), 
                                 labels = c("random_skip", "Strongly disagree", "Disagree", 
                                            "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                 ordered = TRUE)

# Transform 'recover_quickly_after_bad_event': Agreement on recovering quickly after something bad happens
df$recover_quickly_after_bad_event <- factor(ifelse(df$recover_quickly_after_bad_event == 99, NA, 
                                                    ifelse(df$recover_quickly_after_bad_event == 97, "random_skip", df$recover_quickly_after_bad_event)), 
                                             levels = c("random_skip", 1, 2, 3, 4, 5), 
                                             labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                        "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                             ordered = TRUE)

# Transform 'handle_stress_well': Agreement on handling stress well
df$handle_stress_well <- factor(ifelse(df$handle_stress_well == 99, NA, 
                                       ifelse(df$handle_stress_well == 97, "random_skip", df$handle_stress_well)), 
                                levels = c("random_skip", 1, 2, 3, 4, 5), 
                                labels = c("random_skip", "Strongly disagree", "Disagree", 
                                           "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                ordered = TRUE)

# Transform 'keep_emotions_under_control': Agreement on keeping emotions under control
df$keep_emotions_under_control <- factor(ifelse(df$keep_emotions_under_control == 99, NA, 
                                                ifelse(df$keep_emotions_under_control == 97, "random_skip", df$keep_emotions_under_control)), 
                                         levels = c("random_skip", 1, 2, 3, 4, 5), 
                                         labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                    "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                         ordered = TRUE)

# Transform 'get_mad_easily': Agreement on getting mad easily
df$get_mad_easily <- factor(ifelse(df$get_mad_easily == 99, NA, 
                                   ifelse(df$get_mad_easily == 97, "random_skip", df$get_mad_easily)), 
                            levels = c("random_skip", 1, 2, 3, 4, 5), 
                            labels = c("random_skip", "Strongly disagree", "Disagree", 
                                       "Neither agree nor disagree", "Agree", "Strongly agree"), 
                            ordered = TRUE)

# Transform 'change_mood_a_lot': Agreement on changing mood frequently
df$change_mood_a_lot <- factor(ifelse(df$change_mood_a_lot == 99, NA, 
                                      ifelse(df$change_mood_a_lot == 97, "random_skip", df$change_mood_a_lot)), 
                               levels = c("random_skip", 1, 2, 3, 4, 5), 
                               labels = c("random_skip", "Strongly disagree", "Disagree", 
                                          "Neither agree nor disagree", "Agree", "Strongly agree"), 
                               ordered = TRUE)

# Transform 'overreact_to_little_things': Agreement on overreacting to small things
df$overreact_to_little_things <- factor(ifelse(df$overreact_to_little_things == 99, NA, 
                                               ifelse(df$overreact_to_little_things == 97, "random_skip", df$overreact_to_little_things)), 
                                        levels = c("random_skip", 1, 2, 3, 4, 5), 
                                        labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                   "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                        ordered = TRUE)

# Transform 'stay_calm_in_tense_situations': Agreement on staying calm in tense situations
df$stay_calm_in_tense_situations <- factor(ifelse(df$stay_calm_in_tense_situations == 99, NA, 
                                                  ifelse(df$stay_calm_in_tense_situations == 97, "random_skip", df$stay_calm_in_tense_situations)), 
                                           levels = c("random_skip", 1, 2, 3, 4, 5), 
                                           labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                      "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                           ordered = TRUE)

# Transform 'easily_upset': Agreement on being easily upset
df$easily_upset <- factor(ifelse(df$easily_upset == 99, NA, 
                                 ifelse(df$easily_upset == 97, "random_skip", df$easily_upset)), 
                          levels = c("random_skip", 1, 2, 3, 4, 5), 
                          labels = c("random_skip", "Strongly disagree", "Disagree", 
                                     "Neither agree nor disagree", "Agree", "Strongly agree"), 
                          ordered = TRUE)

# Transform 'know_how_to_control_feelings': Agreement on knowing how to control feelings
df$know_how_to_control_feelings <- factor(ifelse(df$know_how_to_control_feelings == 99, NA, 
                                                 ifelse(df$know_how_to_control_feelings == 97, "random_skip", df$know_how_to_control_feelings)), 
                                          levels = c("random_skip", 1, 2, 3, 4, 5), 
                                          labels = c("random_skip", "Strongly disagree", "Disagree", 
                                                     "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                          ordered = TRUE)

# Transform 'unpredictable_emotions': Agreement on having unpredictable emotions
df$unpredictable_emotions <- factor(ifelse(df$unpredictable_emotions == 99, NA, 
                                           ifelse(df$unpredictable_emotions == 97, "random_skip", df$unpredictable_emotions)), 
                                    levels = c("random_skip", 1, 2, 3, 4, 5), 
                                    labels = c("random_skip", "Strongly disagree", "Disagree", 
                                               "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                    ordered = TRUE)

# Transform 'moody': Agreement on being moody
df$moody <- factor(ifelse(df$moody == 99, NA, 
                          ifelse(df$moody == 97, "random_skip", df$moody)), 
                   levels = c("random_skip", 1, 2, 3, 4, 5), 
                   labels = c("random_skip", "Strongly disagree", "Disagree", 
                              "Neither agree nor disagree", "Agree", "Strongly agree"), 
                   ordered = TRUE)

# Transform 'frustrated_quickly': Agreement on getting frustrated quickly
df$frustrated_quickly <- factor(ifelse(df$frustrated_quickly == 99, NA, 
                                       ifelse(df$frustrated_quickly == 97, "random_skip", df$frustrated_quickly)), 
                                levels = c("random_skip", 1, 2, 3, 4, 5), 
                                labels = c("random_skip", "Strongly disagree", "Disagree", 
                                           "Neither agree nor disagree", "Agree", "Strongly agree"), 
                                ordered = TRUE)

# Transform 'intelligence_hard_to_change': Agreement on intelligence being hard to change
df$intelligence_hard_to_change <- factor(ifelse(df$intelligence_hard_to_change == 99, NA, 
                                                df$intelligence_hard_to_change), 
                                         levels = c(1, 2, 3, 4), 
                                         labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
                                         ordered = TRUE)

# Transform 'math_skill_hard_to_improve': Agreement on math skill being hard to improve
df$math_skill_hard_to_improve <- factor(ifelse(df$math_skill_hard_to_improve == 99, NA, 
                                               df$math_skill_hard_to_improve), 
                                        levels = c(1, 2, 3, 4), 
                                        labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
                                        ordered = TRUE)

# Transform 'language_skill_hard_to_improve': Agreement on language skill being hard to improve
df$language_skill_hard_to_improve <- factor(ifelse(df$language_skill_hard_to_improve == 99, NA, 
                                                   df$language_skill_hard_to_improve), 
                                            levels = c(1, 2, 3, 4), 
                                            labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
                                            ordered = TRUE)

# Transform 'overall_life_satisfaction': Overall life satisfaction rating from 0 to 10
df$overall_life_satisfaction <- factor(ifelse(df$overall_life_satisfaction %in% c(98, 99), NA, 
                                              df$overall_life_satisfaction), 
                                       levels = c(0:10), 
                                       labels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"), 
                                       ordered = TRUE)

# ---------------------------------------------------------

# Handle implausible math and total class periods values

# ---------------------------------------------------------

# Transform 'math_class_periods_per_week'
df$math_class_periods_per_week <- ifelse(df$math_class_periods_per_week == 99, NA, df$math_class_periods_per_week)

# Create 'math_class_periods_flag' immediately after 'math_class_periods_per_week'
df <- df %>%
  mutate(math_class_periods_flag = factor(ifelse(math_class_periods_per_week > 10, "Yes", "No"), 
                                          levels = c("No", "Yes"), 
                                          labels = c("Not Flagged", "Flagged"))) %>%
  relocate(math_class_periods_flag, .after = math_class_periods_per_week)

# ---------------------------------------------------------

# Transform 'total_class_periods_per_week'
df$total_class_periods_per_week <- ifelse(df$total_class_periods_per_week == 99, NA, df$total_class_periods_per_week)

# Create 'total_class_periods_flag' immediately after 'total_class_periods_per_week'
df <- df %>%
  mutate(total_class_periods_flag = factor(ifelse(total_class_periods_per_week > 50, "Yes", "No"), 
                                           levels = c("No", "Yes"), 
                                           labels = c("Not Flagged", "Flagged"))) %>%
  relocate(total_class_periods_flag, .after = total_class_periods_per_week)

# ---------------------------------------------------------

# Transform 'time_spent_on_math_homework': Time spent on mathematics homework
df$time_spent_on_math_homework <- factor(ifelse(df$time_spent_on_math_homework == 99, NA, 
                                                df$time_spent_on_math_homework), 
                                         levels = c(1, 2, 3, 4, 5, 6), 
                                         labels = c("Up to 30 minutes", "More than 30 minutes to 1 hour", 
                                                    "More than 1 hour to 2 hours", "More than 2 hours to 3 hours", 
                                                    "More than 3 hours to 4 hours", "More than 4 hours"), 
                                         ordered = TRUE)

# Transform 'time_spent_on_language_homework': Time spent on language homework
df$time_spent_on_language_homework <- factor(ifelse(df$time_spent_on_language_homework == 99, NA, 
                                                    df$time_spent_on_language_homework), 
                                             levels = c(1, 2, 3, 4, 5, 6), 
                                             labels = c("Up to 30 minutes", "More than 30 minutes to 1 hour", 
                                                        "More than 1 hour to 2 hours", "More than 2 hours to 3 hours", 
                                                        "More than 3 hours to 4 hours", "More than 4 hours"), 
                                             ordered = TRUE)

# Transform 'time_spent_on_science_homework': Time spent on science homework
df$time_spent_on_science_homework <- factor(ifelse(df$time_spent_on_science_homework == 99, NA, 
                                                   df$time_spent_on_science_homework), 
                                            levels = c(1, 2, 3, 4, 5, 6), 
                                            labels = c("Up to 30 minutes", "More than 30 minutes to 1 hour", 
                                                       "More than 1 hour to 2 hours", "More than 2 hours to 3 hours", 
                                                       "More than 3 hours to 4 hours", "More than 4 hours"), 
                                            ordered = TRUE)

# Transform 'total_homework_time_all_subjects': Total time spent on all homework
df$total_homework_time_all_subjects <- factor(ifelse(df$total_homework_time_all_subjects == 99, NA, 
                                                     df$total_homework_time_all_subjects), 
                                              levels = c(1, 2, 3, 4, 5, 6), 
                                              labels = c("Up to 30 minutes", "More than 30 minutes to 1 hour", 
                                                         "More than 1 hour to 2 hours", "More than 2 hours to 3 hours", 
                                                         "More than 3 hours to 4 hours", "More than 4 hours"), 
                                              ordered = TRUE)

# Transform ST272Q01JA: Quality of Mathematics Instruction (Scale 1-10)
df$rate_quality_math_instruction <- ifelse(df$rate_quality_math_instruction == 99, NA, df$rate_quality_math_instruction)

# Transform ST273Q01JA to ST273Q07JA: Classroom Environment Factors (Ordered Factors)
df$students_ignore_teacher <- factor(ifelse(df$students_ignore_teacher == 99, NA, 
                                            ifelse(df$students_ignore_teacher == 97, "random_skip", df$students_ignore_teacher)),
                                     levels = c("random_skip", 1, 2, 3, 4),
                                     labels = c("random_skip", "Every lesson", "Most lessons", "Some lessons", "Never or almost never"),
                                     ordered = TRUE)

df$noise_and_disorder_in_class <- factor(ifelse(df$noise_and_disorder_in_class == 99, NA, 
                                                ifelse(df$noise_and_disorder_in_class == 97, "random_skip", df$noise_and_disorder_in_class)),
                                         levels = c("random_skip", 1, 2, 3, 4),
                                         labels = c("random_skip", "Every lesson", "Most lessons", "Some lessons", "Never or almost never"),
                                         ordered = TRUE)

df$teacher_waits_long_for_quiet <- factor(ifelse(df$teacher_waits_long_for_quiet == 99, NA, 
                                                 ifelse(df$teacher_waits_long_for_quiet == 97, "random_skip", df$teacher_waits_long_for_quiet)),
                                          levels = c("random_skip", 1, 2, 3, 4),
                                          labels = c("random_skip", "Every lesson", "Most lessons", "Some lessons", "Never or almost never"),
                                          ordered = TRUE)

df$students_cannot_work_well <- factor(ifelse(df$students_cannot_work_well == 99, NA, 
                                              ifelse(df$students_cannot_work_well == 97, "random_skip", df$students_cannot_work_well)),
                                       levels = c("random_skip", 1, 2, 3, 4),
                                       labels = c("random_skip", "Every lesson", "Most lessons", "Some lessons", "Never or almost never"),
                                       ordered = TRUE)

df$slow_start_after_lesson_begins <- factor(ifelse(df$slow_start_after_lesson_begins == 99, NA, 
                                                   ifelse(df$slow_start_after_lesson_begins == 97, "random_skip", df$slow_start_after_lesson_begins)),
                                            levels = c("random_skip", 1, 2, 3, 4),
                                            labels = c("random_skip", "Every lesson", "Most lessons", "Some lessons", "Never or almost never"),
                                            ordered = TRUE)

df$distracted_by_digital_resources <- factor(ifelse(df$distracted_by_digital_resources == 99, NA, 
                                                    ifelse(df$distracted_by_digital_resources == 97, "random_skip", df$distracted_by_digital_resources)),
                                             levels = c("random_skip", 1, 2, 3, 4),
                                             labels = c("random_skip", "Every lesson", "Most lessons", "Some lessons", "Never or almost never"),
                                             ordered = TRUE)

df$distracted_by_others_using_digital <- factor(ifelse(df$distracted_by_others_using_digital == 99, NA, 
                                                       ifelse(df$distracted_by_others_using_digital == 97, "random_skip", df$distracted_by_others_using_digital)),
                                                levels = c("random_skip", 1, 2, 3, 4),
                                                labels = c("random_skip", "Every lesson", "Most lessons", "Some lessons", "Never or almost never"),
                                                ordered = TRUE)

# Transform 'teacher_shows_interest_learning': Frequency of teacher showing interest in students' learning
df$teacher_shows_interest_learning <- factor(ifelse(df$teacher_shows_interest_learning == 99, NA, df$teacher_shows_interest_learning),
                                             levels = c(1, 2, 3, 4), 
                                             labels = c("Every lesson", "Most lessons", "Some lessons", "Never or almost never"), 
                                             ordered = TRUE)

# Transform 'teacher_gives_extra_help': Frequency of teacher giving extra help when needed
df$teacher_gives_extra_help <- factor(ifelse(df$teacher_gives_extra_help == 99, NA, df$teacher_gives_extra_help),
                                      levels = c(1, 2, 3, 4), 
                                      labels = c("Every lesson", "Most lessons", "Some lessons", "Never or almost never"), 
                                      ordered = TRUE)

# Transform 'teacher_helps_students': Frequency of teacher helping students with their learning
df$teacher_helps_students <- factor(ifelse(df$teacher_helps_students == 99, NA, df$teacher_helps_students),
                                    levels = c(1, 2, 3, 4), 
                                    labels = c("Every lesson", "Most lessons", "Some lessons", "Never or almost never"), 
                                    ordered = TRUE)

# Transform 'teacher_teaches_until_understanding': Frequency of teacher continuing teaching until students understand
df$teacher_teaches_until_understanding <- factor(ifelse(df$teacher_teaches_until_understanding == 99, NA, df$teacher_teaches_until_understanding),
                                                 levels = c(1, 2, 3, 4), 
                                                 labels = c("Every lesson", "Most lessons", "Some lessons", "Never or almost never"), 
                                                 ordered = TRUE)

# Transform 'solve_math_without_computation': Frequency of solving math without computation
df$solve_math_without_computation <- factor(ifelse(df$solve_math_without_computation %in% c(99), NA,
                                                   ifelse(df$solve_math_without_computation == 97, "random_skip", df$solve_math_without_computation)),
                                            levels = c("random_skip", 1, 2, 3, 4, 5), 
                                            labels = c("random_skip", "Never or almost never", "Less than half of the lessons", 
                                                       "About half of the lessons", "More than half of the lessons", 
                                                       "Every lesson or almost every lesson"), 
                                            ordered = TRUE)

# Transform 'explain_math_solution': Frequency of explaining how to solve math problems
df$explain_math_solution <- factor(ifelse(df$explain_math_solution %in% c(99), NA,
                                          ifelse(df$explain_math_solution == 97, "random_skip", df$explain_math_solution)),
                                   levels = c("random_skip", 1, 2, 3, 4, 5), 
                                   labels = c("random_skip", "Never or almost never", "Less than half of the lessons", 
                                              "About half of the lessons", "More than half of the lessons", 
                                              "Every lesson or almost every lesson"), 
                                   ordered = TRUE)

# Transform 'explain_math_assumptions': Frequency of explaining math assumptions
df$explain_math_assumptions <- factor(ifelse(df$explain_math_assumptions %in% c(99), NA,
                                             ifelse(df$explain_math_assumptions == 97, "random_skip", df$explain_math_assumptions)),
                                      levels = c("random_skip", 1, 2, 3, 4, 5), 
                                      labels = c("random_skip", "Never or almost never", "Less than half of the lessons", 
                                                 "About half of the lessons", "More than half of the lessons", 
                                                 "Every lesson or almost every lesson"), 
                                      ordered = TRUE)

# Transform 'explain_math_reasoning': Frequency of explaining math reasoning
df$explain_math_reasoning <- factor(ifelse(df$explain_math_reasoning %in% c(99), NA,
                                           ifelse(df$explain_math_reasoning == 97, "random_skip", df$explain_math_reasoning)),
                                    levels = c("random_skip", 1, 2, 3, 4, 5), 
                                    labels = c("random_skip", "Never or almost never", "Less than half of the lessons", 
                                               "About half of the lessons", "More than half of the lessons", 
                                               "Every lesson or almost every lesson"), 
                                    ordered = TRUE)

# Transform 'defend_math_answer': Frequency of defending math answers
df$defend_math_answer <- factor(ifelse(df$defend_math_answer %in% c(99), NA,
                                       ifelse(df$defend_math_answer == 97, "random_skip", df$defend_math_answer)),
                                levels = c("random_skip", 1, 2, 3, 4, 5), 
                                labels = c("random_skip", "Never or almost never", "Less than half of the lessons", 
                                           "About half of the lessons", "More than half of the lessons", 
                                           "Every lesson or almost every lesson"), 
                                ordered = TRUE)

# Transform 'relate_old_new_math': Frequency of relating new and old math topics
df$relate_old_new_math <- factor(ifelse(df$relate_old_new_math %in% c(99), NA,
                                        ifelse(df$relate_old_new_math == 97, "random_skip", df$relate_old_new_math)),
                                 levels = c("random_skip", 1, 2, 3, 4, 5), 
                                 labels = c("random_skip", "Never or almost never", "Less than half of the lessons", 
                                            "About half of the lessons", "More than half of the lessons", 
                                            "Every lesson or almost every lesson"), 
                                 ordered = TRUE)

# Transform 'solve_math_in_different_ways': Frequency of solving math problems in different ways
df$solve_math_in_different_ways <- factor(ifelse(df$solve_math_in_different_ways %in% c(99), NA,
                                                 ifelse(df$solve_math_in_different_ways == 97, "random_skip", df$solve_math_in_different_ways)),
                                          levels = c("random_skip", 1, 2, 3, 4, 5), 
                                          labels = c("random_skip", "Never or almost never", "Less than half of the lessons", 
                                                     "About half of the lessons", "More than half of the lessons", 
                                                     "Every lesson or almost every lesson"), 
                                          ordered = TRUE)

# Transform 'keep_trying_with_math_task': Frequency of encouragement to keep trying with math tasks
df$keep_trying_with_math_task <- factor(ifelse(df$keep_trying_with_math_task %in% c(99), NA,
                                               ifelse(df$keep_trying_with_math_task == 97, "random_skip", df$keep_trying_with_math_task)),
                                        levels = c("random_skip", 1, 2, 3, 4, 5), 
                                        labels = c("random_skip", "Never or almost never", "Less than half of the lessons", 
                                                   "About half of the lessons", "More than half of the lessons", 
                                                   "Every lesson or almost every lesson"), 
                                        ordered = TRUE)

# Transform 'memorize_math_rules': Frequency of memorizing math rules
df$memorize_math_rules <- factor(ifelse(df$memorize_math_rules %in% c(99), NA,
                                        ifelse(df$memorize_math_rules == 97, "random_skip", df$memorize_math_rules)),
                                 levels = c("random_skip", 1, 2, 3, 4, 5), 
                                 labels = c("random_skip", "Never or almost never", "Less than half of the lessons", 
                                            "About half of the lessons", "More than half of the lessons", 
                                            "Every lesson or almost every lesson"), 
                                 ordered = TRUE)

# Transform 'think_everyday_problems_with_math': Thinking about everyday problems that could be solved with math
df$think_everyday_problems_with_math <- factor(ifelse(df$think_everyday_problems_with_math == 99, NA, 
                                                      ifelse(df$think_everyday_problems_with_math == 97, "random_skip", 
                                                             df$think_everyday_problems_with_math)), 
                                               levels = c("random_skip", 1, 2, 3, 4, 5), 
                                               labels = c("random_skip", "Never or almost never", "Less than half of the lessons", 
                                                          "About half of the lessons", "More than half of the lessons", 
                                                          "Every lesson or almost every lesson"), 
                                               ordered = TRUE)

# Transform 'math_useful_in_everyday_life': Teacher showed how math is useful in everyday life
df$math_useful_in_everyday_life <- factor(ifelse(df$math_useful_in_everyday_life == 99, NA, 
                                                 ifelse(df$math_useful_in_everyday_life == 97, "random_skip", 
                                                        df$math_useful_in_everyday_life)), 
                                          levels = c("random_skip", 1, 2, 3, 4, 5), 
                                          labels = c("random_skip", "Never or almost never", "Less than half of the lessons", 
                                                     "About half of the lessons", "More than half of the lessons", 
                                                     "Every lesson or almost every lesson"), 
                                          ordered = TRUE)

# Transform 'think_mathematically': Teacher encouraged thinking mathematically
df$think_mathematically <- factor(ifelse(df$think_mathematically == 99, NA, 
                                         ifelse(df$think_mathematically == 97, "random_skip", 
                                                df$think_mathematically)), 
                                  levels = c("random_skip", 1, 2, 3, 4, 5), 
                                  labels = c("random_skip", "Never or almost never", "Less than half of the lessons", 
                                             "About half of the lessons", "More than half of the lessons", 
                                             "Every lesson or almost every lesson"), 
                                  ordered = TRUE)

# Transform 'use_math_logic_in_new_situations': Using mathematical logic when approaching new situations
df$use_math_logic_in_new_situations <- factor(ifelse(df$use_math_logic_in_new_situations == 99, NA, 
                                                     ifelse(df$use_math_logic_in_new_situations == 97, "random_skip", 
                                                            df$use_math_logic_in_new_situations)), 
                                              levels = c("random_skip", 1, 2, 3, 4, 5), 
                                              labels = c("random_skip", "Never or almost never", "Less than half of the lessons", 
                                                         "About half of the lessons", "More than half of the lessons", 
                                                         "Every lesson or almost every lesson"), 
                                              ordered = TRUE)

# Transform 'solve_difficult_problems_with_number_system': Using number system knowledge to solve problems
df$solve_difficult_problems_with_number_system <- factor(ifelse(df$solve_difficult_problems_with_number_system == 99, NA, 
                                                                ifelse(df$solve_difficult_problems_with_number_system == 97, "random_skip", 
                                                                       df$solve_difficult_problems_with_number_system)), 
                                                         levels = c("random_skip", 1, 2, 3, 4, 5), 
                                                         labels = c("random_skip", "Never or almost never", "Less than half of the lessons", 
                                                                    "About half of the lessons", "More than half of the lessons", 
                                                                    "Every lesson or almost every lesson"), 
                                                         ordered = TRUE)

# Transform 'everyday_math_problems_decision': Everyday life math problems for decision-making
df$everyday_math_problems_decision <- factor(ifelse(df$everyday_math_problems_decision == 99, NA, 
                                                    ifelse(df$everyday_math_problems_decision == 97, "random_skip", 
                                                           df$everyday_math_problems_decision)), 
                                             levels = c("random_skip", 1, 2, 3, 4, 5), 
                                             labels = c("random_skip", "Never or almost never", "Less than half of the lessons", 
                                                        "About half of the lessons", "More than half of the lessons", 
                                                        "Every lesson or almost every lesson"), 
                                             ordered = TRUE)

# Transform 'connect_math_topics_to_big_ideas': Connecting different math topics to big ideas
df$connect_math_topics_to_big_ideas <- factor(ifelse(df$connect_math_topics_to_big_ideas == 99, NA, 
                                                     ifelse(df$connect_math_topics_to_big_ideas == 97, "random_skip", 
                                                            df$connect_math_topics_to_big_ideas)), 
                                              levels = c("random_skip", 1, 2, 3, 4, 5), 
                                              labels = c("random_skip", "Never or almost never", "Less than half of the lessons", 
                                                         "About half of the lessons", "More than half of the lessons", 
                                                         "Every lesson or almost every lesson"), 
                                              ordered = TRUE)

# Transform 'solve_real_life_problems_with_math': Using math to solve real-life problems
df$solve_real_life_problems_with_math <- factor(ifelse(df$solve_real_life_problems_with_math == 99, NA, 
                                                       ifelse(df$solve_real_life_problems_with_math == 97, "random_skip", 
                                                              df$solve_real_life_problems_with_math)), 
                                                levels = c("random_skip", 1, 2, 3, 4, 5), 
                                                labels = c("random_skip", "Never or almost never", "Less than half of the lessons", 
                                                           "About half of the lessons", "More than half of the lessons", 
                                                           "Every lesson or almost every lesson"), 
                                                ordered = TRUE)

# Transform 'connect_math_to_larger_context': Connecting math ideas to a larger context
df$connect_math_to_larger_context <- factor(ifelse(df$connect_math_to_larger_context == 99, NA, 
                                                   ifelse(df$connect_math_to_larger_context == 97, "random_skip", 
                                                          df$connect_math_to_larger_context)), 
                                            levels = c("random_skip", 1, 2, 3, 4, 5), 
                                            labels = c("random_skip", "Never or almost never", "Less than half of the lessons", 
                                                       "About half of the lessons", "More than half of the lessons", 
                                                       "Every lesson or almost every lesson"), 
                                            ordered = TRUE)

# Transform 'school_train_timetable_task': Frequency of working out travel times from a train timetable
df$school_train_timetable_task <- factor(ifelse(df$school_train_timetable_task == 99, NA, 
                                                ifelse(df$school_train_timetable_task == 97, "random_skip", df$school_train_timetable_task)), 
                                         levels = c("random_skip", 1, 2, 3, 4), 
                                         labels = c("random_skip", "Frequently", "Sometimes", "Rarely", "Never"), 
                                         ordered = TRUE)

# Transform 'school_calculate_tax_task': Frequency of calculating tax on a computer purchase
df$school_calculate_tax_task <- factor(ifelse(df$school_calculate_tax_task == 99, NA, 
                                              ifelse(df$school_calculate_tax_task == 97, "random_skip", df$school_calculate_tax_task)), 
                                       levels = c("random_skip", 1, 2, 3, 4), 
                                       labels = c("random_skip", "Frequently", "Sometimes", "Rarely", "Never"), 
                                       ordered = TRUE)

# Transform 'school_calculate_tile_area_task': Frequency of calculating tiles needed for a floor
df$school_calculate_tile_area_task <- factor(ifelse(df$school_calculate_tile_area_task == 99, NA, 
                                                    ifelse(df$school_calculate_tile_area_task == 97, "random_skip", df$school_calculate_tile_area_task)), 
                                             levels = c("random_skip", 1, 2, 3, 4), 
                                             labels = c("random_skip", "Frequently", "Sometimes", "Rarely", "Never"), 
                                             ordered = TRUE)

# Transform 'school_scientific_tables_task': Frequency of interpreting scientific tables
df$school_scientific_tables_task <- factor(ifelse(df$school_scientific_tables_task == 99, NA, 
                                                  ifelse(df$school_scientific_tables_task == 97, "random_skip", df$school_scientific_tables_task)), 
                                           levels = c("random_skip", 1, 2, 3, 4), 
                                           labels = c("random_skip", "Frequently", "Sometimes", "Rarely", "Never"), 
                                           ordered = TRUE)

# Transform 'school_solve_equation_example1': Frequency of solving equations like 6x^2+5=29
df$school_solve_equation_example1 <- factor(ifelse(df$school_solve_equation_example1 == 99, NA, 
                                                   ifelse(df$school_solve_equation_example1 == 97, "random_skip", df$school_solve_equation_example1)), 
                                            levels = c("random_skip", 1, 2, 3, 4), 
                                            labels = c("random_skip", "Frequently", "Sometimes", "Rarely", "Never"), 
                                            ordered = TRUE)

# Transform 'school_map_distance_task': Frequency of measuring distances on a map with a scale
df$school_map_distance_task <- factor(ifelse(df$school_map_distance_task == 99, NA, 
                                             ifelse(df$school_map_distance_task == 97, "random_skip", df$school_map_distance_task)), 
                                      levels = c("random_skip", 1, 2, 3, 4), 
                                      labels = c("random_skip", "Frequently", "Sometimes", "Rarely", "Never"), 
                                      ordered = TRUE)

# Transform 'school_solve_equation_example2': Frequency of solving equations like 2(x+3)=(x+3)(x-3)
df$school_solve_equation_example2 <- factor(ifelse(df$school_solve_equation_example2 == 99, NA, 
                                                   ifelse(df$school_solve_equation_example2 == 97, "random_skip", df$school_solve_equation_example2)), 
                                            levels = c("random_skip", 1, 2, 3, 4), 
                                            labels = c("random_skip", "Frequently", "Sometimes", "Rarely", "Never"), 
                                            ordered = TRUE)

# Transform 'school_power_consumption_task': Frequency of calculating weekly power consumption
df$school_power_consumption_task <- factor(ifelse(df$school_power_consumption_task == 99, NA, 
                                                  ifelse(df$school_power_consumption_task == 97, "random_skip", df$school_power_consumption_task)), 
                                           levels = c("random_skip", 1, 2, 3, 4), 
                                           labels = c("random_skip", "Frequently", "Sometimes", "Rarely", "Never"), 
                                           ordered = TRUE)

# Transform 'school_solve_equation_example3': Frequency of solving equations like 3x+5=17
df$school_solve_equation_example3 <- factor(ifelse(df$school_solve_equation_example3 == 99, NA, 
                                                   ifelse(df$school_solve_equation_example3 == 97, "random_skip", df$school_solve_equation_example3)), 
                                            levels = c("random_skip", 1, 2, 3, 4), 
                                            labels = c("random_skip", "Frequently", "Sometimes", "Rarely", "Never"), 
                                            ordered = TRUE)

# Transform 'math_extract_info_from_diagrams': Extracting mathematical info from diagrams
df$math_extract_info_from_diagrams <- factor(ifelse(df$math_extract_info_from_diagrams == 99, NA, 
                                                    ifelse(df$math_extract_info_from_diagrams == 97, "random_skip", 
                                                           df$math_extract_info_from_diagrams)), 
                                             levels = c("random_skip", 1, 2, 3, 4), 
                                             labels = c("random_skip", "Frequently", "Sometimes", "Rarely", "Never"), 
                                             ordered = TRUE)

# Transform 'interpret_math_solutions_in_context': Interpreting math solutions in real-world challenges
df$interpret_math_solutions_in_context <- factor(ifelse(df$interpret_math_solutions_in_context == 99, NA, 
                                                        ifelse(df$interpret_math_solutions_in_context == 97, "random_skip", 
                                                               df$interpret_math_solutions_in_context)), 
                                                 levels = c("random_skip", 1, 2, 3, 4), 
                                                 labels = c("random_skip", "Frequently", "Sometimes", "Rarely", "Never"), 
                                                 ordered = TRUE)

# Transform 'use_statistical_variation_for_decision': Using statistical variation for decisions
df$use_statistical_variation_for_decision <- factor(ifelse(df$use_statistical_variation_for_decision == 99, NA, 
                                                           ifelse(df$use_statistical_variation_for_decision == 97, "random_skip", 
                                                                  df$use_statistical_variation_for_decision)), 
                                                    levels = c("random_skip", 1, 2, 3, 4), 
                                                    labels = c("random_skip", "Frequently", "Sometimes", "Rarely", "Never"), 
                                                    ordered = TRUE)

# Transform 'identify_math_aspects_real_world': Identifying math aspects in real-world problems
df$identify_math_aspects_real_world <- factor(ifelse(df$identify_math_aspects_real_world == 99, NA, 
                                                     ifelse(df$identify_math_aspects_real_world == 97, "random_skip", 
                                                            df$identify_math_aspects_real_world)), 
                                              levels = c("random_skip", 1, 2, 3, 4), 
                                              labels = c("random_skip", "Frequently", "Sometimes", "Rarely", "Never"), 
                                              ordered = TRUE)

# Transform 'identify_constraints_assumptions': Identifying constraints & assumptions behind modeling
df$identify_constraints_assumptions <- factor(ifelse(df$identify_constraints_assumptions == 99, NA, 
                                                     ifelse(df$identify_constraints_assumptions == 97, "random_skip", 
                                                            df$identify_constraints_assumptions)), 
                                              levels = c("random_skip", 1, 2, 3, 4), 
                                              labels = c("random_skip", "Frequently", "Sometimes", "Rarely", "Never"), 
                                              ordered = TRUE)

# Transform 'represent_situation_mathematically': Representing situations mathematically
df$represent_situation_mathematically <- factor(ifelse(df$represent_situation_mathematically == 99, NA, 
                                                       ifelse(df$represent_situation_mathematically == 97, "random_skip", 
                                                              df$represent_situation_mathematically)), 
                                                levels = c("random_skip", 1, 2, 3, 4), 
                                                labels = c("random_skip", "Frequently", "Sometimes", "Rarely", "Never"), 
                                                ordered = TRUE)

# Transform 'evaluate_significance_of_data_patterns': Evaluating significance of observed data patterns
df$evaluate_significance_of_data_patterns <- factor(ifelse(df$evaluate_significance_of_data_patterns == 99, NA, 
                                                           ifelse(df$evaluate_significance_of_data_patterns == 97, "random_skip", 
                                                                  df$evaluate_significance_of_data_patterns)), 
                                                    levels = c("random_skip", 1, 2, 3, 4), 
                                                    labels = c("random_skip", "Frequently", "Sometimes", "Rarely", "Never"), 
                                                    ordered = TRUE)

# Transform 'code_or_program_computers': Coding/programming computers
df$code_or_program_computers <- factor(ifelse(df$code_or_program_computers == 99, NA, 
                                              ifelse(df$code_or_program_computers == 97, "random_skip", 
                                                     df$code_or_program_computers)), 
                                       levels = c("random_skip", 1, 2, 3, 4), 
                                       labels = c("random_skip", "Frequently", "Sometimes", "Rarely", "Never"), 
                                       ordered = TRUE)

# Transform 'work_with_math_computer_systems': Working with math computer systems
df$work_with_math_computer_systems <- factor(ifelse(df$work_with_math_computer_systems == 99, NA, 
                                                    ifelse(df$work_with_math_computer_systems == 97, "random_skip", 
                                                           df$work_with_math_computer_systems)), 
                                             levels = c("random_skip", 1, 2, 3, 4), 
                                             labels = c("random_skip", "Frequently", "Sometimes", "Rarely", "Never"), 
                                             ordered = TRUE)

# Transform 'calculate_properties_irregular_objects': Calculating properties of irregular objects
df$calculate_properties_irregular_objects <- factor(ifelse(df$calculate_properties_irregular_objects == 99, NA, 
                                                           ifelse(df$calculate_properties_irregular_objects == 97, "random_skip", 
                                                                  df$calculate_properties_irregular_objects)), 
                                                    levels = c("random_skip", 1, 2, 3, 4), 
                                                    labels = c("random_skip", "Frequently", "Sometimes", "Rarely", "Never"), 
                                                    ordered = TRUE)

# Convert math_favourite_subject: Agreement on Mathematics as a favourite subject
df$math_favourite_subject <- factor(ifelse(df$math_favourite_subject == 99, NA, df$math_favourite_subject),
                                    levels = c(1, 2, 3, 4),
                                    labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"),
                                    ordered = TRUE)

# Convert language_favourite_subject: Agreement on test language as a favourite subject
df$language_favourite_subject <- factor(ifelse(df$language_favourite_subject == 99, NA, df$language_favourite_subject),
                                        levels = c(1, 2, 3, 4),
                                        labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"),
                                        ordered = TRUE)

# Convert science_favourite_subject: Agreement on Science as a favourite subject
df$science_favourite_subject <- factor(ifelse(df$science_favourite_subject == 99, NA, df$science_favourite_subject),
                                       levels = c(1, 2, 3, 4),
                                       labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"),
                                       ordered = TRUE)

# Convert math_easy_for_me: Agreement on Mathematics being easy
df$math_easy_for_me <- factor(ifelse(df$math_easy_for_me == 99, NA, df$math_easy_for_me),
                              levels = c(1, 2, 3, 4),
                              labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"),
                              ordered = TRUE)

# Convert language_easy_for_me: Agreement on test language being easy
df$language_easy_for_me <- factor(ifelse(df$language_easy_for_me == 99, NA, df$language_easy_for_me),
                                  levels = c(1, 2, 3, 4),
                                  labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"),
                                  ordered = TRUE)

# Convert science_easy_for_me: Agreement on Science being easy
df$science_easy_for_me <- factor(ifelse(df$science_easy_for_me == 99, NA, df$science_easy_for_me),
                                 levels = c(1, 2, 3, 4),
                                 labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"),
                                 ordered = TRUE)

# Convert want_to_do_well_in_math: Agreement on wanting to do well in Mathematics
df$want_to_do_well_in_math <- factor(ifelse(df$want_to_do_well_in_math == 99, NA, df$want_to_do_well_in_math),
                                     levels = c(1, 2, 3, 4),
                                     labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"),
                                     ordered = TRUE)

# Convert want_to_do_well_in_language: Agreement on wanting to do well in test language
df$want_to_do_well_in_language <- factor(ifelse(df$want_to_do_well_in_language == 99, NA, df$want_to_do_well_in_language),
                                         levels = c(1, 2, 3, 4),
                                         labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"),
                                         ordered = TRUE)

# Convert want_to_do_well_in_science: Agreement on wanting to do well in Science
df$want_to_do_well_in_science <- factor(ifelse(df$want_to_do_well_in_science == 99, NA, df$want_to_do_well_in_science),
                                        levels = c(1, 2, 3, 4),
                                        labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"),
                                        ordered = TRUE)


# Transform 'confident_in_math_train_timetable': Confidence in working out time from a train timetable
df$confident_in_math_train_timetable <- factor(ifelse(df$confident_in_math_train_timetable == 99, NA, 
                                                      ifelse(df$confident_in_math_train_timetable == 97, "random_skip", 
                                                             df$confident_in_math_train_timetable)), 
                                               levels = c("random_skip", 1, 2, 3, 4), 
                                               labels = c("random_skip", "Not at all confident", "Not very confident", "Confident", "Very confident"), 
                                               ordered = TRUE)

# Transform 'confident_in_math_calculate_tax': Confidence in calculating tax
df$confident_in_math_calculate_tax <- factor(ifelse(df$confident_in_math_calculate_tax == 99, NA, 
                                                    ifelse(df$confident_in_math_calculate_tax == 97, "random_skip", 
                                                           df$confident_in_math_calculate_tax)), 
                                             levels = c("random_skip", 1, 2, 3, 4), 
                                             labels = c("random_skip", "Not at all confident", "Not very confident", "Confident", "Very confident"), 
                                             ordered = TRUE)

# Transform 'confident_math_tile_area': Confidence in calculating tile area
df$confident_math_tile_area <- factor(ifelse(df$confident_math_tile_area == 99, NA, 
                                             ifelse(df$confident_math_tile_area == 97, "random_skip", 
                                                    df$confident_math_tile_area)), 
                                      levels = c("random_skip", 1, 2, 3, 4), 
                                      labels = c("random_skip", "Not at all confident", "Not very confident", "Confident", "Very confident"), 
                                      ordered = TRUE)

# Transform 'confident_math_scientific_tables': Confidence in understanding scientific tables
df$confident_math_scientific_tables <- factor(ifelse(df$confident_math_scientific_tables == 99, NA, 
                                                     ifelse(df$confident_math_scientific_tables == 97, "random_skip", 
                                                            df$confident_math_scientific_tables)), 
                                              levels = c("random_skip", 1, 2, 3, 4), 
                                              labels = c("random_skip", "Not at all confident", "Not very confident", "Confident", "Very confident"), 
                                              ordered = TRUE)

# Transform 'confident_math_map_distance': Confidence in finding map distances
df$confident_math_map_distance <- factor(ifelse(df$confident_math_map_distance == 99, NA, 
                                                ifelse(df$confident_math_map_distance == 97, "random_skip", 
                                                       df$confident_math_map_distance)), 
                                         levels = c("random_skip", 1, 2, 3, 4), 
                                         labels = c("random_skip", "Not at all confident", "Not very confident", "Confident", "Very confident"), 
                                         ordered = TRUE)

# Transform 'confident_math_power_consumption': Confidence in calculating power consumption
df$confident_math_power_consumption <- factor(ifelse(df$confident_math_power_consumption == 99, NA, 
                                                     ifelse(df$confident_math_power_consumption == 97, "random_skip", 
                                                            df$confident_math_power_consumption)), 
                                              levels = c("random_skip", 1, 2, 3, 4), 
                                              labels = c("random_skip", "Not at all confident", "Not very confident", "Confident", "Very confident"), 
                                              ordered = TRUE)

# Transform 'confident_math_extract_diagrams': Confidence in extracting mathematical information from diagrams
df$confident_math_extract_diagrams <- factor(ifelse(df$confident_math_extract_diagrams == 99, NA, 
                                                    ifelse(df$confident_math_extract_diagrams == 97, "random_skip", df$confident_math_extract_diagrams)), 
                                             levels = c("random_skip", 1, 2, 3, 4), 
                                             labels = c("random_skip", "Not at all confident", "Not very confident", "Confident", "Very confident"), 
                                             ordered = TRUE)

# Transform 'confident_math_interpret_solutions': Confidence in interpreting mathematical solutions in real-life context
df$confident_math_interpret_solutions <- factor(ifelse(df$confident_math_interpret_solutions == 99, NA, 
                                                       ifelse(df$confident_math_interpret_solutions == 97, "random_skip", df$confident_math_interpret_solutions)), 
                                                levels = c("random_skip", 1, 2, 3, 4), 
                                                labels = c("random_skip", "Not at all confident", "Not very confident", "Confident", "Very confident"), 
                                                ordered = TRUE)

# Transform 'confident_math_statistical_variation': Confidence in using statistical variation to make decisions
df$confident_math_statistical_variation <- factor(ifelse(df$confident_math_statistical_variation == 99, NA, 
                                                         ifelse(df$confident_math_statistical_variation == 97, "random_skip", df$confident_math_statistical_variation)), 
                                                  levels = c("random_skip", 1, 2, 3, 4), 
                                                  labels = c("random_skip", "Not at all confident", "Not very confident", "Confident", "Very confident"), 
                                                  ordered = TRUE)

# Transform 'confident_math_real_world_problems': Confidence in identifying mathematical aspects in real-world problems
df$confident_math_real_world_problems <- factor(ifelse(df$confident_math_real_world_problems == 99, NA, 
                                                       ifelse(df$confident_math_real_world_problems == 97, "random_skip", df$confident_math_real_world_problems)), 
                                                levels = c("random_skip", 1, 2, 3, 4), 
                                                labels = c("random_skip", "Not at all confident", "Not very confident", "Confident", "Very confident"), 
                                                ordered = TRUE)

# Transform 'confident_math_constraints': Confidence in identifying constraints and assumptions behind mathematical modelling
df$confident_math_constraints <- factor(ifelse(df$confident_math_constraints == 99, NA, 
                                               ifelse(df$confident_math_constraints == 97, "random_skip", df$confident_math_constraints)), 
                                        levels = c("random_skip", 1, 2, 3, 4), 
                                        labels = c("random_skip", "Not at all confident", "Not very confident", "Confident", "Very confident"), 
                                        ordered = TRUE)

# Transform 'confident_math_represent_situations': Confidence in representing situations mathematically
df$confident_math_represent_situations <- factor(ifelse(df$confident_math_represent_situations == 99, NA, 
                                                        ifelse(df$confident_math_represent_situations == 97, "random_skip", df$confident_math_represent_situations)), 
                                                 levels = c("random_skip", 1, 2, 3, 4), 
                                                 labels = c("random_skip", "Not at all confident", "Not very confident", "Confident", "Very confident"), 
                                                 ordered = TRUE)

# Transform 'confident_math_patterns': Confidence in evaluating significance of observed patterns in data
df$confident_math_patterns <- factor(ifelse(df$confident_math_patterns == 99, NA, 
                                            ifelse(df$confident_math_patterns == 97, "random_skip", df$confident_math_patterns)), 
                                     levels = c("random_skip", 1, 2, 3, 4), 
                                     labels = c("random_skip", "Not at all confident", "Not very confident", "Confident", "Very confident"), 
                                     ordered = TRUE)

# Transform 'confident_math_coding': Confidence in coding/programming computers
df$confident_math_coding <- factor(ifelse(df$confident_math_coding == 99, NA, 
                                          ifelse(df$confident_math_coding == 97, "random_skip", df$confident_math_coding)), 
                                   levels = c("random_skip", 1, 2, 3, 4), 
                                   labels = c("random_skip", "Not at all confident", "Not very confident", "Confident", "Very confident"), 
                                   ordered = TRUE)

# Transform 'confident_math_computer_systems': Confidence in working with computer mathematics systems
df$confident_math_computer_systems <- factor(ifelse(df$confident_math_computer_systems == 99, NA, 
                                                    ifelse(df$confident_math_computer_systems == 97, "random_skip", df$confident_math_computer_systems)), 
                                             levels = c("random_skip", 1, 2, 3, 4), 
                                             labels = c("random_skip", "Not at all confident", "Not very confident", "Confident", "Very confident"), 
                                             ordered = TRUE)

# Transform 'confident_math_irregular_objects': Confidence in calculating the properties of an irregularly shaped object
df$confident_math_irregular_objects <- factor(ifelse(df$confident_math_irregular_objects == 99, NA, 
                                                     ifelse(df$confident_math_irregular_objects == 97, "random_skip", df$confident_math_irregular_objects)), 
                                              levels = c("random_skip", 1, 2, 3, 4), 
                                              labels = c("random_skip", "Not at all confident", "Not very confident", "Confident", "Very confident"), 
                                              ordered = TRUE)

# Transform 'familiar_math_divisor': Familiarity with the term "Divisor"
df$familiar_math_divisor <- factor(ifelse(df$familiar_math_divisor == 99, NA, 
                                          ifelse(df$familiar_math_divisor == 97, "random_skip", df$familiar_math_divisor)), 
                                   levels = c("random_skip", 1, 2, 3, 4, 5), 
                                   labels = c("random_skip", "Never heard of it", "Heard of it once or twice", "Heard of it a few times", "Heard of it often", "Know it well, understand the concept"), 
                                   ordered = TRUE)

# Transform 'familiar_math_area_circle': Familiarity with the term "Area of a circle"
df$familiar_math_area_circle <- factor(ifelse(df$familiar_math_area_circle == 99, NA, 
                                              ifelse(df$familiar_math_area_circle == 97, "random_skip", df$familiar_math_area_circle)), 
                                       levels = c("random_skip", 1, 2, 3, 4, 5), 
                                       labels = c("random_skip", "Never heard of it", "Heard of it once or twice", "Heard of it a few times", "Heard of it often", "Know it well, understand the concept"), 
                                       ordered = TRUE)

# Transform 'familiar_math_congruent_figures': Familiarity with the term "Congruent figures"
df$familiar_math_congruent_figures <- factor(ifelse(df$familiar_math_congruent_figures == 99, NA, 
                                                    ifelse(df$familiar_math_congruent_figures == 97, "random_skip", df$familiar_math_congruent_figures)), 
                                             levels = c("random_skip", 1, 2, 3, 4, 5), 
                                             labels = c("random_skip", "Never heard of it", "Heard of it once or twice", "Heard of it a few times", "Heard of it often", "Know it well, understand the concept"), 
                                             ordered = TRUE)

# Transform 'familiar_math_linear_equation': Familiarity with the term "Linear equation"
df$familiar_math_linear_equation <- factor(ifelse(df$familiar_math_linear_equation == 99, NA, 
                                                  ifelse(df$familiar_math_linear_equation == 97, "random_skip", df$familiar_math_linear_equation)), 
                                           levels = c("random_skip", 1, 2, 3, 4, 5), 
                                           labels = c("random_skip", "Never heard of it", "Heard of it once or twice", "Heard of it a few times", "Heard of it often", "Know it well, understand the concept"), 
                                           ordered = TRUE)

# Transform 'familiar_math_pythagorean_theorem': Familiarity with the term "Pythagorean theorem"
df$familiar_math_pythagorean_theorem <- factor(ifelse(df$familiar_math_pythagorean_theorem == 99, NA, 
                                                      ifelse(df$familiar_math_pythagorean_theorem == 97, "random_skip", df$familiar_math_pythagorean_theorem)), 
                                               levels = c("random_skip", 1, 2, 3, 4, 5), 
                                               labels = c("random_skip", "Never heard of it", "Heard of it once or twice", "Heard of it a few times", "Heard of it often", "Know it well, understand the concept"), 
                                               ordered = TRUE)

# Transform 'familiar_math_linear_inequalities': Familiarity with the term "Linear inequalities"
df$familiar_math_linear_inequalities <- factor(ifelse(df$familiar_math_linear_inequalities == 99, NA, 
                                                      ifelse(df$familiar_math_linear_inequalities == 97, "random_skip", df$familiar_math_linear_inequalities)), 
                                               levels = c("random_skip", 1, 2, 3, 4, 5), 
                                               labels = c("random_skip", "Never heard of it", "Heard of it once or twice", "Heard of it a few times", "Heard of it often", "Know it well, understand the concept"), 
                                               ordered = TRUE)

# Transform 'familiar_math_probability': Familiarity with the term "Probability"
df$familiar_math_probability <- factor(ifelse(df$familiar_math_probability == 99, NA, 
                                              ifelse(df$familiar_math_probability == 97, "random_skip", df$familiar_math_probability)), 
                                       levels = c("random_skip", 1, 2, 3, 4, 5), 
                                       labels = c("random_skip", "Never heard of it", "Heard of it once or twice", "Heard of it a few times", "Heard of it often", "Know it well, understand the concept"), 
                                       ordered = TRUE)

# Transform 'familiar_math_fraction': Familiarity with the term "Fraction"
df$familiar_math_fraction <- factor(ifelse(df$familiar_math_fraction == 99, NA, 
                                           ifelse(df$familiar_math_fraction == 97, "random_skip", df$familiar_math_fraction)), 
                                    levels = c("random_skip", 1, 2, 3, 4, 5), 
                                    labels = c("random_skip", "Never heard of it", "Heard of it once or twice", "Heard of it a few times", "Heard of it often", "Know it well, understand the concept"), 
                                    ordered = TRUE)

# Transform 'familiar_math_geometry_3d': Familiarity with the term "3-dimensional geometry"
df$familiar_math_geometry_3d <- factor(ifelse(df$familiar_math_geometry_3d == 99, NA, 
                                              ifelse(df$familiar_math_geometry_3d == 97, "random_skip", df$familiar_math_geometry_3d)), 
                                       levels = c("random_skip", 1, 2, 3, 4, 5), 
                                       labels = c("random_skip", "Never heard of it", "Heard of it once or twice", "Heard of it a few times", "Heard of it often", "Know it well, understand the concept"), 
                                       ordered = TRUE)

# Transform 'math_participate_group_discussion': Frequency of participation in group discussions during mathematics class
df$math_participate_group_discussion <- factor(ifelse(df$math_participate_group_discussion == 99, NA, 
                                                      ifelse(df$math_participate_group_discussion == 97, "random_skip", df$math_participate_group_discussion)), 
                                               levels = c("random_skip", 1, 2, 3, 4, 5), 
                                               labels = c("random_skip", "Never or almost never", "Less than half of the time", 
                                                          "About half of the time", "More than half of the time", "All or almost all of the time"), 
                                               ordered = TRUE)

# Transform 'math_attention_to_teacher': Frequency of paying attention in mathematics class
df$math_attention_to_teacher <- factor(ifelse(df$math_attention_to_teacher == 99, NA, 
                                              ifelse(df$math_attention_to_teacher == 97, "random_skip", df$math_attention_to_teacher)), 
                                       levels = c("random_skip", 1, 2, 3, 4, 5), 
                                       labels = c("random_skip", "Never or almost never", "Less than half of the time", 
                                                  "About half of the time", "More than half of the time", "All or almost all of the time"), 
                                       ordered = TRUE)

# Transform 'math_effort_assignments': Frequency of putting effort into assignments for mathematics class
df$math_effort_assignments <- factor(ifelse(df$math_effort_assignments == 99, NA, 
                                            ifelse(df$math_effort_assignments == 97, "random_skip", df$math_effort_assignments)), 
                                     levels = c("random_skip", 1, 2, 3, 4, 5), 
                                     labels = c("random_skip", "Never or almost never", "Less than half of the time", 
                                                "About half of the time", "More than half of the time", "All or almost all of the time"), 
                                     ordered = TRUE)

# Transform 'math_time_to_learn': Frequency of making time to learn the material for mathematics class
df$math_time_to_learn <- factor(ifelse(df$math_time_to_learn == 99, NA, 
                                       ifelse(df$math_time_to_learn == 97, "random_skip", df$math_time_to_learn)), 
                                levels = c("random_skip", 1, 2, 3, 4, 5), 
                                labels = c("random_skip", "Never or almost never", "Less than half of the time", 
                                           "About half of the time", "More than half of the time", "All or almost all of the time"), 
                                ordered = TRUE)

# Transform 'math_questions_when_confused': Frequency of asking questions when confused in mathematics class
df$math_questions_when_confused <- factor(ifelse(df$math_questions_when_confused == 99, NA, 
                                                 ifelse(df$math_questions_when_confused == 97, "random_skip", df$math_questions_when_confused)), 
                                          levels = c("random_skip", 1, 2, 3, 4, 5), 
                                          labels = c("random_skip", "Never or almost never", "Less than half of the time", 
                                                     "About half of the time", "More than half of the time", "All or almost all of the time"), 
                                          ordered = TRUE)

# Transform 'math_connect_new_material': Frequency of connecting new material to previous mathematics lessons
df$math_connect_new_material <- factor(ifelse(df$math_connect_new_material == 99, NA, 
                                              ifelse(df$math_connect_new_material == 97, "random_skip", df$math_connect_new_material)), 
                                       levels = c("random_skip", 1, 2, 3, 4, 5), 
                                       labels = c("random_skip", "Never or almost never", "Less than half of the time", 
                                                  "About half of the time", "More than half of the time", "All or almost all of the time"), 
                                       ordered = TRUE)

# Transform 'math_start_assignments': Frequency of starting work on mathematics assignments right away
df$math_start_assignments <- factor(ifelse(df$math_start_assignments == 99, NA, 
                                           ifelse(df$math_start_assignments == 97, "random_skip", df$math_start_assignments)), 
                                    levels = c("random_skip", 1, 2, 3, 4, 5), 
                                    labels = c("random_skip", "Never or almost never", "Less than half of the time", 
                                               "About half of the time", "More than half of the time", "All or almost all of the time"), 
                                    ordered = TRUE)

# Transform 'worry_difficult_math': Concern about difficulty in mathematics
df$worry_difficult_math <- factor(ifelse(df$worry_difficult_math == 99, NA, 
                                         ifelse(df$worry_difficult_math == 97, "random_skip", df$worry_difficult_math)), 
                                  levels = c("random_skip", 1, 2, 3, 4), 
                                  labels = c("random_skip", "Strongly agree", "Agree", "Disagree", "Strongly disagree"), 
                                  ordered = TRUE)

# Transform 'worry_math_homework': Tension regarding mathematics homework
df$worry_math_homework <- factor(ifelse(df$worry_math_homework == 99, NA, 
                                        ifelse(df$worry_math_homework == 97, "random_skip", df$worry_math_homework)), 
                                 levels = c("random_skip", 1, 2, 3, 4), 
                                 labels = c("random_skip", "Strongly agree", "Agree", "Disagree", "Strongly disagree"), 
                                 ordered = TRUE)

# Transform 'nervous_math_problems': Nervousness when solving math problems
df$nervous_math_problems <- factor(ifelse(df$nervous_math_problems == 99, NA, 
                                          ifelse(df$nervous_math_problems == 97, "random_skip", df$nervous_math_problems)), 
                                   levels = c("random_skip", 1, 2, 3, 4), 
                                   labels = c("random_skip", "Strongly agree", "Agree", "Disagree", "Strongly disagree"), 
                                   ordered = TRUE)

# Transform 'helpless_math_problem': Feeling helpless when solving a math problem
df$helpless_math_problem <- factor(ifelse(df$helpless_math_problem == 99, NA, 
                                          ifelse(df$helpless_math_problem == 97, "random_skip", df$helpless_math_problem)), 
                                   levels = c("random_skip", 1, 2, 3, 4), 
                                   labels = c("random_skip", "Strongly agree", "Agree", "Disagree", "Strongly disagree"), 
                                   ordered = TRUE)

# Transform 'worry_math_grades': Concern about getting poor marks in mathematics
df$worry_math_grades <- factor(ifelse(df$worry_math_grades == 99, NA, 
                                      ifelse(df$worry_math_grades == 97, "random_skip", df$worry_math_grades)), 
                               levels = c("random_skip", 1, 2, 3, 4), 
                               labels = c("random_skip", "Strongly agree", "Agree", "Disagree", "Strongly disagree"), 
                               ordered = TRUE)

# Transform 'anxious_math_failure': Anxiety about failing in mathematics
df$anxious_math_failure <- factor(ifelse(df$anxious_math_failure == 99, NA, 
                                         ifelse(df$anxious_math_failure == 97, "random_skip", df$anxious_math_failure)), 
                                  levels = c("random_skip", 1, 2, 3, 4), 
                                  labels = c("random_skip", "Strongly agree", "Agree", "Disagree", "Strongly disagree"), 
                                  ordered = TRUE)

# Transform 'math_instruction_internet_program': Additional math instruction received - Internet or computer tutoring with a program or application
df$math_instruction_internet_program <- factor(df$math_instruction_internet_program, 
                                               levels = c(0, 1), 
                                               labels = c("Not Checked", "Checked"))

# Transform 'math_instruction_video_recorded': Additional math instruction received - Video-recorded instruction by a person
df$math_instruction_video_recorded <- factor(df$math_instruction_video_recorded, 
                                             levels = c(0, 1), 
                                             labels = c("Not Checked", "Checked"))

# Transform 'parent_discuss_progress': How often parents discuss student progress
df$parent_discuss_progress <- factor(ifelse(df$parent_discuss_progress == 99, NA, 
                                            ifelse(df$parent_discuss_progress == 97, "random_skip", df$parent_discuss_progress)), 
                                     levels = c("random_skip", 1, 2, 3, 4, 5), 
                                     labels = c("random_skip", "Never or almost never", "About once or twice a year", 
                                                "About once or twice a month", "About once or twice a week", 
                                                "Every day or almost every day"), 
                                     ordered = TRUE)

# Transform 'parent_talk_time': How often parents spend time talking with student
df$parent_talk_time <- factor(ifelse(df$parent_talk_time == 99, NA, 
                                     ifelse(df$parent_talk_time == 97, "random_skip", df$parent_talk_time)), 
                              levels = c("random_skip", 1, 2, 3, 4, 5), 
                              labels = c("random_skip", "Never or almost never", "About once or twice a year", 
                                         "About once or twice a month", "About once or twice a week", 
                                         "Every day or almost every day"), 
                              ordered = TRUE)

# Transform 'parent_discuss_education_importance': How often parents discuss importance of education
df$parent_discuss_education_importance <- factor(ifelse(df$parent_discuss_education_importance == 99, NA, 
                                                        ifelse(df$parent_discuss_education_importance == 97, "random_skip", df$parent_discuss_education_importance)), 
                                                 levels = c("random_skip", 1, 2, 3, 4, 5), 
                                                 labels = c("random_skip", "Never or almost never", "About once or twice a year", 
                                                            "About once or twice a month", "About once or twice a week", 
                                                            "Every day or almost every day"), 
                                                 ordered = TRUE)

# Transform 'parent_discuss_problems': How often parents discuss school-related problems
df$parent_discuss_problems <- factor(ifelse(df$parent_discuss_problems == 99, NA, 
                                            ifelse(df$parent_discuss_problems == 97, "random_skip", df$parent_discuss_problems)), 
                                     levels = c("random_skip", 1, 2, 3, 4, 5), 
                                     labels = c("random_skip", "Never or almost never", "About once or twice a year", 
                                                "About once or twice a month", "About once or twice a week", 
                                                "Every day or almost every day"), 
                                     ordered = TRUE)

# Transform 'parent_encourage_grades': How often parents encourage good grades
df$parent_encourage_grades <- factor(ifelse(df$parent_encourage_grades == 99, NA, 
                                            ifelse(df$parent_encourage_grades == 97, "random_skip", df$parent_encourage_grades)), 
                                     levels = c("random_skip", 1, 2, 3, 4, 5), 
                                     labels = c("random_skip", "Never or almost never", "About once or twice a year", 
                                                "About once or twice a month", "About once or twice a week", 
                                                "Every day or almost every day"), 
                                     ordered = TRUE)

# Transform 'parent_interest_learning': How often parents show interest in student's learning
df$parent_interest_learning <- factor(ifelse(df$parent_interest_learning == 99, NA, 
                                             ifelse(df$parent_interest_learning == 97, "random_skip", df$parent_interest_learning)), 
                                      levels = c("random_skip", 1, 2, 3, 4, 5), 
                                      labels = c("random_skip", "Never or almost never", "About once or twice a year", 
                                                 "About once or twice a month", "About once or twice a week", 
                                                 "Every day or almost every day"), 
                                      ordered = TRUE)

# Transform 'parent_discuss_future': How often parents discuss future education plans
df$parent_discuss_future <- factor(ifelse(df$parent_discuss_future == 99, NA, 
                                          ifelse(df$parent_discuss_future == 97, "random_skip", df$parent_discuss_future)), 
                                   levels = c("random_skip", 1, 2, 3, 4, 5), 
                                   labels = c("random_skip", "Never or almost never", "About once or twice a year", 
                                              "About once or twice a month", "About once or twice a week", 
                                              "Every day or almost every day"), 
                                   ordered = TRUE)

# Transform 'parent_ask_daily_school_activity': How often parents ask about daily school activities
df$parent_ask_daily_school_activity <- factor(ifelse(df$parent_ask_daily_school_activity == 99, NA, 
                                                     ifelse(df$parent_ask_daily_school_activity == 97, "random_skip", df$parent_ask_daily_school_activity)), 
                                              levels = c("random_skip", 1, 2, 3, 4, 5), 
                                              labels = c("random_skip", "Never or almost never", "About once or twice a year", 
                                                         "About once or twice a month", "About once or twice a week", 
                                                         "Every day or almost every day"), 
                                              ordered = TRUE)

# Transform expected education qualification variables

df$expected_complete_qualification_ISCED2 <- factor(ifelse(df$expected_complete_qualification_ISCED2 == 99, 
                                                           "Don't know", df$expected_complete_qualification_ISCED2), 
                                                    levels = c("Don't know", 1, 2, 3), 
                                                    labels = c("Don't know", "Yes", "No", "I don't know"))

df$expected_complete_qualification_ISCED3.3 <- factor(ifelse(df$expected_complete_qualification_ISCED3.3 == 99, 
                                                             "Don't know", df$expected_complete_qualification_ISCED3.3), 
                                                      levels = c("Don't know", 1, 2, 3), 
                                                      labels = c("Don't know", "Yes", "No", "I don't know"))

df$expected_complete_qualification_ISCED3.4 <- factor(ifelse(df$expected_complete_qualification_ISCED3.4 == 99, 
                                                             "Don't know", df$expected_complete_qualification_ISCED3.4), 
                                                      levels = c("Don't know", 1, 2, 3), 
                                                      labels = c("Don't know", "Yes", "No", "I don't know"))

df$expected_complete_qualification_ISCED5 <- factor(ifelse(df$expected_complete_qualification_ISCED5 == 99, 
                                                           "Don't know", df$expected_complete_qualification_ISCED5), 
                                                    levels = c("Don't know", 1, 2, 3), 
                                                    labels = c("Don't know", "Yes", "No", "I don't know"))

df$expected_complete_qualification_ISCED6 <- factor(ifelse(df$expected_complete_qualification_ISCED6 == 99, 
                                                           "Don't know", df$expected_complete_qualification_ISCED6), 
                                                    levels = c("Don't know", 1, 2, 3), 
                                                    labels = c("Don't know", "Yes", "No", "I don't know"))

df$expected_complete_qualification_ISCED7 <- factor(ifelse(df$expected_complete_qualification_ISCED7 == 99, 
                                                           "Don't know", df$expected_complete_qualification_ISCED7), 
                                                    levels = c("Don't know", 1, 2, 3), 
                                                    labels = c("Don't know", "Yes", "No", "I don't know"))

df$expected_complete_qualification_ISCED8 <- factor(ifelse(df$expected_complete_qualification_ISCED8 == 99, 
                                                           "Don't know", df$expected_complete_qualification_ISCED8), 
                                                    levels = c("Don't know", 1, 2, 3), 
                                                    labels = c("Don't know", "Yes", "No", "I don't know"))

# Transform 'future_study_work_internship': Participation in internships
df$future_study_work_internship <- factor(ifelse(df$future_study_work_internship == 99, NA, 
                                                 ifelse(df$future_study_work_internship == 97, "random_skip", df$future_study_work_internship)), 
                                          levels = c("random_skip", 1, 2, 3), 
                                          labels = c("random_skip", "Yes, once", "Yes, two or more times", "No"))

# Transform 'future_study_work_job_shadowing': Job shadowing or work-site visits
df$future_study_work_job_shadowing <- factor(ifelse(df$future_study_work_job_shadowing == 99, NA, 
                                                    ifelse(df$future_study_work_job_shadowing == 97, "random_skip", df$future_study_work_job_shadowing)), 
                                             levels = c("random_skip", 1, 2, 3), 
                                             labels = c("random_skip", "Yes, once", "Yes, two or more times", "No"))

# Transform 'future_study_work_job_fair': Visiting a job fair
df$future_study_work_job_fair <- factor(ifelse(df$future_study_work_job_fair == 99, NA, 
                                               ifelse(df$future_study_work_job_fair == 97, "random_skip", df$future_study_work_job_fair)), 
                                        levels = c("random_skip", 1, 2, 3), 
                                        labels = c("random_skip", "Yes, once", "Yes, two or more times", "No"))

# Transform 'future_study_work_school_career_advisor': Speaking to a career advisor at school
df$future_study_work_school_career_advisor <- factor(ifelse(df$future_study_work_school_career_advisor == 99, NA, 
                                                            ifelse(df$future_study_work_school_career_advisor == 97, "random_skip", df$future_study_work_school_career_advisor)), 
                                                     levels = c("random_skip", 1, 2, 3), 
                                                     labels = c("random_skip", "Yes, once", "Yes, two or more times", "No"))

# Transform 'future_study_work_outside_career_advisor': Speaking to a career advisor outside school
df$future_study_work_outside_career_advisor <- factor(ifelse(df$future_study_work_outside_career_advisor == 99, NA, 
                                                             ifelse(df$future_study_work_outside_career_advisor == 97, "random_skip", df$future_study_work_outside_career_advisor)), 
                                                      levels = c("random_skip", 1, 2, 3), 
                                                      labels = c("random_skip", "Yes, once", "Yes, two or more times", "No"))

# Transform 'future_study_work_interest_questionnaire': Completing an interest/ability questionnaire
df$future_study_work_interest_questionnaire <- factor(ifelse(df$future_study_work_interest_questionnaire == 99, NA, 
                                                             ifelse(df$future_study_work_interest_questionnaire == 97, "random_skip", df$future_study_work_interest_questionnaire)), 
                                                      levels = c("random_skip", 1, 2, 3), 
                                                      labels = c("random_skip", "Yes, once", "Yes, two or more times", "No"))

# Transform 'future_study_work_internet_careers': Researching careers online
df$future_study_work_internet_careers <- factor(ifelse(df$future_study_work_internet_careers == 99, NA, 
                                                       ifelse(df$future_study_work_internet_careers == 97, "random_skip", df$future_study_work_internet_careers)), 
                                                levels = c("random_skip", 1, 2, 3), 
                                                labels = c("random_skip", "Yes, once", "Yes, two or more times", "No"))

# Transform 'future_study_work_tour_institution': Attending a tour at an educational institution
df$future_study_work_tour_institution <- factor(ifelse(df$future_study_work_tour_institution == 99, NA, 
                                                       ifelse(df$future_study_work_tour_institution == 97, "random_skip", df$future_study_work_tour_institution)), 
                                                levels = c("random_skip", 1, 2, 3), 
                                                labels = c("random_skip", "Yes, once", "Yes, two or more times", "No"))

# Transform 'future_study_work_internet_programmes': Researching educational programmes online
df$future_study_work_internet_programmes <- factor(ifelse(df$future_study_work_internet_programmes == 99, NA, 
                                                          ifelse(df$future_study_work_internet_programmes == 97, "random_skip", df$future_study_work_internet_programmes)), 
                                                   levels = c("random_skip", 1, 2, 3), 
                                                   labels = c("random_skip", "Yes, once", "Yes, two or more times", "No"))

# Transform 'future_study_work_student_financing': Researching student financing options
df$future_study_work_student_financing <- factor(ifelse(df$future_study_work_student_financing == 99, NA, 
                                                        ifelse(df$future_study_work_student_financing == 97, "random_skip", df$future_study_work_student_financing)), 
                                                 levels = c("random_skip", 1, 2, 3), 
                                                 labels = c("random_skip", "Yes, once", "Yes, two or more times", "No"))

# Transform 'worry_unprepared_after_school': Concerns about preparedness for life after school
df$worry_unprepared_after_school <- factor(ifelse(df$worry_unprepared_after_school == 99, NA, 
                                                  ifelse(df$worry_unprepared_after_school == 97, "random_skip", df$worry_unprepared_after_school)), 
                                           levels = c("random_skip", 1, 2, 3, 4), 
                                           labels = c("random_skip", "Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
                                           ordered = TRUE)

# Transform 'informed_paths_after_school': Feeling informed about possible post-school paths
df$informed_paths_after_school <- factor(ifelse(df$informed_paths_after_school == 99, NA, 
                                                ifelse(df$informed_paths_after_school == 97, "random_skip", df$informed_paths_after_school)), 
                                         levels = c("random_skip", 1, 2, 3, 4), 
                                         labels = c("random_skip", "Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
                                         ordered = TRUE)

# Transform 'family_pressure_specific_path': Family pressure to follow a specific post-school path
df$family_pressure_specific_path <- factor(ifelse(df$family_pressure_specific_path == 99, NA, 
                                                  ifelse(df$family_pressure_specific_path == 97, "random_skip", df$family_pressure_specific_path)), 
                                           levels = c("random_skip", 1, 2, 3, 4), 
                                           labels = c("random_skip", "Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
                                           ordered = TRUE)

# Transform 'worry_insufficient_money_future': Worry about financial situation after school
df$worry_insufficient_money_future <- factor(ifelse(df$worry_insufficient_money_future == 99, NA, 
                                                    ifelse(df$worry_insufficient_money_future == 97, "random_skip", df$worry_insufficient_money_future)), 
                                    levels = c("random_skip", 1, 2, 3, 4), 
                                    labels = c("random_skip", "Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
                                    ordered = TRUE)

# Transform 'school_unprepared_adult_life': Feeling that school has not prepared for adulthood
df$school_unprepared_adult_life <- factor(ifelse(df$school_unprepared_adult_life == 99, NA, 
                                                 ifelse(df$school_unprepared_adult_life == 97, "random_skip", df$school_unprepared_adult_life)), 
                                    levels = c("random_skip", 1, 2, 3, 4), 
                                    labels = c("random_skip", "Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
                                    ordered = TRUE)

# Transform 'school_waste_of_time': Belief that school has been a waste of time
df$school_waste_of_time <- factor(ifelse(df$school_waste_of_time == 99, NA, 
                                         ifelse(df$school_waste_of_time == 97, "random_skip", df$school_waste_of_time)), 
                                    levels = c("random_skip", 1, 2, 3, 4), 
                                    labels = c("random_skip", "Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
                                    ordered = TRUE)

# Transform 'school_confidence_decisions': School's impact on confidence for decision-making
df$school_confidence_decisions <- factor(ifelse(df$school_confidence_decisions == 99, NA, 
                                                ifelse(df$school_confidence_decisions == 97, "random_skip", df$school_confidence_decisions)), 
                                    levels = c("random_skip", 1, 2, 3, 4), 
                                    labels = c("random_skip", "Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
                                    ordered = TRUE)

# Transform 'school_useful_in_job': School teaching useful job-related skills
df$school_useful_in_job <- factor(ifelse(df$school_useful_in_job == 99, NA, 
                                         ifelse(df$school_useful_in_job == 97, "random_skip", df$school_useful_in_job)), 
                                    levels = c("random_skip", 1, 2, 3, 4), 
                                    labels = c("random_skip", "Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
                                    ordered = TRUE)

# Transform 'school_prepared_future': School’s role in preparing for the future
df$school_prepared_future <- factor(ifelse(df$school_prepared_future == 99, NA, 
                                           ifelse(df$school_prepared_future == 97, "random_skip", df$school_prepared_future)), 
                                    levels = c("random_skip", 1, 2, 3, 4), 
                                    labels = c("random_skip", "Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
                                    ordered = TRUE)

df$school_closure_covid <- factor(ifelse(df$school_closure_covid == 99, "unknown", df$school_closure_covid), 
                                    levels = c("unknown", 1, 2, 3, 4, 5, 6), 
                                    labels = c("unknown", "No", "Yes, up to 1 month", "Yes, more than 1 month and up to 3 months", 
                                             "Yes, more than 3 months and up to 6 months", "Yes, more than 6 months and up to 12 months", 
                                             "Yes, more than 12 months"),
                                    ordered = TRUE)

df$school_closure_other_reason <- factor(ifelse(df$school_closure_other_reason == 99, "unknown", df$school_closure_other_reason), 
                                    levels = c("unknown", 1, 2, 3, 4, 5, 6), 
                                    labels = c("unknown", "No", "Yes, up to 1 month", "Yes, more than 1 month and up to 3 months", 
                                                    "Yes, more than 3 months and up to 6 months", "Yes, more than 6 months and up to 12 months", 
                                                    "Yes, more than 12 months"),
                                    ordered = TRUE)

# Transform 'covid_materials_sent': Frequency of receiving learning materials during COVID closures
df$covid_materials_sent <- factor(ifelse(df$covid_materials_sent == 99, NA, 
                                         ifelse(df$covid_materials_sent == 97, "random_skip", 
                                                ifelse(df$covid_materials_sent == 95, "valid_skip", df$covid_materials_sent))), 
                                    levels = c("valid_skip", "random_skip", 1, 2, 3, 4), 
                                    labels = c("valid_skip", "random_skip", "Never", "A few times", "About once or twice a week", "Every day or almost every day"), 
                                    ordered = TRUE)

# Transform 'covid_assignments_sent': Frequency of receiving assignments during COVID closures
df$covid_assignments_sent <- factor(ifelse(df$covid_assignments_sent == 99, NA, 
                                           ifelse(df$covid_assignments_sent == 97, "random_skip", 
                                                  ifelse(df$covid_assignments_sent == 95, "valid_skip", df$covid_assignments_sent))), 
                                    levels = c("valid_skip", "random_skip", 1, 2, 3, 4), 
                                    labels = c("valid_skip", "random_skip", "Never", "A few times", "About once or twice a week", "Every day or almost every day"), 
                                    ordered = TRUE)

# Transform 'covid_uploaded_material': Frequency of material uploads to LMS during COVID closures
df$covid_uploaded_material <- factor(ifelse(df$covid_uploaded_material == 99, NA, 
                                            ifelse(df$covid_uploaded_material == 97, "random_skip", 
                                                   ifelse(df$covid_uploaded_material == 95, "valid_skip", df$covid_uploaded_material))), 
                                     levels = c("valid_skip", "random_skip", 1, 2, 3, 4), 
                                     labels = c("valid_skip", "random_skip", "Never", "A few times", "About once or twice a week", "Every day or almost every day"), 
                                     ordered = TRUE)

# Transform 'covid_checked_assignments': Frequency of teachers checking assignments during COVID closures
df$covid_checked_assignments <- factor(ifelse(df$covid_checked_assignments == 99, NA, 
                                              ifelse(df$covid_checked_assignments == 97, "random_skip", 
                                                     ifelse(df$covid_checked_assignments == 95, "valid_skip", df$covid_checked_assignments))), 
                                      levels = c("valid_skip", "random_skip", 1, 2, 3, 4), 
                                      labels = c("valid_skip", "random_skip", "Never", "A few times", "About once or twice a week", "Every day or almost every day"), 
                                      ordered = TRUE)

# Transform 'covid_virtual_classes': Frequency of attending live virtual classes during COVID closures
df$covid_virtual_classes <- factor(ifelse(df$covid_virtual_classes == 99, NA, 
                                          ifelse(df$covid_virtual_classes == 97, "random_skip", 
                                                 ifelse(df$covid_virtual_classes == 95, "valid_skip", df$covid_virtual_classes))), 
                                      levels = c("valid_skip", "random_skip", 1, 2, 3, 4), 
                                      labels = c("valid_skip", "random_skip", "Never", "A few times", "About once or twice a week", "Every day or almost every day"), 
                                      ordered = TRUE)

# Transform 'covid_submitted_assignments': Frequency of submitting assignments during COVID closures
df$covid_submitted_assignments <- factor(ifelse(df$covid_submitted_assignments == 99, NA, 
                                                ifelse(df$covid_submitted_assignments == 97, "random_skip", 
                                                       ifelse(df$covid_submitted_assignments == 95, "valid_skip", df$covid_submitted_assignments))), 
                                      levels = c("valid_skip", "random_skip", 1, 2, 3, 4), 
                                      labels = c("valid_skip", "random_skip", "Never", "A few times", "About once or twice a week", "Every day or almost every day"), 
                                      ordered = TRUE)

# Transform 'covid_tips_self_study': Frequency of receiving study tips during COVID closures
df$covid_tips_self_study <- factor(ifelse(df$covid_tips_self_study == 99, NA, 
                                          ifelse(df$covid_tips_self_study == 97, "random_skip", 
                                                 ifelse(df$covid_tips_self_study == 95, "valid_skip", df$covid_tips_self_study))), 
                                    levels = c("valid_skip", "random_skip", 1, 2, 3, 4), 
                                    labels = c("valid_skip", "random_skip", "Never", "A few times", "About once or twice a week", "Every day or almost every day"), 
                                    ordered = TRUE)

# Transform 'covid_checked_wellbeing': Frequency of teachers checking on student well-being during COVID closures
df$covid_checked_wellbeing <- factor(ifelse(df$covid_checked_wellbeing == 99, NA, 
                                            ifelse(df$covid_checked_wellbeing == 97, "random_skip", 
                                                   ifelse(df$covid_checked_wellbeing == 95, "valid_skip", df$covid_checked_wellbeing))), 
                                     levels = c("valid_skip", "random_skip", 1, 2, 3, 4), 
                                     labels = c("valid_skip", "random_skip", "Never", "A few times", "About once or twice a week", "Every day or almost every day"), 
                                     ordered = TRUE)

# Define Mode function (excluding 'valid_skip' from mode calculation)
Mode <- function(x, na.rm = TRUE) {
  ux <- unique(x[!is.na(x) & x != "valid_skip"])  # Ensure 'valid_skip' is excluded from mode calculation
  ux[which.max(tabulate(match(x, ux)))]
}

# Transform 'covid_device_most_used': Device most used during COVID school closures
df$covid_device_most_used <- factor(ifelse(df$covid_device_most_used == 95 | df$covid_device_most_used == 1, "valid_skip",  # Ensure 95 is always "valid_skip"
                                           ifelse(df$covid_device_most_used %in% c(99, NA),   
                                                  Mode(df$covid_device_most_used, na.rm = TRUE),  # Impute mode for 99 and NA only
                                                  df$covid_device_most_used)),   
                                    levels = c("valid_skip", 1, 2, 3, 4, 5),   
                                    labels = c("valid_skip",   
                                               "My own laptop, desktop computer, or tablet",   
                                               "My own smartphone",   
                                               "A digital device that was also used by other family members",   
                                               "A digital device that my school gave or lent to me",   
                                               "I did not have any digital device for my schoolwork"))   

# Transform 'covid_learning_comparison': Learning comparison during COVID closures
df$covid_learning_comparison <- factor(ifelse(df$covid_learning_comparison == 95 | df$covid_learning_comparison == 1, "valid_skip",  # Ensure 95 is always "valid_skip"
                                              ifelse(df$covid_learning_comparison %in% c(99, NA),   
                                                     Mode(df$covid_learning_comparison, na.rm = TRUE),  # Impute mode for 99 and NA only
                                                     df$covid_learning_comparison)),   
                                       levels = c("valid_skip", 1, 2, 3),   
                                       labels = c("valid_skip",   
                                                  "I learnt less when my school building was closed.",   
                                                  "I learnt about as much when my school building was closed.",   
                                                  "I learnt more when my school building was closed."),   
                                       ordered = TRUE)

# Transform 'covid_use_paper_materials': Frequency of using paper materials during COVID closures
df$covid_use_paper_materials <- factor(ifelse(df$covid_use_paper_materials == 99, NA, 
                                              ifelse(df$covid_use_paper_materials == 95, "valid_skip", 
                                                     ifelse(df$covid_use_paper_materials == 97, "random_skip", 
                                                            df$covid_use_paper_materials))), 
                                      levels = c("valid_skip", "random_skip", 1, 2, 3, 4), 
                                      labels = c("valid_skip", "random_skip", "Never", "A few times", "About once or twice a week", "Every day or almost every day"), 
                                      ordered = TRUE)

# Transform 'covid_use_digital_materials': Frequency of using digital materials during COVID closures
df$covid_use_digital_materials <- factor(ifelse(df$covid_use_digital_materials == 99, NA, 
                                                ifelse(df$covid_use_digital_materials == 95, "valid_skip", 
                                                       ifelse(df$covid_use_digital_materials == 97, "random_skip", 
                                                              df$covid_use_digital_materials))), 
                                      levels = c("valid_skip", "random_skip", 1, 2, 3, 4), 
                                      labels = c("valid_skip", "random_skip", "Never", "A few times", "About once or twice a week", "Every day or almost every day"), 
                                      ordered = TRUE)

# Transform 'covid_use_school_realtime_lessons': Frequency of attending school live lessons during COVID closures
df$covid_use_school_realtime_lessons <- factor(ifelse(df$covid_use_school_realtime_lessons == 99, NA, 
                                                      ifelse(df$covid_use_school_realtime_lessons == 95, "valid_skip", 
                                                             ifelse(df$covid_use_school_realtime_lessons == 97, "random_skip", 
                                                                    df$covid_use_school_realtime_lessons))), 
                                      levels = c("valid_skip", "random_skip", 1, 2, 3, 4), 
                                      labels = c("valid_skip", "random_skip", "Never", "A few times", "About once or twice a week", "Every day or almost every day"), 
                                      ordered = TRUE)

# Transform 'covid_use_private_realtime_lessons': Frequency of attending private tutor live lessons during COVID closures
df$covid_use_private_realtime_lessons <- factor(ifelse(df$covid_use_private_realtime_lessons == 99, NA, 
                                                       ifelse(df$covid_use_private_realtime_lessons == 95, "valid_skip", 
                                                              ifelse(df$covid_use_private_realtime_lessons == 97, "random_skip", 
                                                                     df$covid_use_private_realtime_lessons))), 
                                      levels = c("valid_skip", "random_skip", 1, 2, 3, 4), 
                                      labels = c("valid_skip", "random_skip", "Never", "A few times", "About once or twice a week", "Every day or almost every day"), 
                                      ordered = TRUE)

# Transform 'covid_use_sms_materials': Frequency of receiving learning materials via SMS or WhatsApp during COVID closures
df$covid_use_sms_materials <- factor(ifelse(df$covid_use_sms_materials == 99, NA, 
                                            ifelse(df$covid_use_sms_materials == 95, "valid_skip", 
                                                   ifelse(df$covid_use_sms_materials == 97, "random_skip", 
                                                          df$covid_use_sms_materials))), 
                                      levels = c("valid_skip", "random_skip", 1, 2, 3, 4), 
                                      labels = c("valid_skip", "random_skip", "Never", "A few times", "About once or twice a week", "Every day or almost every day"), 
                                      ordered = TRUE)

# Transform 'covid_use_teacher_recorded_material': Frequency of using recorded lessons from school teachers during COVID closures
df$covid_use_teacher_recorded_material <- factor(ifelse(df$covid_use_teacher_recorded_material == 99, NA, 
                                                        ifelse(df$covid_use_teacher_recorded_material == 95, "valid_skip", 
                                                               ifelse(df$covid_use_teacher_recorded_material == 97, "random_skip", 
                                                                      df$covid_use_teacher_recorded_material))), 
                                      levels = c("valid_skip", "random_skip", 1, 2, 3, 4), 
                                      labels = c("valid_skip", "random_skip", "Never", "A few times", "About once or twice a week", "Every day or almost every day"), 
                                      ordered = TRUE)

# Transform 'covid_use_other_recorded_material': Frequency of using recorded lessons from external sources during COVID closures
df$covid_use_other_recorded_material <- factor(ifelse(df$covid_use_other_recorded_material == 99, NA, 
                                                      ifelse(df$covid_use_other_recorded_material == 95, "valid_skip", 
                                                             ifelse(df$covid_use_other_recorded_material == 97, "random_skip", 
                                                                    df$covid_use_other_recorded_material))), 
                                      levels = c("valid_skip", "random_skip", 1, 2, 3, 4), 
                                      labels = c("valid_skip", "random_skip", "Never", "A few times", "About once or twice a week", "Every day or almost every day"), 
                                      ordered = TRUE)

# Transform 'covid_use_broadcast_lessons': Frequency of using lessons broadcast over television or radio during COVID closures
df$covid_use_broadcast_lessons <- factor(ifelse(df$covid_use_broadcast_lessons == 99, NA, 
                                                ifelse(df$covid_use_broadcast_lessons == 95, "valid_skip", 
                                                       ifelse(df$covid_use_broadcast_lessons == 97, "random_skip", 
                                                              df$covid_use_broadcast_lessons))), 
                                      levels = c("valid_skip", "random_skip", 1, 2, 3, 4), 
                                      labels = c("valid_skip", "random_skip", "Never", "A few times", "About once or twice a week", "Every day or almost every day"), 
                                      ordered = TRUE)

# Transform 'covid_problems_device_access': Problems with access to a digital device when needed during COVID closures
df$covid_problems_device_access <- factor(ifelse(df$covid_problems_device_access == 99, NA, 
                                                 ifelse(df$covid_problems_device_access == 97, "random_skip", 
                                                        ifelse(df$covid_problems_device_access == 95, "valid_skip", 
                                                               df$covid_problems_device_access))), 
                                      levels = c("valid_skip", "random_skip", 1, 2, 3, 4), 
                                      labels = c("valid_skip", "random_skip", "Never", "A few times", "About once or twice a week", "Every day or almost every day"), 
                                      ordered = TRUE)

# Transform 'covid_problems_internet_access': Problems with internet access during COVID closures
df$covid_problems_internet_access <- factor(ifelse(df$covid_problems_internet_access == 99, NA, 
                                                   ifelse(df$covid_problems_internet_access == 97, "random_skip", 
                                                          ifelse(df$covid_problems_internet_access == 95, "valid_skip", 
                                                                 df$covid_problems_internet_access))), 
                                      levels = c("valid_skip", "random_skip", 1, 2, 3, 4), 
                                      labels = c("valid_skip", "random_skip", "Never", "A few times", "About once or twice a week", "Every day or almost every day"), 
                                      ordered = TRUE)

# Transform 'covid_problems_school_supplies': Problems with access to school supplies during COVID closures
df$covid_problems_school_supplies <- factor(ifelse(df$covid_problems_school_supplies == 99, NA, 
                                                   ifelse(df$covid_problems_school_supplies == 97, "random_skip", 
                                                          ifelse(df$covid_problems_school_supplies == 95, "valid_skip", 
                                                                 df$covid_problems_school_supplies))), 
                                      levels = c("valid_skip", "random_skip", 1, 2, 3, 4), 
                                      labels = c("valid_skip", "random_skip", "Never", "A few times", "About once or twice a week", "Every day or almost every day"), 
                                      ordered = TRUE)

# Transform 'covid_problems_quiet_study': Problems with finding a quiet place to study during COVID closures
df$covid_problems_quiet_study <- factor(ifelse(df$covid_problems_quiet_study == 99, NA, 
                                               ifelse(df$covid_problems_quiet_study == 97, "random_skip", 
                                                      ifelse(df$covid_problems_quiet_study == 95, "valid_skip", 
                                                             df$covid_problems_quiet_study))), 
                                      levels = c("valid_skip", "random_skip", 1, 2, 3, 4), 
                                      labels = c("valid_skip", "random_skip", "Never", "A few times", "About once or twice a week", "Every day or almost every day"), 
                                      ordered = TRUE)

# Transform 'covid_problems_household_responsibilities': Problems with household responsibilities affecting study during COVID closures
df$covid_problems_household_responsibilities <- factor(ifelse(df$covid_problems_household_responsibilities == 99, NA, 
                                                              ifelse(df$covid_problems_household_responsibilities == 97, "random_skip", 
                                                                     ifelse(df$covid_problems_household_responsibilities == 95, "valid_skip", 
                                                                            df$covid_problems_household_responsibilities))), 
                                      levels = c("valid_skip", "random_skip", 1, 2, 3, 4), 
                                      labels = c("valid_skip", "random_skip", "Never", "A few times", "About once or twice a week", "Every day or almost every day"), 
                                      ordered = TRUE)

# Transform 'covid_problems_motivation': Problems with motivation during COVID closures
df$covid_problems_motivation <- factor(ifelse(df$covid_problems_motivation == 99, NA, 
                                              ifelse(df$covid_problems_motivation == 97, "random_skip", 
                                                     ifelse(df$covid_problems_motivation == 95, "valid_skip", 
                                                            df$covid_problems_motivation))), 
                                      levels = c("valid_skip", "random_skip", 1, 2, 3, 4), 
                                      labels = c("valid_skip", "random_skip", "Never", "A few times", "About once or twice a week", "Every day or almost every day"), 
                                      ordered = TRUE)

# Transform 'covid_problems_assignment_understanding': Problems with understanding assignments during COVID closures
df$covid_problems_assignment_understanding <- factor(ifelse(df$covid_problems_assignment_understanding == 99, NA, 
                                                            ifelse(df$covid_problems_assignment_understanding == 97, "random_skip", 
                                                                   ifelse(df$covid_problems_assignment_understanding == 95, "valid_skip", 
                                                                          df$covid_problems_assignment_understanding))), 
                                      levels = c("valid_skip", "random_skip", 1, 2, 3, 4), 
                                      labels = c("valid_skip", "random_skip", "Never", "A few times", "About once or twice a week", "Every day or almost every day"), 
                                      ordered = TRUE)

# Transform 'covid_problems_help_with_work': Problems with finding someone to help with school work during COVID closures
df$covid_problems_help_with_work <- factor(ifelse(df$covid_problems_help_with_work == 99, NA, 
                                                  ifelse(df$covid_problems_help_with_work == 97, "random_skip", 
                                                         ifelse(df$covid_problems_help_with_work == 95, "valid_skip", 
                                                                df$covid_problems_help_with_work))), 
                                      levels = c("valid_skip", "random_skip", 1, 2, 3, 4), 
                                      labels = c("valid_skip", "random_skip", "Never", "A few times", "About once or twice a week", "Every day or almost every day"), 
                                      ordered = TRUE)

# Transform 'covid_family_help_work': Family help with school work during COVID closures
df$covid_family_help_work <- factor(ifelse(df$covid_family_help_work == 99, NA, 
                                           ifelse(df$covid_family_help_work == 97, "random_skip", 
                                                  ifelse(df$covid_family_help_work == 95, "valid_skip", 
                                                         df$covid_family_help_work))), 
                                     levels = c("valid_skip", "random_skip", 1, 2, 3, 4), 
                                     labels = c("valid_skip", "random_skip", "Never", "A few times", "About once or twice a week", "Every day or almost every day"), 
                                     ordered = TRUE)

# Transform 'covid_family_ask_learning': Family asked about learning during COVID closures
df$covid_family_ask_learning <- factor(ifelse(df$covid_family_ask_learning == 99, NA, 
                                              ifelse(df$covid_family_ask_learning == 97, "random_skip", 
                                                     ifelse(df$covid_family_ask_learning == 95, "valid_skip", 
                                                            df$covid_family_ask_learning))), 
                                      levels = c("valid_skip", "random_skip", 1, 2, 3, 4), 
                                      labels = c("valid_skip", "random_skip", "Never", "A few times", "About once or twice a week", "Every day or almost every day"), 
                                      ordered = TRUE)

# Transform 'covid_family_create_schedule': Family helped create learning schedule during COVID closures
df$covid_family_create_schedule <- factor(ifelse(df$covid_family_create_schedule == 99, NA, 
                                                 ifelse(df$covid_family_create_schedule == 97, "random_skip", 
                                                        ifelse(df$covid_family_create_schedule == 95, "valid_skip", 
                                                               df$covid_family_create_schedule))), 
                                      levels = c("valid_skip", "random_skip", 1, 2, 3, 4), 
                                      labels = c("valid_skip", "random_skip", "Never", "A few times", "About once or twice a week", "Every day or almost every day"), 
                                      ordered = TRUE)

# Transform 'covid_family_access_materials': Family helped access learning materials online during COVID closures
df$covid_family_access_materials <- factor(ifelse(df$covid_family_access_materials == 99, NA, 
                                                  ifelse(df$covid_family_access_materials == 97, "random_skip", 
                                                         ifelse(df$covid_family_access_materials == 95, "valid_skip", 
                                                                df$covid_family_access_materials))), 
                                      levels = c("valid_skip", "random_skip", 1, 2, 3, 4), 
                                      labels = c("valid_skip", "random_skip", "Never", "A few times", "About once or twice a week", "Every day or almost every day"), 
                                      ordered = TRUE)

# Transform 'covid_family_check_assignments': Family checked assignments during COVID closures
df$covid_family_check_assignments <- factor(ifelse(df$covid_family_check_assignments == 99, NA, 
                                                   ifelse(df$covid_family_check_assignments == 97, "random_skip", 
                                                          ifelse(df$covid_family_check_assignments == 95, "valid_skip", 
                                                                 df$covid_family_check_assignments))), 
                                      levels = c("valid_skip", "random_skip", 1, 2, 3, 4), 
                                      labels = c("valid_skip", "random_skip", "Never", "A few times", "About once or twice a week", "Every day or almost every day"), 
                                      ordered = TRUE)

# Transform 'covid_family_explain_content': Family explained new content during COVID closures
df$covid_family_explain_content <- factor(ifelse(df$covid_family_explain_content == 99, NA, 
                                                 ifelse(df$covid_family_explain_content == 97, "random_skip", 
                                                        ifelse(df$covid_family_explain_content == 95, "valid_skip", 
                                                               df$covid_family_explain_content))), 
                                      levels = c("valid_skip", "random_skip", 1, 2, 3, 4), 
                                      labels = c("valid_skip", "random_skip", "Never", "A few times", "About once or twice a week", "Every day or almost every day"), 
                                      ordered = TRUE)

# Transform 'covid_family_find_resources': Family helped find additional learning resources during COVID closures
df$covid_family_find_resources <- factor(ifelse(df$covid_family_find_resources == 99, NA, 
                                                ifelse(df$covid_family_find_resources == 97, "random_skip", 
                                                       ifelse(df$covid_family_find_resources == 95, "valid_skip", 
                                                              df$covid_family_find_resources))), 
                                      levels = c("valid_skip", "random_skip", 1, 2, 3, 4), 
                                      labels = c("valid_skip", "random_skip", "Never", "A few times", "About once or twice a week", "Every day or almost every day"), 
                                      ordered = TRUE)

# Transform 'covid_family_teach_additional_topics': Family taught additional topics during COVID closures
df$covid_family_teach_additional_topics <- factor(ifelse(df$covid_family_teach_additional_topics == 99, NA, 
                                                         ifelse(df$covid_family_teach_additional_topics == 97, "random_skip", 
                                                                ifelse(df$covid_family_teach_additional_topics == 95, "valid_skip", 
                                                                       df$covid_family_teach_additional_topics))), 
                                        levels = c("valid_skip", "random_skip", 1, 2, 3, 4), 
                                        labels = c("valid_skip", "random_skip", "Never", "A few times", "About once or twice a week", "Every day or almost every day"), 
                                        ordered = TRUE)

# Transform 'covid_feeling_lonely': Agreement on feeling lonely during COVID closures
df$covid_feeling_lonely <- factor(ifelse(df$covid_feeling_lonely == 99, NA, 
                                         ifelse(df$covid_feeling_lonely == 97, "random_skip", 
                                                ifelse(df$covid_feeling_lonely == 95, "valid_skip", 
                                                       df$covid_feeling_lonely))), 
                                        levels = c("valid_skip", "random_skip", 1, 2, 3, 4), 
                                        labels = c("valid_skip", "random_skip", "Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
                                        ordered = TRUE)

# Transform 'covid_feeling_enjoy_self_learning': Agreement on enjoying self-learning during COVID closures
df$covid_feeling_enjoy_self_learning <- factor(ifelse(df$covid_feeling_enjoy_self_learning == 99, NA, 
                                                      ifelse(df$covid_feeling_enjoy_self_learning == 97, "random_skip", 
                                                             ifelse(df$covid_feeling_enjoy_self_learning == 95, "valid_skip", 
                                                                    df$covid_feeling_enjoy_self_learning))), 
                                        levels = c("valid_skip", "random_skip", 1, 2, 3, 4), 
                                        labels = c("valid_skip", "random_skip", "Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
                                        ordered = TRUE)

# Transform 'covid_teacher_availability': Agreement on teacher availability during COVID closures
df$covid_teacher_availability <- factor(ifelse(df$covid_teacher_availability == 99, NA, 
                                               ifelse(df$covid_teacher_availability == 97, "random_skip", 
                                                      ifelse(df$covid_teacher_availability == 95, "valid_skip", 
                                                             df$covid_teacher_availability))), 
                                        levels = c("valid_skip", "random_skip", 1, 2, 3, 4), 
                                        labels = c("valid_skip", "random_skip", "Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
                                        ordered = TRUE)

# Transform 'covid_feeling_anxious_schoolwork': Agreement on feeling anxious about schoolwork during COVID closures
df$covid_feeling_anxious_schoolwork <- factor(ifelse(df$covid_feeling_anxious_schoolwork == 99, NA, 
                                                     ifelse(df$covid_feeling_anxious_schoolwork == 97, "random_skip", 
                                                            ifelse(df$covid_feeling_anxious_schoolwork == 95, "valid_skip", 
                                                                   df$covid_feeling_anxious_schoolwork))), 
                                        levels = c("valid_skip", "random_skip", 1, 2, 3, 4), 
                                        labels = c("valid_skip", "random_skip", "Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
                                        ordered = TRUE)

# Transform 'covid_feeling_motivated': Agreement on motivation to learn during COVID closures
df$covid_feeling_motivated <- factor(ifelse(df$covid_feeling_motivated == 99, NA, 
                                            ifelse(df$covid_feeling_motivated == 97, "random_skip", 
                                                   ifelse(df$covid_feeling_motivated == 95, "valid_skip", 
                                                          df$covid_feeling_motivated))), 
                                        levels = c("valid_skip", "random_skip", 1, 2, 3, 4), 
                                        labels = c("valid_skip", "random_skip", "Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
                                        ordered = TRUE)

# Transform 'covid_falling_behind': Agreement on falling behind in schoolwork during COVID closures
df$covid_falling_behind <- factor(ifelse(df$covid_falling_behind == 99, NA, 
                                         ifelse(df$covid_falling_behind == 97, "random_skip", 
                                                ifelse(df$covid_falling_behind == 95, "valid_skip", 
                                                       df$covid_falling_behind))), 
                                        levels = c("valid_skip", "random_skip", 1, 2, 3, 4), 
                                        labels = c("valid_skip", "random_skip", "Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
                                        ordered = TRUE)

# Transform 'covid_skills_improved_devices': Agreement on improving digital learning skills during COVID closures
df$covid_skills_improved_devices <- factor(ifelse(df$covid_skills_improved_devices == 99, NA, 
                                                  ifelse(df$covid_skills_improved_devices == 97, "random_skip", 
                                                         ifelse(df$covid_skills_improved_devices == 95, "valid_skip", 
                                                                df$covid_skills_improved_devices))), 
                                        levels = c("valid_skip", "random_skip", 1, 2, 3, 4), 
                                        labels = c("valid_skip", "random_skip", "Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
                                        ordered = TRUE)

# Transform 'covid_teacher_preparedness': Agreement on teachers being prepared for remote instruction during COVID closures
df$covid_teacher_preparedness <- factor(ifelse(df$covid_teacher_preparedness == 99, NA, 
                                               ifelse(df$covid_teacher_preparedness == 97, "random_skip", 
                                                      ifelse(df$covid_teacher_preparedness == 95, "valid_skip", 
                                                             df$covid_teacher_preparedness))), 
                                        levels = c("valid_skip", "random_skip", 1, 2, 3, 4), 
                                        labels = c("valid_skip", "random_skip", "Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
                                        ordered = TRUE)

# Transform 'covid_self_prepared_learning': Agreement on self-preparedness for learning during COVID closures
df$covid_self_prepared_learning <- factor(ifelse(df$covid_self_prepared_learning == 99, NA, 
                                                 ifelse(df$covid_self_prepared_learning == 97, "random_skip", 
                                                        ifelse(df$covid_self_prepared_learning == 95, "valid_skip", 
                                                               df$covid_self_prepared_learning))), 
                                        levels = c("valid_skip", "random_skip", 1, 2, 3, 4), 
                                        labels = c("valid_skip", "random_skip", "Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
                                        ordered = TRUE)

# Transform 'confidence_lms_use': Confidence in using a learning management system
df$confidence_lms_use <- factor(ifelse(df$confidence_lms_use == 99, NA, 
                                       ifelse(df$confidence_lms_use == 97, "random_skip", 
                                              ifelse(df$confidence_lms_use == 95, "valid_skip", 
                                                     df$confidence_lms_use))), 
                                      levels = c("valid_skip", "random_skip", 1, 2, 3, 4), 
                                      labels = c("valid_skip", "random_skip", "Not at all confident", 
                                           "Not very confident", "Confident", "Very confident"), 
                                      ordered = TRUE)

# Transform 'confidence_video_communication_use': Confidence in using a video communication program
df$confidence_video_communication_use <- factor(ifelse(df$confidence_video_communication_use == 99, NA, 
                                                       ifelse(df$confidence_video_communication_use == 97, "random_skip", 
                                                              ifelse(df$confidence_video_communication_use == 95, "valid_skip", 
                                                                     df$confidence_video_communication_use))), 
                                      levels = c("valid_skip", "random_skip", 1, 2, 3, 4), 
                                      labels = c("valid_skip", "random_skip", "Not at all confident", 
                                                           "Not very confident", "Confident", "Very confident"), 
                                      ordered = TRUE)

# Transform 'confidence_finding_learning_resources': Confidence in finding learning resources online
df$confidence_finding_learning_resources <- factor(ifelse(df$confidence_finding_learning_resources == 99, NA, 
                                                          ifelse(df$confidence_finding_learning_resources == 97, "random_skip", 
                                                                 ifelse(df$confidence_finding_learning_resources == 95, "valid_skip", 
                                                                        df$confidence_finding_learning_resources))), 
                                      levels = c("valid_skip", "random_skip", 1, 2, 3, 4), 
                                      labels = c("valid_skip", "random_skip", "Not at all confident", 
                                                              "Not very confident", "Confident", "Very confident"), 
                                      ordered = TRUE)

# Transform 'confidence_planning_schoolwork': Confidence in planning schoolwork independently
df$confidence_planning_schoolwork <- factor(ifelse(df$confidence_planning_schoolwork == 99, NA, 
                                                   ifelse(df$confidence_planning_schoolwork == 97, "random_skip", 
                                                          ifelse(df$confidence_planning_schoolwork == 95, "valid_skip", 
                                                                 df$confidence_planning_schoolwork))), 
                                      levels = c("valid_skip", "random_skip", 1, 2, 3, 4), 
                                      labels = c("valid_skip", "random_skip", "Not at all confident", 
                                                       "Not very confident", "Confident", "Very confident"), 
                                      ordered = TRUE)

# Transform 'confidence_motivation_schoolwork': Confidence in self-motivation for schoolwork
df$confidence_motivation_schoolwork <- factor(ifelse(df$confidence_motivation_schoolwork == 99, NA, 
                                                     ifelse(df$confidence_motivation_schoolwork == 97, "random_skip", 
                                                            ifelse(df$confidence_motivation_schoolwork == 95, "valid_skip", 
                                                                   df$confidence_motivation_schoolwork))), 
                                      levels = c("valid_skip", "random_skip", 1, 2, 3, 4), 
                                      labels = c("valid_skip", "random_skip", "Not at all confident", 
                                                         "Not very confident", "Confident", "Very confident"), 
                                      ordered = TRUE)

# Transform 'confidence_focus_schoolwork': Confidence in focusing on schoolwork without reminders
df$confidence_focus_schoolwork <- factor(ifelse(df$confidence_focus_schoolwork == 99, NA, 
                                                ifelse(df$confidence_focus_schoolwork == 97, "random_skip", 
                                                       ifelse(df$confidence_focus_schoolwork == 95, "valid_skip", 
                                                              df$confidence_focus_schoolwork))), 
                                      levels = c("valid_skip", "random_skip", 1, 2, 3, 4), 
                                      labels = c("valid_skip", "random_skip", "Not at all confident", 
                                                    "Not very confident", "Confident", "Very confident"), 
                                      ordered = TRUE)

# Transform 'confidence_independent_work': Confidence in completing schoolwork independently
df$confidence_independent_work <- factor(ifelse(df$confidence_independent_work == 99, NA, 
                                                ifelse(df$confidence_independent_work == 97, "random_skip", 
                                                       ifelse(df$confidence_independent_work == 95, "valid_skip", 
                                                              df$confidence_independent_work))), 
                                      levels = c("valid_skip", "random_skip", 1, 2, 3, 4), 
                                      labels = c("valid_skip", "random_skip", "Not at all confident", 
                                                    "Not very confident", "Confident", "Very confident"), 
                                      ordered = TRUE)

# Transform 'confidence_assess_progress': Confidence in assessing progress with learning
df$confidence_assess_progress <- factor(ifelse(df$confidence_assess_progress == 99, NA, 
                                               ifelse(df$confidence_assess_progress == 97, "random_skip", 
                                                      ifelse(df$confidence_assess_progress == 95, "valid_skip", 
                                                             df$confidence_assess_progress))), 
                                      levels = c("valid_skip", "random_skip", 1, 2, 3, 4), 
                                      labels = c("valid_skip", "random_skip", "Not at all confident", 
                                                   "Not very confident", "Confident", "Very confident"), 
                                      ordered = TRUE)

# Transform 'confidence_prepared_future_closures': Preparedness for future school closures
df$confidence_prepared_future_closures <- factor(ifelse(df$confidence_prepared_future_closures == 99, NA, 
                                                        ifelse(df$confidence_prepared_future_closures == 95, "valid_skip", 
                                                               df$confidence_prepared_future_closures)), 
                                      levels = c("valid_skip", 1, 2, 3, 4), 
                                      labels = c("valid_skip", "Not prepared at all", "Not very prepared", "Well prepared", "Very well prepared"),
                                      ordered = TRUE)

# Transform 'effort_invested_pisa': Effort put into PISA test
df$effort_invested_pisa <- factor(ifelse(df$effort_invested_pisa == 99, NA, 
                                         df$effort_invested_pisa), 
                                      levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 
                                      ordered = TRUE)

# Transform 'effort_invested_marks_pisa': Effort if PISA counted towards school marks
df$effort_invested_marks_pisa <- factor(ifelse(df$effort_invested_marks_pisa == 99, NA, 
                                               df$effort_invested_marks_pisa), 
                                      levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 
                                      ordered = TRUE)

# Transform 'effort_accurate_pisa': Effort put into giving accurate answers
df$effort_accurate_pisa <- factor(ifelse(df$effort_accurate_pisa == 99, NA, 
                                         df$effort_accurate_pisa), 
                                      levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 
                                      ordered = TRUE)

# ----------------------------
# Debugging Analysis (After Cleaning)
# ----------------------------
debug_after <- data.frame(
  Dataset = "After Cleaning",
  `95` = sum(df[, student_vars] == 95, na.rm = TRUE),  # Should be 0 if cleaned correctly
  valid_skip = sum(df[, student_vars] == "valid_skip", na.rm = TRUE),  # Count transformed valid_skip
  `97` = sum(df[, student_vars] == 97, na.rm = TRUE),  # Should be 0 if cleaned correctly
  random_skip = sum(df[, student_vars] == "random_skip", na.rm = TRUE),  # Count transformed random_skip
  `98` = sum(df[, student_vars] == 98, na.rm = TRUE),  # Should be 0 after transformation
  `99` = sum(df[, student_vars] == 99, na.rm = TRUE),  # Should be 0 after transformation
  Total_NA = sum(is.na(df[, student_vars]))  # Count total NA values
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
write_csv(df, output_path)
write_csv(debug_summary, debug_path)

# Save cleaned dataset as RDS (preserves factors, order, etc.)
rds_output_path <- str_replace(output_path, ".csv$", ".rds")  
saveRDS(df, rds_output_path)

# Completion Message
cat("✅ Student Questionnaire Cleaning Complete!\n")
cat("Cleaned data saved to:", output_path, "\n")
cat("Debugging analysis saved to:", debug_path, "\n")
cat("Cleaned data also saved as RDS to:", rds_output_path, "\n")
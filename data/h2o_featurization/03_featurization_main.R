# ------------
# Libraries
# ------------
library(tidyverse)
library(sqldf)
library(lubridate)

# rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)

# --------------
# Data import 
# --------------

cohort <- read_csv("data\\tables\\01_cohort_w_pred_dts.csv")
attend <- read_csv("data\\tables\\02_feat_attendance.csv")
behavior <- read_csv("data\\tables\\02_feat_behavior.csv")
comorbidities <- read_csv("data\\tables\\02_feat_comorbidities.csv")
demo <- read_csv("data\\tables\\02_feat_demographics.csv")
fr_status <- read_csv("data\\tables\\02_feat_fr_status.csv")
grades <- read_csv("data\\tables\\02_feat_grades.csv")
homeless_status <- read_csv("data\\tables\\02_feat_homeless.csv")
household <- read_csv("data\\tables\\02_feat_household.csv")
outcome <- read_csv("data\\tables\\02_feat_outcome_v2.csv")

# student_id main dataset
student_main <- cohort %>%
  filter(!is.na(student_id)) %>%
  select(-person_id) %>%
  left_join(attend %>% filter(!is.na(student_id)), by = c("student_id", "pred_dt")) %>%
  left_join(behavior %>% select(-person_id) %>% filter(!is.na(student_id)), by = c("student_id", "pred_dt")) %>%
  left_join(comorbidities %>% select(-person_id) %>% filter(!is.na(student_id)), by = c("student_id" = "student_id", "pred_dt" = "pred_dt")) %>%
  left_join(demo %>% select(-enrollment_start, -enrollment_end, -person_id) %>% filter(!is.na(student_id)), by = c("student_id", "pred_dt")) %>%
  left_join(fr_status %>% filter(!is.na(student_id)), by = c("student_id", "pred_dt")) %>%
  inner_join(grades %>% select(-person_id) %>% filter(!is.na(student_id)), by = c("student_id", "pred_dt")) %>% # Need to have grades variables, or we won't predict on
  left_join(homeless_status %>% filter(!is.na(student_id)), by = c("student_id", "pred_dt")) %>%
  left_join(household %>% select(-person_id, -enrollment_start, -enrollment_end, -pred_yr) %>% filter(!is.na(student_id)), by = c("student_id", "pred_dt")) %>%
  left_join(outcome %>% select(-person_id, -enrollment_start, -enrollment_end, -pred_yr, -depress_dt, -min_pred_dt, -stop_dt) %>% filter(!is.na(student_id)), by = c("student_id", "pred_dt")) %>%
  select(-enrollment_start, -enrollment_end, -pred_yr, -birth_date, -age_at_pred_natural, -condition_start, -start_dt, -pred_window_end) %>%
  replace(is.na(.), 0) %>%
  filter(age_at_pred != 0 & yrs_enrolled_in_hopkins_ps != 0)

# person_id main
# Need to do feat engineering by person_id for each category before we can do this


# Stack student_id, person_id
# main_dataset <- bind_rows(student_main, person_main)

write_csv(student_main, "data\\tables\\03_main_dataset_v3.csv")

main_dataset <- student_main

# ---------------------------
# Split into training/test
# ---------------------------

main_dataset <- read_csv("data\\tables\\03_main_dataset_v3.csv")

main_dataset %>%
  mutate(pred_yr = year(pred_dt)) %>%
  group_by(pred_yr) %>%
  count()

outcome_n <- main_dataset %>%
  mutate(pred_yr = year(pred_dt)) %>%
  group_by(pred_yr, depression_dx_in_next_6m) %>%
  count()

main_w_yr <- main_dataset %>%
  mutate(pred_yr = year(pred_dt)) %>%
  select(-(contains("12m")|contains("18m")|contains("24m"))) %>%
  select(student_id,
         pred_dt,
         age_at_pred,
         grade_bucket1_6mo,
         yrs_enrolled_in_hopkins_ps,
         exc_absence_1_5_6mo,
         aa_black,
         grade_bucket4_6mo,
         white,
         exc_absence_11_15_6mo,
         exc_absence_gt15_6mo,
         FR_status,
         exc_tardy_1_5_6mo,
         genderf,
         grade_bucket3_6mo,
         lang_somali,
         sister,
         depression_dx_in_next_6m,
         pred_yr
  )

# Comes from outcome v2
cal <- main_w_yr %>%
  filter(pred_yr == 2009)

train <- main_w_yr %>%
  filter(pred_yr <= 2017 & pred_yr >= 2010)
# 
# cal <- main_w_yr %>%
#   filter(pred_yr == 2017)

# Test
test <- main_w_yr %>%
  filter(pred_yr > 2017 & pred_yr < 2020)

# Without calibration frame
main_n <- main_w_yr %>% count()

train %>%
  group_by(depression_dx_in_next_6m) %>%
  count()

test %>%
  group_by(depression_dx_in_next_6m) %>%
  count()

train_n <- train %>% count()
test_n <- test %>% count()

train_n$n/(train_n$n + test_n$n)
test_n$n/(train_n$n + test_n$n)


# With cal
# train_n <- train %>% count()
# cal_n <- cal %>% count()
# test_n <- test %>% count()
# 
# train_n$n/(train_n$n + test_n$n)
# cal_n$n/(cal_n$n + cal_n$n)
# test_n$n/(train_n$n + test_n$n)
# 
# train %>%
#   group_by(depression_dx_in_next_6m) %>%
#   count()
# 
# cal %>%
#   group_by(depression_dx_in_next_6m) %>%
#   count()
# 
# test %>%
#   group_by(depression_dx_in_next_6m) %>%
#   count()
# 
# train_n <- train %>% count()
# cal_n <- cal %>% count()
# test_n <- test %>% count()
# 
# train_n$n/(train_n$n + cal_n$n + test_n$n)
# cal_n$n/(train_n$n + cal_n$n + test_n$n)
# test_n$n/(train_n$n + cal_n$n + test_n$n)


write_csv(train, "data\\tables\\03_train_v5.csv")
# write_csv(val, "data\\tables\\03_calibration_v2.csv")
write_csv(test, "data\\tables\\03_test_v5.csv")

write_csv(cal, "data\\tables\\03_calibration_v5.csv")

# ------------------------------------------
# Save out main df without IDs for Tetrad
# ------------------------------------------

main_dataset_wo_id <- main_dataset %>%
  select(-student_id, -pred_dt, -person_id)

write_csv(main_dataset_wo_id, "data\\tables\\03_main_sans_id_v2.csv")


# -------------------------
# All features created
# -------------------------

main_for_feats <- read_csv("data\\tables\\03_main_dataset.csv")

all_feats <- data.frame('feature' = colnames(main_for_feats))

write_csv(all_feats, "data\\paper_tables\\all_feats.csv")



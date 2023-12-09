# ---------------------
# Libraries & set-up
# ---------------------

library(tidyverse)
library(sqldf)
library(lubridate)

# --------------
# Data import 
# --------------

cohort <- read_csv("data\\tables\\01_cohort_w_pred_dts.csv")
conditions <- read_csv("clean_data\\health_conditions\\to_use.csv")
condition_class <- read_csv("ancillary_data\\condition_classifications.csv")

# For code quality checks only
enrolled <- read_csv("enrollment\\overall_cleaned_enrollment_updated.csv")

# ------------------------------------------------------------------------------
# Feat engineering: depression dx in 6 months following the prediction date
# ------------------------------------------------------------------------------

depression_dx <- sqldf("
SELECT a.student_id, a.person_id, a.health_condition, a.condition_start_date, a.condition_end_date
FROM conditions a
LEFT JOIN condition_class b
ON a.health_condition = b.health_condition
WHERE b.depressive_disorder = 1
")

# Some students have multiple dx dates so for simplicity we'll take the first dx date only
# No end dates are recorded for any depressive dx'es in dataset
first_depression_dx <- depression_dx %>%
  mutate(condition_start_date = as_date(mdy(condition_start_date))) %>%
  group_by(student_id, person_id) %>%
  summarize(condition_start = min(condition_start_date)) %>%
  ungroup()

cohort_w_depression_dx <- sqldf("SELECT a.*, b.condition_start
                                FROM cohort a
                                LEFT JOIN first_depression_dx b
                                ON a.student_id = b.student_id
                                WHERE a.student_id IS NOT NULL
                                
                                UNION ALL
                                SELECT a.*, b.condition_start
                                FROM cohort a
                                LEFT JOIN first_depression_dx b
                                ON a.person_id = b.person_id
                                WHERE a.student_id IS NULL
      
      ")

# v2: only take first depress dx
cohort_outcome_step1 <- cohort_w_depression_dx %>%
  mutate(#condition_start = as_date(mdy(condition_start)),
         pred_dt = as_date(ymd(pred_dt)),
         pred_window_end = pred_dt + months(12),
         depression_dx_in_next_6m = ifelse(condition_start > pred_dt & condition_start <= pred_window_end, 1, 0),
         depression_dx_in_next_6m = ifelse(is.na(depression_dx_in_next_6m), 0, depression_dx_in_next_6m),
         depression_dx_prior_to_pred_dt = ifelse(condition_start < pred_dt, 1, 0),
         depression_dx_prior_to_pred_dt = ifelse(is.na(depression_dx_prior_to_pred_dt), 0, depression_dx_prior_to_pred_dt),
         depress_dt = as_date((ifelse(depression_dx_in_next_6m == 1, pred_dt, NA)))) %>%
  group_by(student_id) %>%
  mutate(min_pred_dt = as_date(max(depress_dt, na.rm = T))) %>% # get last row with a 1
  ungroup()

cohort_outcome <- cohort_outcome_step1 %>%
  mutate(stop_dt = as_date(ifelse(is.finite(min_pred_dt), min_pred_dt, pred_dt))) %>%
  filter(pred_dt <= stop_dt)

stucheck <- cohort_outcome %>%
  filter(student_id == 101702)

write_csv(cohort_outcome, "Q:\\Data2\\HopkinsPublicSchool\\sull1120\\data\\tables\\causal\\02_feat_outcome.csv")

# v3: consider depression  as unresolved in all cases
cohort_outcome <- cohort_w_depression_dx %>%
  mutate(#condition_start = as_date(mdy(condition_start)),
         pred_dt = as_date(ymd(pred_dt)),
         pred_window_end = pred_dt + months(12),
         depression_dx_in_next_6m = ifelse(condition_start <= pred_window_end, 1, 0),
         depression_dx_in_next_6m = ifelse(is.na(depression_dx_in_next_6m), 0, depression_dx_in_next_6m),
         depression_dx_prior_to_pred_dt = ifelse(condition_start < pred_dt, 1, 0),
         depression_dx_prior_to_pred_dt = ifelse(is.na(depression_dx_prior_to_pred_dt), 0, depression_dx_prior_to_pred_dt))
  
# cohort_outcome <- cohort_outcome_step1 %>%
#   filter(depression_dx_prior_to_pred_dt != 1) # Filter out any rows where depression was dx'ed prior to the pred date; dx has already occurred, so no need for prediction

write_csv(cohort_outcome, "Q:\\Data2\\HopkinsPublicSchool\\sull1120\\data\\tables\\02_feat_outcome_v3.csv")

# ---------------------
# Code quality checks
# ---------------------

# Overall, how many students have a depression dx in next 6m?
cohort_outcome %>%
  group_by(depression_dx_in_next_6m) %>%
  count()

# Students with a dx after enrollment **should** show up with a 1 at some point - so why aren't they?
# (1) Dx occurred before enrollment start (ie they were already dx'ed, and this was reported around the time they enrolled)
# (2) They weren't enrolled in a Hopkins Public School (need to have school ID 12, 13, 14)
# (3) For students where the dx is reported after enrollment, it's generally about a month or two after, so wouldn't fall within a prediction window

# To check that outcome has been correctly coded, the chunk below should return near zero students
# Manually check that the students that do show up are still not within a prediction window date occurring after their enrollment start
students_w_dx_not_in_cohort <- first_depression_dx %>%
  anti_join(cohort_outcome, by = "student_id") %>%
  left_join(cohort %>% distinct(student_id, enrollment_start), by = "student_id") %>%
  left_join(enrolled %>% distinct(student_id, person_id, school_id), by = "student_id") %>%
  filter(school_id %in% c("12", "13", "14") & enrollment_start < condition_start) %>%
  distinct(student_id, enrollment_start, condition_start) %>%
  filter(condition_start > enrollment_start + days(60))


# Check: how many students had a depression dx prior to their index date?
cohort_outcome_step1 %>%
  filter(depression_dx_prior_to_pred_dt == 1) %>%
  distinct(student_id) %>%
  count()



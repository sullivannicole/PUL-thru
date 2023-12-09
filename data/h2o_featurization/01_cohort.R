# ---------------------
# Libraries & set-up
# ---------------------

library(tidyverse)
library(DescTools)
library(lubridate)
library(sqldf)

# --------------
# Data import 
# --------------

enrolled <- read_csv("enrollment\\overall_cleaned_enrollment_updated.csv")

# --------------
# Base cohort
# --------------

# Calculate continuous enrollment dates for middle school and high school students
base_cohort <- enrolled %>%
  filter(school_id %in% c("12", "13", "14")) %>%
  select(student_id, person_id, start_date, end_date, birth_date) %>%
  mutate(birth_date = as_date(mdy(birth_date)),
         start_date = as_date(mdy(start_date)),
         end_date = as_date(mdy(end_date)),
         start_yr = year(start_date),
         end_yr = year(end_date),
         calendar_start = as_date(ifelse(month(start_date) %in% c(8, 9), as_date(ymd(paste0(start_yr, '-08-01'))), start_date)),
         calendar_end = as_date(case_when(month(end_date) %in% c(5, 6) & !is.na(end_yr) ~ as_date(ymd(paste0(end_yr, '-07-31'))),
                                          is.na(end_yr) ~ as_date(ymd(paste0(start_yr + 1, '-07-31'))), # In more recent years (2019 onward), end date is blank, so set to end of school year
                                          TRUE ~ end_date))) %>%
  
  group_by(student_id, person_id) %>%
  arrange(calendar_start) %>%
  mutate(prior_calendar_end = lag(calendar_end),
         non_consec_enrollment = ifelse(prior_calendar_end + days(1) == calendar_start, 0, 1),
         non_consec_enrollment = ifelse(is.na(non_consec_enrollment), 1, 0),
         enrollment_yrs_grp = cumsum(non_consec_enrollment)) %>%
  group_by(student_id, person_id, enrollment_yrs_grp) %>%
  summarize(enrollment_start = min(start_date),
            enrollment_end = as_date(ifelse(is.na(max(end_date)), max(calendar_end), max(end_date)))) %>% # If they have empty enrollment date ends, pick the max inferred end
  ungroup() %>%
  select(-enrollment_yrs_grp)

# Feature dataframe
# Join enrollment dates to times at which we want to generate a prediction
pred_intervals <- data.frame(pred_dt = c(seq.Date(as_date(ymd("2004-07-01")), as_date(ymd("2023-07-30")), by = "6 months"))) %>%
  mutate(pred_yr = year(pred_dt))

# If they were only enrolled for a few days at the beginning of the school year, don't predict for them (hence the exclusion where pred_dt IS NULL)
cohort_w_pred_dts <- sqldf("
      WITH cohort_preds AS (SELECT a.*, b.*
      FROM base_cohort a
      LEFT JOIN pred_intervals b
      ON b.pred_dt BETWEEN a.enrollment_start AND a.enrollment_end)
      
      SELECT *
      FROM cohort_preds
      WHERE pred_dt IS NOT NULL;
      ")

write_csv(cohort_w_pred_dts, "data\\tables\\01_cohort_w_pred_dts.csv")

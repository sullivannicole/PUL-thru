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
fr_lunch <- read_csv("free_reduced\\concatenated_file.csv")

# ---------------
# Engineering
# ---------------

fr_lunch_tidy <- fr_lunch %>%
  distinct(student_id, eligibility, year) %>%
  mutate(fall_yr = ymd(as.numeric(substr(year, 1, 4)), truncated = 2L) + months(6),
         spring_yr = fall_yr + months(6)) %>%
  select(student_id, eligibility, fall_yr, spring_yr) %>%
  gather(fall_yr, spring_yr, key = "season", value = "eligibility_dt") %>%
  select(-season) %>%
  mutate(eligibility_dt = as_date(ymd(eligibility_dt))) %>%
  inner_join(cohort, by = c("student_id" = "student_id", "eligibility_dt" = "pred_dt")) %>%
  mutate()
  # mutate(fr_val = 1,
  #        eligibility_agg = ifelse(eligibility %in% c("F", "R"), "FR_status", "HSA_status")) %>%
  # rename(pred_dt = eligibility_dt) %>%
  # spread(eligibility_agg, value = fr_val) %>%
  # distinct(student_id, pred_dt, FR_status, HSA_status) %>%
  # group_by(student_id, pred_dt) %>%
  # summarize(FR_status = max(FR_status, na.rm = T),
  #           HSA_status = max(HSA_status, na.rm = T))

fr_lunch_agg <- sqldf("SELECT student_id, person_id, eligibility_dt AS pred_dt,
                      MAX(CASE WHEN eligibility IN ('F', 'R') THEN 1 ELSE 0 END) AS FR_status,
                      MAX(CASE WHEN eligibility IN ('H', 'S', 'A') THEN 1 ELSE 0 END) AS HSA_status
                      FROM fr_lunch_tidy
                      GROUP BY 1, 2, 3;")

# Re-convert date
fr_lunch_w_dt <- fr_lunch_agg %>%
  mutate(pred_dt = as_date(pred_dt))

write_csv(fr_lunch_w_dt, "data\\tables\\02_feat_fr_status.csv")

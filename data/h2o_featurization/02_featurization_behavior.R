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
behavior <- read_csv("clean_data\\behavior\\to_use.csv")

# ---------------------
# Feature engineering
# ---------------------

# Create crosswalk of overarching behavioral resolution categories
# Based on resolution rather than behavior since resolution usually indicates severity of behavior better
resolutions <- sqldf("
                     WITH res_unique AS (SELECT DISTINCT resolution_name
                                          FROM behavior)
                                          
                      SELECT resolution_name,
                      CASE WHEN resolution_name = 'Withdraw in lieu of expulsion' THEN 1 ELSE 0 END AS behav_expelled_0_6m,
                      CASE WHEN resolution_name = 'Police Referral' THEN 1 ELSE 0 END AS behav_police_0_6m,
                      CASE WHEN resolution_name LIKE '%OSS%' THEN 1 ELSE 0 END AS behav_oss_0_6m,
                      CASE WHEN resolution_name LIKE '%ISS%' THEN 1 ELSE 0 END AS behav_iss_0_6m,
                      CASE WHEN resolution_name = 'Leave School for Day' THEN 1 ELSE 0 END AS behav_leave_school_0_6m,
                      CASE WHEN resolution_name LIKE '%Detention%' THEN 1 ELSE 0 END AS behav_detention_0_6m,
                      CASE WHEN NOT (resolution_name IN ('Withdraw in lieu of expulsion', 'Police Referral', 'Leave School for Day'))
                      AND NOT (resolution_name LIKE '%OSS%')
                      AND NOT (resolution_name LIKE '%ISS%')
                      AND NOT (resolution_name LIKE '%Detention%')
                      THEN 1 ELSE 0 END AS behav_minor_infraction_0_6m
                      FROM res_unique
                     ")


behavior_tidy <- behavior %>%
  mutate(incident_date = as_date(mdy(incident_date))) %>%
  select(student_id, person_id, incident_date, resolution_name) %>%
  left_join(resolutions, by = c("resolution_name")) %>%
  select(-resolution_name)


behav_w_pred_dts <- sqldf("
                          SELECT b.student_id, b.person_id, a.pred_dt,
                          MAX(behav_expelled_0_6m) AS behav_expelled_0_6m,
                          MAX(behav_police_0_6m) AS behav_police_0_6m,
                          MAX(behav_oss_0_6m) AS behav_oss_0_6m,
                          MAX(behav_iss_0_6m) AS behav_iss_0_6m,
                          MAX(behav_leave_school_0_6m) AS behav_leave_school_0_6m,
                          MAX(behav_detention_0_6m) AS behav_detention_0_6m,
                          MAX(behav_minor_infraction_0_6m) AS behav_minor_infraction_0_6m
                          FROM cohort a
                          INNER JOIN behavior_tidy b
                          ON a.student_id = b.student_id
                          AND DATE(b.incident_date + 2440588) BETWEEN DATE(a.pred_dt + 2440588, '-6 month') AND DATE(a.pred_dt + 2440588)
                          GROUP BY 1, 2, 3
                          ")

# Create more variables farther back in observation period
behav_lagged <- behav_w_pred_dts %>%
  # select(-incident_date) %>%
  group_by(student_id, person_id) %>%
  mutate(behav_expelled_6_12m = lag(behav_expelled_0_6m, order_by = pred_dt, default = 0),
         behav_police_6_12m = lag(behav_police_0_6m, order_by = pred_dt, default = 0),
         behav_oss_6_12m = lag(behav_oss_0_6m, order_by = pred_dt, default = 0),
         behav_iss_6_12m = lag(behav_iss_0_6m, order_by = pred_dt, default = 0),
         behav_leave_school_6_12m = lag(behav_leave_school_0_6m, order_by = pred_dt, default = 0),
         behav_detention_6_12m = lag(behav_detention_0_6m, order_by = pred_dt, default = 0),
         behav_minor_infraction_6_12m = lag(behav_minor_infraction_0_6m, order_by = pred_dt, default = 0),
         
         behav_expelled_12_18m = lag(behav_expelled_0_6m, order_by = pred_dt, default = 0, n = 2),
         behav_polic_12_18m = lag(behav_police_0_6m, order_by = pred_dt, default = 0, n = 2),
         behav_oss_12_18m = lag(behav_oss_0_6m, order_by = pred_dt, default = 0, n = 2),
         behav_iss_12_18m = lag(behav_iss_0_6m, order_by = pred_dt, default = 0, n = 2),
         behav_leave_school_12_18m = lag(behav_leave_school_0_6m, order_by = pred_dt, default = 0, n = 2),
         behav_detention_12_18m = lag(behav_detention_0_6m, order_by = pred_dt, default = 0, n = 2),
         behav_minor_infraction_12_18m = lag(behav_minor_infraction_0_6m, order_by = pred_dt, default = 0, n = 2),
         
         behav_expelled_18_24m = lag(behav_expelled_0_6m, order_by = pred_dt, default = 0, n = 3),
         behav_polic_18_24m = lag(behav_police_0_6m, order_by = pred_dt, default = 0, n = 3),
         behav_oss_18_24m = lag(behav_oss_0_6m, order_by = pred_dt, default = 0, n = 3),
         behav_iss_18_24m = lag(behav_iss_0_6m, order_by = pred_dt, default = 0, n = 3),
         behav_detention_18_24m = lag(behav_detention_0_6m, order_by = pred_dt, default = 0, n = 3),
         behav_minor_infraction_18_24m = lag(behav_minor_infraction_0_6m, order_by = pred_dt, default = 0, n = 3)) %>%
  ungroup()

# QA: Check for duplicates
# behav_lagged %>%
#   group_by(student_id, pred_dt) %>%
#   count() %>%
#   ungroup() %>%
#   filter(n > 1)

write_csv(behav_lagged, "data\\tables\\02_feat_behavior.csv")



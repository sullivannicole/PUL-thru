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
homeless_status <- read_csv("clean_data\\homeless_tag\\concatenated_file_2.csv")

# -------------------
# Feat engineering
# -------------------

homeless_tidy <- homeless_status %>%
  rename(student_id = `Student Number`,
         person_id = `Person ID`) %>%
  mutate(fall_yr = lubridate::ymd(as.numeric(substring(year, 1, 4)), truncated = 2L) + months(6),
         spring_yr = fall_yr + months(6)) %>%
  gather(fall_yr, spring_yr, key = "season", value = "homeless_status_dt")

cohort_homeless <- sqldf("WITH homeless_yr AS (
                                                SELECT student_id, homeless_status_dt, 
                                                -- If they're ever marked as homeless in the year, then consider them as having a bout of
                                                -- homelessness that year
                                                MAX(CASE WHEN Homeless = 'Y' THEN 1 ELSE 0 END) AS homeless_y_in_yr
                                                FROM homeless_tidy
                                                WHERE student_id IS NOT NULL
                                                GROUP BY 1, 2),
                                                
                        homeless_ever AS (SELECT a.student_id, a.pred_dt,
                        MAX(b.homeless_y_in_yr) AS homeless_y_ever
                        FROM cohort a
                        INNER JOIN homeless_yr b
                        ON a.student_id = b.student_id
                        AND a.pred_dt > b.homeless_status_dt
                        GROUP BY 1,2)
                         
                        SELECT DISTINCT a.student_id, a.pred_dt, b.homeless_y_in_yr, c.homeless_y_ever
                        FROM cohort a
                        INNER JOIN homeless_yr b
                        ON a.student_id = b.student_id
                        AND a.pred_dt = b.homeless_status_dt
                        LEFT JOIN homeless_ever c
                        ON a.student_id = c.student_id
                        AND a.pred_dt = c.pred_dt;")


write_csv(cohort_homeless, "data\\tables\\02_feat_homeless.csv")

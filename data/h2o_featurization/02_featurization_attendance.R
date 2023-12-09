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
attend <- read_csv("secondary_attendance\\2022_06_17_secondary_attendance_concatenated.csv")
# excuse_cat <- read_csv("ancillary_data\\excuse_classifications.csv")

# ---------------
# Engineering
# ---------------

# Need to create a crosswalk of excused/unexcused/unknown - one-time
# attendance_excuses <- sqldf("
#       SELECT DISTINCT excuse_description
#       FROM attend
#       ")
# 
# write_csv(attendance_excuses, "ancillary_data\\excuse_classifications.csv")

attend_tidy <- attend %>% mutate(absent_dt = as_date(mdy(date)))


attend_categories <- sqldf("WITH excuse_category AS (
                                           SELECT a.student_id, a.pred_dt, b.period_name, b.absent_dt,
                                           
                                           -- 6 months
                                           CASE 
                                           WHEN b.excuse_code = 'DS' THEN 'detention'
                                           WHEN b.excuse_code = 'Rcp' THEN 'restitution'
                                           WHEN b.excuse_code IN ('EIS', 'EOS', 'ESS', 'ISS', 'SU', 'SUS', 'Sus', 'AE-D', 'AE-S') THEN 'suspension'
                                           WHEN b.status IN ('A', 'P') AND b.excuse IN ('E', 'X') THEN 'exc_absence'
                                           WHEN b.status IN ('A', 'P') AND b.excuse = 'U' THEN 'unexc_absence'
                                           WHEN b.status IN ('A', 'P') AND (b.excuse = 'K' OR excuse IS NULL) THEN 'unkn_absence'
                                           WHEN b.status = 'T' AND b.excuse IN ('E', 'X') THEN 'exc_tardy'
                                           WHEN b.status = 'T' AND b.excuse  = 'U' THEN 'unexc_tardy'
                                           WHEN b.status = 'T' AND (b.excuse = 'K' OR excuse IS NULL) THEN 'unkn_tardy'
                                           WHEN b.status = 'E' AND b.excuse IN ('E', 'X') THEN 'exc_early_release'
                                           WHEN b.status = 'E' AND b.excuse = 'U' THEN 'unexc_early_release'
                                           WHEN b.status = 'E' AND (b.excuse = 'K' or b.excuse IS NULL) THEN 'unkn_early_release'
                                           ELSE 'other' END AS excuse_category_6mo,
                                           
                                           b.status, b.excuse_description, b.excuse, b.excuse_code
                                           
                                           FROM cohort a
                                           LEFT JOIN attend_tidy b
                                           ON a.student_id = b.student_id
                                           AND DATE(b.absent_dt + 2440588) BETWEEN DATE(a.pred_dt + 2440588, '-6 month') AND DATE(a.pred_dt + 2440588)
                                           ),
                   
                   excuse_counts AS (SELECT student_id, pred_dt, excuse_category, COUNT(DISTINCT absent_dt) AS n_absences
                                   FROM excuse_category
                                   GROUP BY 1, 2, 3)
                   
                   SELECT *,
                   CASE WHEN n_absences = 0 THEN '_0'
                   WHEN n_absences BETWEEN 1 AND 5 THEN '_1_5'
                   WHEN n_absences BETWEEN 6 AND 10 THEN '_6_10'
                   WHEN n_absences BETWEEN 11 AND 15 THEN '_11_15'
                   ELSE '_gt15'
                   END AS 'n_absence_bucket'
                   FROM excuse_counts;
                  ")

attend_wide <- attend_categories %>%
  filter(!is.na(student_id)) %>%
  mutate(attend_bucket = paste0(excuse_category, n_absence_bucket, "_6mo"),
         attend_value = 1) %>%
  select(-excuse_category, -n_absence_bucket, -n_absences) %>%
  pivot_wider(names_from = attend_bucket, values_from = attend_value) %>%
  replace(is.na(.), 0)

write_csv(attend_wide, "data\\tables\\02_feat_attendance.csv")





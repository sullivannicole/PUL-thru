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

# ------------------
# Feat engineering
# ------------------

conditions_tidy <- conditions %>%
  mutate(condition_start_date = as_date(mdy(condition_start_date))) %>%
  inner_join(condition_class, by = c("health_condition")) %>%
  # Since most of the comorbidities we're considering are chronic, ignore end date and just look to see if they had a dx at all prior to pred date
  select(-condition_status, -treatment, -depressive_disorder, -health_condition, -condition_end_date)

comorbidities <- sqldf("SELECT b.student_id, b.person_id, a.pred_dt,
                        MAX(psychiatric_disorder_non_depressive) AS psychiatric_disorder_non_depressive,
                        MAX(stress_or_depression_related) AS stress_or_depression_related,
                        MAX(general_inflammatory_disease) AS general_inflammatory_disease,
                        MAX(autoimmune_disease) AS autoimmune_disease,
                        MAX(substance_related) AS substance_related,
                        MAX(self_harm) AS self_harm
                        FROM cohort a 
                        INNER JOIN conditions_tidy b 
                        ON a.student_id = b.student_id
                        AND b.condition_start_date < a.pred_dt
                        GROUP BY 1, 2, 3")

# QA: check for duplicates
# comorbidities %>%
#   group_by(student_id, pred_dt) %>%
#   count() %>%
#   ungroup() %>%
#   filter(n > 1)

write_csv(comorbidities, "data\\tables\\02_feat_comorbidities.csv")

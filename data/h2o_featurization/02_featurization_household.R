# ------------
# Libraries
# ------------

library(tidyverse)
library(sqldf)
library(lubridate)

# --------------
# Data import 
# --------------

cohort <- read_csv("data\\tables\\01_cohort_w_pred_dts.csv")
household <- read_csv("clean_data\\household\\2022_05_12_household_concatenated_raw.csv")

# ---------------
# Engineering
# ---------------
hh_mbrs_count <- sqldf("SELECT student_studentNumber, personID,
                    CASE WHEN TRIM(LOWER(Relationship)) = 'sibling' AND ContactID = 'M' THEN 'brother'
                    WHEN TRIM(LOWER(Relationship)) = 'sibling' AND ContactID = 'F' THEN 'sister'
                    WHEN REPLACE(REPLACE(LOWER(Relationship), '-', '_'), ' ', '_') IN ('mother', 'father', 'brother', 'sister', 'step_father', 'step_mother', 'spouse', 'grandparent', 'foster_parent')
                    THEN REPLACE(REPLACE(LOWER(Relationship), '-', '_'), ' ', '_') 
                    ELSE 'other' END AS relationship,
                    year, SUBSTRING(year, 1, 4) AS start_yr, COUNT(*) AS n_mbrs
                    
                    FROM household
                    WHERE Relationship != 'Self'
                    GROUP BY 1, 2, 3, 4, 5;")

hh_mbrs_wide <- hh_mbrs_count %>%
  pivot_wider(names_from = "relationship", values_from = "n_mbrs") %>%
  filter(!(is.na(student_studentNumber) & is.na(personID))) %>% # Get rid of rows where both person ID and student ID are NA
  mutate(across(brother:spouse, ~replace_na(.x, 0)),
         end_yr = as.numeric(start_yr)+1,
         start_dt = as_date(ymd(paste0(start_yr, "-07-01"))),
         end_dt = as_date(ymd(paste0(end_yr, "-01-01"))))


# Get most recent year's data; assume it's updated on a yearly basis, at the beginning of the school year
# Therefore, data for the 2005-2006 year isn't available as of the 2005-07-01 prediction
cohort_hh <- sqldf("
      SELECT * (SELECT a.*, b.start_dt, b.brother, b.sister, b.father, b.mother, b.step_father, b.step_mother, b.grandparent, b.foster_parent, b.spouse, b.other,
      RANK() OVER (PARTITION BY a.student_id, a.pred_dt ORDER BY b.start_dt DESC) AS recency_rank_relative_to_pred_tm
      FROM cohort a
      LEFT JOIN hh_mbrs_wide b
      ON a.student_id = b.student_studentNumber
      WHERE a.student_id IS NOT NULL
      AND b.start_dt < a.pred_dt) WHERE recency_rank_relative_to_pred_tm = 1
    
    
      UNION
      
      SELECT * FROM (SELECT a.*, b.start_dt, b.brother, b.sister, b.father, b.mother, b.step_father, b.step_mother, b.grandparent, b.foster_parent, b.spouse, b.other,
      RANK() OVER (PARTITION BY a.student_id, a.pred_dt ORDER BY b.start_dt DESC) AS recency_rank_relative_to_pred_tm
      FROM cohort a
      LEFT JOIN hh_mbrs_wide b
      ON a.person_id = b.personID
      WHERE a.student_id IS NULL
      AND b.start_dt < a.pred_dt) WHERE recency_rank_relative_to_pred_tm = 1
      ")

cohort_hh_final <- cohort_hh %>%
  select(-recency_rank_relative_to_pred_tm)

write_csv(cohort_hh_final, "data\\tables\\02_feat_household.csv")

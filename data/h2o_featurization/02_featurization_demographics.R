# ---------------------
# Libraries & set-up
# ---------------------

library(tidyverse)
library(sqldf)

# --------------
# Data import 
# --------------

# length of time enrolled in Hopkins school district at time of prediction
cohort <- read_csv("data\\tables\\01_cohort_w_pred_dts.csv")

# age at time of prediction, race/ethnicity (Fed) - much more complete than demos data
enrolled <- read_csv("enrollment\\overall_cleaned_enrollment_updated.csv")

# language spoken at home, sex at birth
demos <- read_csv("demographics\\2022_06_17_demographics_by_student.csv")

# ---------------
# Engineering
# ---------------

# One-hot encode demographics
# Code both male and female genders, since 25% of the pop = unknown gender; gender in demographics file takes precedence
# since it's more complete; if it's missing then we go to the enrollment file

# For language, enrollment is more complete so it takes precedence
cohort_demo <- sqldf("
      SELECT a.student_id, a.person_id, a.enrollment_start, a.enrollment_end, a.pred_dt, b.birth_date,  
      MAX(CASE WHEN b.race_ethnicity_fed = 1 THEN 1 ELSE 0 END) AS hispanic,
      MAX(CASE WHEN b.race_ethnicity_fed = 2 THEN 1 ELSE 0 END) AS ai_alaskan_native,
      MAX(CASE WHEN b.race_ethnicity_fed = 3 THEN 1 ELSE 0 END) AS asian,
      MAX(CASE WHEN b.race_ethnicity_fed = 4 THEN 1 ELSE 0 END) AS aa_black,
      MAX(CASE WHEN b.race_ethnicity_fed = 5 THEN 1 ELSE 0 END) AS native_hawaiian_pi,
      MAX(CASE WHEN b.race_ethnicity_fed = 6 THEN 1 ELSE 0 END) AS white,
      MAX(CASE WHEN b.race_ethnicity_fed = 7 THEN 1 ELSE 0 END) AS multiracial,
      
      MAX(CASE WHEN c.gender = 'F' THEN 1 ELSE 0 END) AS genderf,
      MAX(CASE WHEN c.gender = 'M' THEN 1 ELSE 0 END) AS genderm,
      
      MAX(CASE WHEN b.home_primary_language = 11 THEN 1 
      WHEN b.home_primary_language IS NULL AND c.language = 'English' THEN 1
      ELSE 0 END) AS lang_english,
      MAX(CASE WHEN b.home_primary_language = 45 THEN 1
      WHEN b.home_primary_language IS NULL AND c.language = 'Spanish' THEN 1
      ELSE 0 END) AS lang_spanish,
      MAX(CASE WHEN b.home_primary_language = 69 THEN 1
      WHEN b.home_primary_language IS NULL AND c.language = 'Somali' THEN 1
      ELSE 0 END) AS lang_somali
      
      FROM cohort a
      LEFT JOIN enrolled b
      ON a.student_id = b.student_id
      LEFT JOIN demos c
      ON a.student_id = c.student_id
      WHERE a.student_id IS NOT NULL
      GROUP BY 1, 2, 3, 4, 5, 6
      
      UNION
      
      SELECT a.student_id, a.person_id, a.enrollment_start, a.enrollment_end, a.pred_dt, b.birth_date, 
      MAX(CASE WHEN b.race_ethnicity_fed = 1 THEN 1 ELSE 0 END) AS hispanic,
      MAX(CASE WHEN b.race_ethnicity_fed = 2 THEN 1 ELSE 0 END) AS ai_alaskan_native,
      MAX(CASE WHEN b.race_ethnicity_fed = 3 THEN 1 ELSE 0 END) AS asian,
      MAX(CASE WHEN b.race_ethnicity_fed = 4 THEN 1 ELSE 0 END) AS aa_black,
      MAX(CASE WHEN b.race_ethnicity_fed = 5 THEN 1 ELSE 0 END) AS native_hawaiian_pi,
      MAX(CASE WHEN b.race_ethnicity_fed = 6 THEN 1 ELSE 0 END) AS white,
      MAX(CASE WHEN b.race_ethnicity_fed = 7 THEN 1 ELSE 0 END) AS multiracial,
      
      MAX(CASE WHEN c.gender = 'F' THEN 1 ELSE 0 END) AS genderf,
      MAX(CASE WHEN c.gender = 'M' THEN 1 ELSE 0 END) AS genderm,
      
      MAX(CASE WHEN b.home_primary_language = 11 THEN 1
      WHEN b.home_primary_language IS NULL AND c.language = 'English' THEN 1
      ELSE 0 END) AS lang_english,
      MAX(CASE WHEN b.home_primary_language = 45 THEN 1
      WHEN b.home_primary_language IS NULL AND c.language = 'Spanish' THEN 1
      ELSE 0 END) AS lang_spanish,
      MAX(CASE WHEN b.home_primary_language = 69 THEN 1
      WHEN b.home_primary_language IS NULL AND c.language = 'Somali' THEN 1
      ELSE 0 END) AS lang_somali
      
      FROM cohort a
      LEFT JOIN enrolled b
      ON a.person_id = b.person_id
      LEFT JOIN demos c
      ON a.student_id = c.student_id
      WHERE a.student_id IS NULL
      GROUP BY 1, 2, 3, 4, 5, 6
                         ")

# QA: Check that no one has both genders at once
# cohort_demo %>%
#   filter(genderf == 1 & genderm == 1)
# 
# 
# check_gender <- sqldf("
#                       with genders as (select distinct student_id, genderf, genderm
#                       from cohort_demo)
#                       
#                       select student_id, count(distinct genderf) as n_genderf, genderm
#                       from genders
#                       group by 1
#                       having count(distinct genderf) > 1 or count(distinct genderm) > 1
#                       
#                       ")

cohort_demo_full <- cohort_demo %>%
  unique() %>%
  mutate(birth_date = as_date(mdy(birth_date)),
         age_at_pred = interval(birth_date, pred_dt)/years(1),
         age_at_pred_natural = trunc(age_at_pred),
         yrs_enrolled_in_hopkins_ps = interval(enrollment_start, pred_dt)/years(1))

write_csv(cohort_demo_full, "data\\tables\\02_feat_demographics.csv")

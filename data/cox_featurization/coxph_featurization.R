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

enrolled <- read_csv("overall_cleaned_enrollment_updated.csv")

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
      WHERE pred_dt IS NOT NULL
      AND student_id IS NOT NULL;
      ")

# --------------
# Attendance
# --------------

attend <- read_csv("2022_06_17_secondary_attendance_concatenated.csv")

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
                                           ELSE 'attend_other' END AS excuse_category_6mo,
                                           
                                           b.status, b.excuse_description, b.excuse, b.excuse_code
                                           
                                           FROM cohort_w_pred_dts a
                                           LEFT JOIN attend_tidy b
                                           ON a.student_id = b.student_id
                                           AND DATE(b.absent_dt + 2440588) BETWEEN DATE(a.pred_dt + 2440588, '-6 month') AND DATE(a.pred_dt + 2440588)
                                           )
                   
                   SELECT student_id, pred_dt, excuse_category_6mo, COUNT(DISTINCT absent_dt) AS n_absences
                    FROM excuse_category
                    GROUP BY 1, 2, 3
                  ")

attend_wide <- attend_categories %>%
  filter(!is.na(student_id)) %>%
  pivot_wider(names_from = excuse_category_6mo, values_from = n_absences) %>%
  replace(is.na(.), 0)

# --------------
# Behavior
# --------------

behavior <- read_csv("gopher_data\\clean_data\\behavior\\to_use.csv")

resolutions <- sqldf("
                     WITH res_unique AS (SELECT DISTINCT resolution_name
                                          FROM behavior)
                                          
                      SELECT resolution_name,
                      CASE WHEN resolution_name = 'Withdraw in lieu of expulsion' THEN 'expelled'
                      WHEN resolution_name = 'Police Referral' THEN 'police'
                      WHEN resolution_name LIKE '%OSS%' THEN 'oss'
                      WHEN resolution_name LIKE '%ISS%' THEN 'iss'
                      WHEN resolution_name = 'Leave School for Day' THEN 'leave_school'
                      WHEN resolution_name LIKE '%Detention%' THEN 'detention'
                      ELSE 'minor_infraction'
                      END AS resolution_category
                      FROM res_unique
                     ")

behavior_tidy <- behavior %>%
  mutate(incident_date = as_date(mdy(incident_date))) %>%
  select(student_id, person_id, incident_date, resolution_name) %>%
  left_join(resolutions, by = c("resolution_name")) %>%
  select(-resolution_name)

behav_w_pred_dts <- sqldf("
                          SELECT b.student_id, b.person_id, a.pred_dt, b.incident_date, b.resolution_category
                          FROM cohort_w_pred_dts a
                          INNER JOIN behavior_tidy b
                          ON a.student_id = b.student_id
                          AND DATE(b.incident_date + 2440588) BETWEEN DATE(a.pred_dt + 2440588, '-6 month') AND DATE(a.pred_dt + 2440588)
                          GROUP BY 1, 2, 3
                          ")

behav_counts <- behav_w_pred_dts %>%
  group_by(student_id, pred_dt, resolution_category) %>%
  count() %>%
  pivot_wider(id_cols = c(student_id, pred_dt), names_from = resolution_category, values_from = n) %>%
  replace(is.na(.), 0)

# -----------------
# Comorbidities
# -----------------

conditions <- read_csv("Workspace\\gopher_data\\clean_data\\health_conditions\\to_use.csv")
condition_class <- read_csv("ancillary_data\\condition_classifications.csv")

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
                        FROM cohort_w_pred_dts a 
                        INNER JOIN conditions_tidy b 
                        ON a.student_id = b.student_id
                        AND b.condition_start_date < a.pred_dt
                        GROUP BY 1, 2, 3")


# -----------------
# Demographics
# -----------------

# language spoken at home, sex at birth
demos <- read_csv("Workspace\\gopher_data\\clean_data\\demographics\\2022_06_17_demographics_by_student.csv")

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
      
      FROM cohort_w_pred_dts a
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
      
      FROM cohort_w_pred_dts a
      LEFT JOIN enrolled b
      ON a.person_id = b.person_id
      LEFT JOIN demos c
      ON a.student_id = c.student_id
      WHERE a.student_id IS NULL
      GROUP BY 1, 2, 3, 4, 5, 6
                         ")


cohort_demo_full <- cohort_demo %>%
  filter(!is.na(student_id)) %>%
  unique() %>%
  mutate(birth_date = as_date(mdy(birth_date)),
         age_at_pred = interval(birth_date, pred_dt)/years(1),
         age_at_pred_natural = trunc(age_at_pred),
         yrs_enrolled_in_hopkins_ps = interval(enrollment_start, pred_dt)/years(1))


# -----------------
# F/R status
# -----------------

fr_lunch <- read_csv("Workspace\\gopher_data\\clean_data\\free_reduced\\concatenated_file.csv")

fr_lunch_tidy <- fr_lunch %>%
  distinct(student_id, eligibility, year) %>%
  filter(!is.na(student_id)) %>%
  mutate(fall_yr = ymd(as.numeric(substr(year, 1, 4)), truncated = 2L) + months(6),
         spring_yr = fall_yr + months(6)) %>%
  select(student_id, eligibility, fall_yr, spring_yr) %>%
  gather(fall_yr, spring_yr, key = "season", value = "eligibility_dt") %>%
  select(-season) %>%
  mutate(eligibility_dt = as_date(ymd(eligibility_dt))) %>%
  inner_join(cohort_w_pred_dts, by = c("student_id" = "student_id", "eligibility_dt" = "pred_dt"))

fr_lunch_agg <- sqldf("SELECT student_id, person_id, eligibility_dt AS pred_dt,
                      MAX(CASE WHEN eligibility IN ('F', 'R') THEN 1 ELSE 0 END) AS FR_status,
                      MAX(CASE WHEN eligibility IN ('H', 'S', 'A') THEN 1 ELSE 0 END) AS HSA_status
                      FROM fr_lunch_tidy
                      GROUP BY 1, 2, 3;")

# Re-convert date
fr_lunch_w_dt <- fr_lunch_agg %>%
  mutate(pred_dt = as_date(pred_dt))


# -----------
# Grades
# -----------

gpa <- read_csv("Workspace\\gopher_data\\clean_data\\gpa\\term gpa\\cleaned_termgpa.csv")
grades <- read_csv("gopher_data\\clean_data\\grades\\secondary\\cleaned_sec_grades.csv")
term_key <- read_csv("Workspace\\gopher_data\\documentation_files\\2022_05_12_term_key.csv")

grades_w_categories <- grades %>%
  select(student_id, year, course_name, term_name, rask, score) %>%
  mutate(score_bucket = case_when(score %in% c("A+", "A", "A-", 7, 6) ~"A_grades",
                                  score %in% c("B", "B+", "B-", 5, 4) ~ "B_grades",
                                  score %in% c("C", "C-", "C+", 3) ~ "C_grades",
                                  score %in% c("D", "D-", "D+", 2) ~ "D_grades",
                                  score %in% c("F", 1) ~ "F_grades",
                                  TRUE ~ "grades_other"))


duplicate_grades <- sqldf("SELECT student_id, year, course_name, term_name, rask, COUNT(DISTINCT score_bucket) AS n_course_ids
                          FROM grades_w_categories
                          GROUP BY 1, 2, 3, 4, 5
                          HAVING n_course_ids > 1
                          ;")

dups_deduped <- sqldf("WITH grade_cnts AS (SELECT student_id, year, course_name, term_name, rask, score_bucket, COUNT(score_bucket) OVER (PARTITION BY student_id, year, course_name, term_name, rask) AS n_course_ids
                          FROM grades_w_categories),
                          
                        duped_grades AS (SELECT a.*
                          FROM grade_cnts a
                          -- Only pull rows that represent a true duplicate (some might have 2+ course IDs, but all the same grades, which is fine since we do DISTINCT to pull score)
                          INNER JOIN duplicate_grades b 
                          ON a.student_id = b.student_id
                          AND a.year = b.year
                          AND a.course_name = b.course_name
                          AND a.term_name = b.term_name
                          AND a.rask = b.rask
                          WHERE a.n_course_ids > 1)
                          
                        SELECT student_id, year, course_name, term_name, rask, MAX(score_bucket) AS score_bucket
                        FROM duped_grades
                        GROUP BY 1, 2, 3, 4, 5
                          ;
                  
                  ")

# Get only the "final" grades (semester or term finals) and stack on top of de-duplicated grades for the duplicated observations
course_grades <- sqldf("SELECT DISTINCT a.student_id, a.year, a.course_name, a.term_name, a.rask, a.score_bucket
                         FROM grades_w_categories a
                         LEFT JOIN duplicate_grades b 
                         ON a.student_id = b.student_id
                         AND a.year = b.year
                         AND a.course_name = b.course_name
                         AND a.term_name = b.term_name
                         AND a.rask = b.rask
                         WHERE a.rask LIKE 'Semester %' OR a.rask LIKE 'Term%'
                         --anti-join duplicated grades to pull them out
                        AND b.student_id IS NULL
                       
                       UNION ALL
                       SELECT *
                       FROM dups_deduped;")

course_grades_tidy <- course_grades %>%
  separate(year, into = c("start_yr", "end_yr"), sep = "-") %>%
  mutate(start_yr = as.numeric(start_yr),
         end_yr = as.numeric(end_yr))

term_key_tidy <- term_key %>%
  mutate(start_dt = as_date(mdy_hm(startDate)),
         end_dt = as_date(mdy_hm(endDate)),
         start_month = month(start_dt),
         start_yr = year(start_dt),
         connect_to = ifelse(start_month >= 8, start_yr, start_yr - 1))

grades_w_dts <- sqldf("
                      SELECT DISTINCT a.*, b.start_dt, b.end_dt
                      FROM course_grades_tidy a 
                      LEFT JOIN term_key_tidy b 
                      ON a.start_yr = b.connect_to
                      AND a.term_name = b.name
                      ")

grades_0_6mo <- sqldf("WITH grades_long AS (
                                            SELECT DISTINCT a.*, b.course_name, b.rask, b.score_bucket, DATE(a.pred_dt + 2440588, '-6 month') AS index_6mo
                                            FROM cohort_w_pred_dts a
                                            INNER JOIN grades_w_dts b 
                                            ON a.student_id = b.student_id
                                            AND DATE(b.end_dt + 2440588) BETWEEN DATE(a.pred_dt + 2440588, '-6 month') AND DATE(a.pred_dt + 2440588)
                                            ),
                      
                      n_courses_w_grade AS (SELECT student_id, person_id, pred_dt, score_bucket, COUNT(*) AS n_courses_w_score
                                            FROM grades_long
                                            GROUP BY 1, 2, 3, 4),
                      
                      tot_courses AS (SELECT *, SUM(n_courses_w_score) OVER (PARTITION BY student_id, person_id, pred_dt) AS tot_courses_prior_6mo
                      FROM n_courses_w_grade)
                      
                      SELECT student_id, person_id, pred_dt, score_bucket, CAST(n_courses_w_score AS REAL)/CAST(tot_courses_prior_6mo AS REAL) AS pc_of_courses_prior_6mo
                      FROM tot_courses;")

grades_wide <- grades_0_6mo %>%
  filter(!is.na(student_id)) %>%
  pivot_wider(id_cols = c(student_id, pred_dt), names_from = score_bucket, values_from = pc_of_courses_prior_6mo) %>%
  replace(is.na(.), 0)

# ---------------
# Homeless
# ---------------

homeless_status <- read_csv("Workspace\\gopher_data\\clean_data\\homeless_tag\\concatenated_file_2.csv")

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
                        FROM cohort_w_pred_dts a
                        INNER JOIN homeless_yr b
                        ON a.student_id = b.student_id
                        AND a.pred_dt > b.homeless_status_dt
                        GROUP BY 1,2)
              
                        SELECT DISTINCT a.student_id, a.pred_dt, b.homeless_y_in_yr, c.homeless_y_ever
                        FROM cohort_w_pred_dts a
                        INNER JOIN homeless_yr b
                        ON a.student_id = b.student_id
                        AND a.pred_dt = b.homeless_status_dt
                        LEFT JOIN homeless_ever c
                        ON a.student_id = c.student_id
                        AND a.pred_dt = c.pred_dt;")


# -------------
# Household
# -------------

household <- read_csv("Workspace\\gopher_data\\clean_data\\household\\2023_06_26\\2023_06_26_household_concatenated_raw.csv")

check_mbrs <- household %>%
  distinct(Relationship)

hh_mbrs_count <- sqldf("SELECT `Student Number` as student_id,
                    CASE WHEN TRIM(LOWER(Relationship)) = 'sibling' AND Gender = 'M' THEN 'brother'
                    WHEN TRIM(LOWER(Relationship)) = 'sibling' AND Gender = 'F' THEN 'sister'
                    WHEN REPLACE(REPLACE(LOWER(Relationship), '-', '_'), ' ', '_') IN ('mother', 'father', 'brother', 'sister', 'step_father', 'step_mother', 'spouse', 'grandparent', 'foster_parent')
                    THEN REPLACE(REPLACE(LOWER(Relationship), '-', '_'), ' ', '_') 
                    ELSE 'other' END AS relationship,
                    year, SUBSTRING(year, 1, 4) AS start_yr, COUNT(*) AS n_mbrs
                    
                    FROM household
                    WHERE Relationship != 'Self'
                    AND `Student Number` IS NOT NULL
                    GROUP BY 1, 2, 3, 4;")

hh_mbrs_wide <- hh_mbrs_count %>%
  pivot_wider(names_from = "relationship", values_from = "n_mbrs") %>%
  filter(!(is.na(student_id))) %>% # Get rid of rows where both person ID and student ID are NA
  mutate(across(brother:spouse, ~replace_na(.x, 0)),
         end_yr = as.numeric(start_yr)+1,
         start_dt = as_date(ymd(paste0(start_yr, "-07-01"))),
         end_dt = as_date(ymd(paste0(end_yr, "-01-01"))))

cohort_hh <- sqldf("
      SELECT * FROM (SELECT a.*, b.start_dt, b.brother, b.sister, b.father, b.mother, b.step_father, b.step_mother, b.grandparent, b.foster_parent, b.spouse, b.other,
      RANK() OVER (PARTITION BY a.student_id, a.pred_dt ORDER BY b.start_dt DESC) AS recency_rank_relative_to_pred_tm
      FROM cohort_w_pred_dts a
      LEFT JOIN hh_mbrs_wide b
      ON a.student_id = b.student_id
      WHERE a.student_id IS NOT NULL
      AND b.start_dt < a.pred_dt) WHERE recency_rank_relative_to_pred_tm = 1
      ")


cohort_hh_final <- cohort_hh %>%
  select(-recency_rank_relative_to_pred_tm)

# -------------
# Outcome
# -------------

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
                                FROM cohort_w_pred_dts a
                                LEFT JOIN first_depression_dx b
                                ON a.student_id = b.student_id
                                WHERE a.student_id IS NOT NULL;
      
      ")

cohort_outcome_step1 <- cohort_w_depression_dx %>%
  mutate(#condition_start = as_date(mdy(condition_start)),
    pred_dt = as_date(ymd(pred_dt)),
    pred_window_end = pred_dt + months(6),
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

# ---------
# Main
# ---------

student_main <- cohort_w_pred_dts %>%
  filter(!is.na(student_id)) %>%
  select(-person_id) %>%
  left_join(attend_wide %>% filter(!is.na(student_id)), by = c("student_id", "pred_dt")) %>%
  left_join(behav_counts %>% filter(!is.na(student_id)), by = c("student_id", "pred_dt")) %>%
  left_join(comorbidities %>% select(-person_id) %>% filter(!is.na(student_id)), by = c("student_id" = "student_id", "pred_dt" = "pred_dt")) %>%
  left_join(cohort_demo_full %>% select(-enrollment_start, -enrollment_end, -person_id) %>% filter(!is.na(student_id)), by = c("student_id", "pred_dt")) %>%
  left_join(fr_lunch_w_dt %>% filter(!is.na(student_id)), by = c("student_id", "pred_dt")) %>%
  inner_join(grades_wide %>% filter(!is.na(student_id)), by = c("student_id", "pred_dt")) %>% # Need to have grades variables, or we won't predict on
  left_join(cohort_homeless %>% filter(!is.na(student_id)), by = c("student_id", "pred_dt")) %>%
  left_join(cohort_hh_final %>% select(-person_id, -enrollment_start, -enrollment_end, -pred_yr) %>% filter(!is.na(student_id)), by = c("student_id", "pred_dt")) %>%
  left_join(cohort_outcome %>% select(-person_id, -enrollment_start, -enrollment_end, -pred_yr, -min_pred_dt, -stop_dt) %>% filter(!is.na(student_id)), by = c("student_id", "pred_dt")) %>%
  select(-enrollment_start, -enrollment_end, -pred_yr, -birth_date, -condition_start, -start_dt, -pred_window_end)

depress_cohort <- student_main %>%
  group_by(student_id) %>%
  mutate(max_pred_dt = as_date(ymd(max(pred_dt))),
         max_depress_dt = as_date(ymd(max(depress_dt, na.rm = T)))) %>%
  ungroup()

depress_cohort2 <- depress_cohort %>%
  mutate(stop_dt = coalesce(max_depress_dt, max_pred_dt)) %>%
  filter(pred_dt <= stop_dt) %>% # once the dx (event) occurs, stop making predictions
  mutate_if(is.numeric, replace_na, replace = 0) %>%
  filter(age_at_pred != 0 & yrs_enrolled_in_hopkins_ps != 0) %>%
  group_by(student_id) %>%
  arrange(pred_dt) %>%
  mutate(row_n = row_number()) %>%
  ungroup() %>%
  mutate(tstop = row_n*6,
         tstart = tstop-6) %>%
  select(-row_n)

write_csv(depress_cohort2, "data\\coxph_featurization\\main.csv")

# Use back-testing instead
# train <- student_main %>%
#   filter(year(pred_dt) <= 2017)
# 
# write_csv(train, "data\\coxph_featurization\\train.csv")
# 
# test <- student_main %>%
#   filter(year(pred_dt) > 2017 & year(pred_dt) < 2020)
# 
# write_csv(test, "data\\coxph_featurization\\test.csv")
# 
# test %>%
#   group_by(depression_dx_in_next_6m) %>%
#   count()

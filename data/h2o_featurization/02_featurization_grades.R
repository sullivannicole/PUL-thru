# ---------------------
# Libraries & set-up
# ---------------------
library(tidyverse)
library(lubridate)
library(sqldf)

# --------------
# Data import 
# --------------

cohort <- read_csv("data\\tables\\01_cohort_w_pred_dts.csv")

gpa <- read_csv("gpa\\term gpa\\cleaned_termgpa.csv")
# grades <- read_csv("grades\\secondary\\Old files\\cleaned_transcript.csv")
grades <- read_csv("clean_data\\grades\\secondary\\cleaned_sec_grades.csv")
term_key <- read_csv("documentation_files\\2022_05_12_term_key.csv")

# ---------------
# Engineering
# ---------------

# Put grades into ordinal categories so (1) we can deduplicate course grades with the same grade and (2) so we can combine similar grades into
# broader collections: high/low/middle/pass/unknown
grades_w_categories <- sqldf("WITH combined_score AS (
                                                  SELECT student_id, year, course_name, term_name, rask, score, `grading.scoreListItemName`, COALESCE(`grading.scoreListItemName`, score) AS score2
                                                  FROM grades)
                          
                          
                              SELECT *, 
                              CASE WHEN score2 IN ('A+', 'A', 'A-', 'B+', 'B', 'B-', 'Excelling', 'Mastery', 'Exceeds Expectations', 'IB 5', 'IB 6', 'IB 7', 'IB 8')
                              THEN 1
                              WHEN score2 IN ('Pass', 'P') THEN 2
                              WHEN score2 IN ('D+', 'D', 'D-', 'F', 'Insufficient Progress to Pass', 'IB 0', 'IB 1', 'IB 2', 'IB 3', 'Does Not Meet', 'Beginning') THEN 4
                              WHEN score2 IN ('No Credit', 'NC', 'Not Assessed', 'No Evidence', 'GW', 'I', 'W') OR `grading.scoreListItemName` IS NULL THEN 5
                              ELSE 3 END AS score_bucket
                              FROM combined_score
                                            ")

# There are ~2k students that were enrolled for the same course twice in one term, but with different course IDs AND a different grade in the 2 diff
# course IDs
# Thought this could be due to an associated lab (?) but many of the courses don't strike me as lab-associated courses:
# e.g. 'LCC - Language Arts T4', 'PBL World Studies - ORA', 'Spanish 1'
# Resolution: take the higher of the 2 grades since it's more likely that grades are adjusted up rather than down at the end
duplicate_grades <- sqldf("SELECT student_id, year, course_name, term_name, rask, COUNT(DISTINCT score_bucket) AS n_course_ids
                          FROM grades_w_categories
                          GROUP BY 1, 2, 3, 4, 5
                          HAVING n_course_ids > 1
                          ;")

# Get the grades for each observation with a duplication
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
         connect_to = ifelse(start_month >= 8, start_yr, start_yr - 1)) # This field connects to the start year in course_grades_tidy

# Join term start/end dates using school year in course grades df
grades_w_dts <- sqldf("
                      SELECT DISTINCT a.*, b.start_dt, b.end_dt
                      FROM course_grades_tidy a 
                      LEFT JOIN term_key_tidy b 
                      ON a.start_yr = b.connect_to
                      AND a.term_name = b.name
                      ")

# Term had to have ended on or after the time window of interest starts (to give us the freshest grades possible in that time window)
# See: stackoverflow.com/questions/39241030/r-date-function-in-sqldf-giving-unusual-answer-wrong-date-format
# for why the 2440588 gets added
# INNER JOIN because students with no past data (newly enrolled or younger) will just show up with NAs - can encode these later by virtue
# of them not being in this df
grades_0_6mo <- sqldf("WITH grades_long AS (
                                            SELECT DISTINCT a.*, b.course_name, b.rask, b.score_bucket, DATE(a.pred_dt + 2440588, '-6 month') AS index_6mo
                                            FROM cohort a
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
  mutate(score_bucket = paste0("grade_bucket", score_bucket, "_6mo")) %>%
  pivot_wider(id_cols = c(student_id, person_id, pred_dt), names_from = score_bucket, values_from = pc_of_courses_prior_6mo) %>%
  replace(is.na(.), 0) %>%
  group_by(student_id, person_id) %>%
  mutate(grade_bucket1_12mo = lag(grade_bucket1_6mo, order_by = pred_dt, default = 0),
         grade_bucket2_12mo = lag(grade_bucket2_6mo, order_by = pred_dt, default = 0),
         grade_bucket3_12mo = lag(grade_bucket3_6mo, order_by = pred_dt, default = 0),
         grade_bucket4_12mo = lag(grade_bucket4_6mo, order_by = pred_dt, default = 0),
         grade_bucket5_12mo = lag(grade_bucket5_6mo, order_by = pred_dt, default = 0),
         grade_bucket1_18mo = lag(grade_bucket1_6mo, order_by = pred_dt, default = 0, n = 2),
         grade_bucket2_18mo = lag(grade_bucket2_6mo, order_by = pred_dt, default = 0, n = 2),
         grade_bucket3_18mo = lag(grade_bucket3_6mo, order_by = pred_dt, default = 0, n = 2),
         grade_bucket4_18mo = lag(grade_bucket4_6mo, order_by = pred_dt, default = 0, n = 2),
         grade_bucket5_18mo = lag(grade_bucket5_6mo, order_by = pred_dt, default = 0, n = 2),
         grade_bucket1_24mo = lag(grade_bucket1_6mo, order_by = pred_dt, default = 0, n = 3),
         grade_bucket2_24mo = lag(grade_bucket2_6mo, order_by = pred_dt, default = 0, n = 3),
         grade_bucket3_24mo = lag(grade_bucket3_6mo, order_by = pred_dt, default = 0, n = 3),
         grade_bucket4_24mo = lag(grade_bucket4_6mo, order_by = pred_dt, default = 0, n = 3),
         grade_bucket5_24mo = lag(grade_bucket5_6mo, order_by = pred_dt, default = 0, n = 3)) %>%
  ungroup()

write_csv(grades_wide, "data\\tables\\02_feat_grades.csv")

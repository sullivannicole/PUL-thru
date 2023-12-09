library(tidyverse)
library(janitor)
library(lubridate)
library(stringr)
library(yardstick)
# library(mice)
# library(arules)

# ----------------------------------------------------------------------------
# 1. Cohort: students who took the Panorama SEL comp assessment in 2022
# ----------------------------------------------------------------------------

selcomp <- read_csv("Q:\\Data2\\HopkinsPublicSchool\\Workspace\\gopher_data\\clean_data\\assessment\\panorama\\cleaned_panorama_selcomp_assessment.csv") %>%
  clean_names()

conditions <- read_csv("Q:\\Data2\\HopkinsPublicSchool\\Workspace\\gopher_data\\clean_data\\health_conditions\\to_use.csv")

conditions_xwalk <- read_csv("Q:\\Data2\\HopkinsPublicSchool\\sull1120\\ancillary_data\\condition_classifications.csv")

depression <- conditions %>%
  left_join(conditions_xwalk, by = "health_condition") %>%
  filter(depressive_disorder == 1) %>%
  group_by(student_id) %>%
  summarize(condition_start = min(as_date(mdy(condition_start_date)))) %>%
  ungroup() %>%
  filter(year(condition_start) > 2019) %>% # future values of depression only # can we predict depression at all? # used in v1
  mutate(depressive_disorder = 1)

cohort_outcome <- selcomp %>%
  filter(!is.na(student_id)) %>%
  left_join(depression, by = "student_id") %>%
  mutate(depressive_disorder = ifelse(is.na(depressive_disorder), 0, 1)) %>%
  select(student_id, grade, gender, race, how_confident_are_you_that_you_can_complete_all_the_work_that_is_assigned_in_your_classes_values, depressive_disorder) %>%
  rename(workload_comp_conf = how_confident_are_you_that_you_can_complete_all_the_work_that_is_assigned_in_your_classes_values)

# --------------------
# 2. Demographics
# --------------------

demos <- read_csv("Q:\\Data2\\HopkinsPublicSchool\\Workspace\\gopher_data\\clean_data\\demographics\\2022_06_17_demographics_by_student.csv")

cohort_demos <- cohort_outcome %>%
  left_join(demos, by = "student_id") %>%
  mutate(race_missing = ifelse(is.na(race) & is.na(race_ethnicity_code), 1, 0),
         gender_missing = ifelse(is.na(gender.x) & is.na(gender.y), 1, 0),
         gender.x = case_when(gender.x == "Male" ~ "M", 
                              gender.x == "Female" ~ "F",
                              TRUE ~ gender.x)) %>%
  filter(race_missing == 0 & gender_missing == 0) %>%
  mutate(race = ifelse(is.na(race), race_ethnicity_code, race),
         gender = ifelse(is.na(gender.y), gender.x, gender.y),
         language = case_when(is.na(language) ~ 'not_recorded',
                              language == "English" ~ "English",
                              TRUE ~ "other")) %>%
  select(student_id, workload_comp_conf, depressive_disorder, race, gender, language, grade)


# ---------------------
# 3. Socioeconomics
# ---------------------

fr_lunch <- read_csv("Q:\\Data2\\HopkinsPublicSchool\\Workspace\\gopher_data\\clean_data\\free_reduced\\concatenated_file.csv")

fr_status <- fr_lunch %>%
  filter(end_year == 2020) %>%
  select(-year, -end_year) %>%
  unique() %>%
  mutate(fr_value = 1) %>%
  spread(eligibility, value = fr_value, fill = 0)

cohort_fr <- cohort_demos %>%
  left_join(fr_status, by = "student_id") %>%
  mutate(`F` = ifelse(is.na(`F`), 0, 1),
         R = ifelse(is.na(R), 0, 1),
         S = ifelse(is.na(S), 0, 1))

# -------------------
# 4. Absences
# -------------------

attend <- read_csv("Q:\\Data2\\HopkinsPublicSchool\\Workspace\\gopher_data\\clean_data\\secondary_attendance\\2022_06_17_secondary_attendance_concatenated.csv")

attendance <- attend %>%
  filter(year == '2019-20' &  status %in% c('A', 'P') & excuse %in% c('E', 'X', 'U')) %>%
  mutate(absence_type = ifelse(excuse == 'U', 'unexcused', 'excused')) %>%
  group_by(student_id, absence_type) %>%
  count() %>%
  ungroup() %>%
  spread(absence_type, value = n, fill = 0)

cohort_attend <- cohort_fr %>%
  left_join(attendance, by = "student_id") %>%
  mutate(excused = ifelse(is.na(excused), 0, excused),
         unexcused = ifelse(is.na(unexcused), 0, unexcused))


# -------------------
# 5. Grades
# -------------------

grades <- read_csv("Q:\\Data2\\HopkinsPublicSchool\\Workspace\\gopher_data\\clean_data\\grades\\secondary\\cleaned_sec_grades.csv")

grade_summary <- grades %>%
  filter(year == "2019-2020" & (str_detect(rask, '^Semester ') | str_detect(rask, '^Term'))) %>%
  group_by(student_id) %>%
  add_tally(name = "total_courses") %>%
  ungroup() %>%
  filter(!is.na(score) & !(score %in% c("NC", "NC-Att", "P"))) %>%
  mutate(grade_family = case_when(score %in% c("A", "A-") ~"A_grades",
                                  score %in% c("B", "B+", "B-") ~ "B_grades",
                                  score %in% c("C", "C-", "C+") ~ "C_grades",
                                  score %in% c("D", "D-", "D+") ~ "D_grades",
                                  score == "F" ~ "F_grades",
                                  TRUE ~ score)) %>%
  group_by(student_id, total_courses, grade_family) %>%
  count() %>%
  ungroup() %>%
  spread(grade_family, value = n, fill = 0)

cohort_grades <- cohort_attend %>%
  left_join(grade_summary, by = "student_id") %>%
  replace(is.na(.), 0)

md.pattern(cohort_grades)

write_csv(cohort_grades, "Q:\\Data2\\HopkinsPublicSchool\\sull1120\\data\\panorama_featurization\\main.csv")

set.seed(626)
train <- cohort_grades %>% slice_sample(prop = 0.75)
test <- cohort_grades %>% anti_join(train)

# Try imputing values for # of courses and course grades

# mice_imputation <- mice(train %>% select(-student_id), m = 5, print = F, seed = 626, method = "cart")
# test_mids <- mice.mids(mice_imputation, newdata = test %>% select(-student_id))
# train_mids <- mice.mids(mice_imputation, newdata = train %>% select(-student_id))
# 
# test_imputed <- complete(test_mids, 5)
# train_imputed <- complete(train_mids, 5)
# 
# train_full <- train %>%
#   select(-total_courses, -A_grades, -B_grades, -C_grades, -D_grades, -F_grades) %>%
#   cbind(train_imputed %>% select(total_courses, A_grades, B_grades, C_grades, D_grades, F_grades))
# 
# test_full <- test %>%
#   select(-total_courses, -A_grades, -B_grades, -C_grades, -D_grades, -F_grades) %>%
#   cbind(test_imputed %>% select(total_courses, A_grades, B_grades, C_grades, D_grades, F_grades))

write_csv(train, "Q:\\Data2\\HopkinsPublicSchool\\sull1120\\data\\panorama_featurization\\train.csv")
write_csv(test, "Q:\\Data2\\HopkinsPublicSchool\\sull1120\\data\\panorama_featurization\\test.csv")
# 
# main <- read_csv("Q:\\Data2\\HopkinsPublicSchool\\sull1120\\data\\panorama_featurization\\main.csv")

# Try downsampling
# train0 <- train %>%
#   filter(depressive_disorder == 0) %>%
#   slice_sample(prop = 0.25)
# 
# train_ds <- train %>%
#   filter(depressive_disorder == 1) %>%
#   rbind(train0)

cohort_grades %>%
  group_by(depressive_disorder) %>%
  count()

# T-test to compare means of responses for this q
main <- cohort_grades %>% filter(workload_comp_conf != 0)
t.test(workload_comp_conf ~ depressive_disorder, data = main) #alternative = "greater"

# Logistic regression
train_logit <- train %>% 
  filter(workload_comp_conf != 0) %>%
  select(-student_id, -R, -S) %>%
  mutate(depressive_disorder = as.factor(depressive_disorder))

yellow_logit <- glm(depressive_disorder ~ ., data = train_logit, family = "binomial")

summary(yellow_logit)

test_logit <- test %>% mutate(depressive_disorder = as.factor(depressive_disorder))
test_preds <- predict(yellow_logit, newdata = test_logit, type = "response")
test_logit_pred <- test_logit %>% mutate(prob = test_preds)

ggplot(test_logit_pred, aes(depressive_disorder, prob, color = depressive_disorder, fill = depressive_disorder)) +
  # stat_smooth(method = "glm", method.args = list(family = binomial), se = F) +
  geom_boxplot(width = 0.1, alpha = 0.5) +
  theme(legend.position = "bottom")

ggplot(test_logit_pred, aes(workload_comp_conf, prob, color = depressive_disorder)) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  # geom_smooth(se = F, method = 'lm') +
  # ylim(0, 0.22) +
  theme(legend.position = "bottom")

test_metrics <- test_logit_pred %>%
  arrange(-prob) %>%
  mutate(pred_rank = row_number()) %>%
  mutate(depress = as.numeric(depressive_disorder)-1,
         num_tps_at_rank = cumsum(depress)) %>%
  group_by(num_tps_at_rank) %>%
  mutate(first_detection = max(prob)) %>%
  ungroup() %>%
  mutate(predict = as.factor(ifelse(prob >= 0.032109036, 1, 0)),
         depressive_disorder = as.factor(depressive_disorder))

test_metrics %>%
  filter(predict == 1 & depressive_disorder == 1) %>%
  count()

# Look at true positive rankings
tps <- test_metrics %>%
  filter(first_detection == prob)

# Training %iles
train_probs <- predict(yellow_logit, newdata = train_logit, type = "response")

test_roc <- roc_auc(test_metrics, truth = depressive_disorder, estimate = prob, event_level = "second")
test_acc <- accuracy(test_metrics, truth = depressive_disorder, estimate = predict, event_level = "second")
test_prec <- precision(test_metrics, truth = depressive_disorder, estimate = predict, event_level = "second")
test_recall <- recall(test_metrics, truth = depressive_disorder, estimate = predict, event_level = "second")
test_pr <- pr_auc(test_metrics, truth = depressive_disorder, estimate = prob, event_level = "second")

# F1
2*(test_prec$.estimate*test_recall$.estimate)/(test_prec$.estimate+test_recall$.estimate)

# train_probs_df <- data.frame(prob = train_probs) %>%
#   mutate(pctile = ntile(prob, 100)) %>%
#   group_by(pctile) %>%
#   summarize(min_prob = min(prob),
#             max_prob = max(prob))
# 
# train_pctiles <- quantile(train_probs, seq(0, 1, by = 0.01))



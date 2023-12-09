# --------------------
# Libraries & data
# --------------------

library(tidyverse)
library(survival)
library(yardstick)
library(probably)

# train <- read_csv("<<path>>")
# test <- read_csv("<<path>>")
main <- read_csv("<<path>>")

main <- depress_cohort2
options(digits = 3)

# --------------------
# Back-testing
# --------------------

test_dts <- seq.Date(as_date(ymd('2015-01-01')), as_date(ymd('2019-07-01')), by = '6 months')

# Initialize results vectors
roc_ls <- vector("list", length(test_dts))
prc_ls <- vector("list", length(test_dts))
roc_curve_ls <- vector("list", length(test_dts))
prc_curve_ls <- vector("list", length(test_dts))
acc_roc_ls <- vector("list", length(test_dts))
acc_prc_ls <- vector("list", length(test_dts))
prec_roc_ls <- vector("list", length(test_dts))
prec_prc_ls <- vector("list", length(test_dts))
recall_roc_ls <- vector("list", length(test_dts))
recall_prc_ls <- vector("list", length(test_dts))
confusion_roc_ls <- vector("list", length(test_dts))
confusion_prc_ls <- vector("list", length(test_dts))
test_preds_ls <- vector("list", length(test_dts))


for(i in 1:length(test_dts)){
  
  train <- main %>% filter(pred_dt < test_dts[i])
  test <- main %>% filter(pred_dt == test_dts[i]) # 1-step ahead forecast

  # depress_coxph <- coxph(Surv(tstart, tstop, depression_dx_in_next_6m) ~ exc_absence + exc_tardy + unexc_absence + unexc_tardy + lang_english + genderf +
  #                          psychiatric_disorder_non_depressive + minor_infraction + police + homeless_y_in_yr +
  #                          white + age_at_pred_natural + yrs_enrolled_in_hopkins_ps + FR_status + A_grades + F_grades +
  #                          father, data = train, ties = "breslow")
  
  depress_coxph <- coxph(Surv(tstart, tstop, depression_dx_in_next_6m) ~ exc_absence + lang_english + genderf +
                           minor_infraction + aa_black +
                           white + age_at_pred_natural + yrs_enrolled_in_hopkins_ps + FR_status + A_grades + F_grades + grades_other, 
                          data = train, ties = "breslow")
  
  
  # Test predictions
  test_pred <- predict(depress_coxph, newdata = test, type = 'expected')

  # Test performance
  test_concord <- concordance(depress_coxph, newdata = test)
  
  test_pred_df <- test %>%
    mutate(predict = test_pred) %>%
    mutate(tps_at_row = cumsum(depression_dx_in_next_6m),
           depression_dx_in_next_6m = as.factor(depression_dx_in_next_6m),
           no_depress_prob = exp(-predict),
           depress_prob = 1-no_depress_prob)
  
  test_roc <- roc_auc(test_pred_df, truth = depression_dx_in_next_6m, estimate = predict, event_level = "second")
  test_prc <- pr_auc(test_pred_df, truth = depression_dx_in_next_6m, estimate = predict, event_level = "second")
  test_roc_thresholds <- probably::threshold_perf(test_pred_df, depression_dx_in_next_6m, predict, thresholds = seq(min(test_pred), max(test_pred), 0.0005), event_level = "second")
  test_prc_thresholds <- yardstick::pr_curve(test_pred_df, depression_dx_in_next_6m, predict, event_level = "second") %>%
    mutate(f1 = 2/((1/precision) + (1/recall)),
           max_f1 = max(f1))
  
  optimal_roc_thresh <- test_roc_thresh_df <- test_roc_thresholds %>%
    spread(.metric, value = .estimate) %>%
    mutate(max_j = max(j_index)) %>%
    filter(max_j == j_index)
  
  # Get optimal thresholds
  optimal_roc_thresh <- test_roc_thresh_df %>%
    filter(max_j == j_index)
  
  optimal_prc_thresh <- test_prc_thresholds %>%
    filter(max_f1 == f1)
  
  test_preds_binary <- test_pred_df %>%
    mutate(predict_binary_roc = as.factor(ifelse(predict < optimal_roc_thresh$.threshold, 0, 1)),
           predict_binary_prc = as.factor(ifelse(predict < optimal_prc_thresh$.threshold, 0, 1)),
           depression_dx_in_next_6m = as.factor(depression_dx_in_next_6m))
  
  acc_roc <- yardstick::accuracy(test_preds_binary, depression_dx_in_next_6m, predict_binary_roc, event_level = "second")
  acc_prc <- yardstick::accuracy(test_preds_binary, depression_dx_in_next_6m, predict_binary_prc, event_level = "second")
  prec_roc <- yardstick::precision(test_preds_binary, truth = depression_dx_in_next_6m, estimate = predict_binary_roc, event_level = "second")
  prec_prc <- yardstick::precision(test_preds_binary, truth = depression_dx_in_next_6m, estimate = predict_binary_prc, event_level = "second")
  recall_roc <- yardstick::recall(test_preds_binary, truth = depression_dx_in_next_6m, estimate = predict_binary_roc, event_level = "second")
  recall_prc <- yardstick::recall(test_preds_binary, truth = depression_dx_in_next_6m, estimate = predict_binary_prc, event_level = "second")
  # auprc <- yardstick::pr_auc(test_preds_binary, truth = depression_dx_in_next_6m, estimate = )
  
  confusion_roc <- test_preds_binary %>%
    group_by(depression_dx_in_next_6m, predict_binary_roc) %>%
    count() %>%
    ungroup() %>%
    mutate(pred_category = case_when(depression_dx_in_next_6m == 0 & predict_binary_roc == 0 ~ 'tn',
                                     depression_dx_in_next_6m == 0 & predict_binary_roc == 1 ~ 'fp',
                                     depression_dx_in_next_6m == 1 & predict_binary_roc == 1 ~ 'tp',
                                     depression_dx_in_next_6m == 1 & predict_binary_roc == 0 ~ 'fn')) %>%
    select(pred_category, n) %>%
    spread(pred_category, value = n)
  
  confusion_prc <- test_preds_binary %>%
    group_by(depression_dx_in_next_6m, predict_binary_prc) %>%
    count() %>%
    ungroup() %>%
    mutate(pred_category = case_when(depression_dx_in_next_6m == 0 & predict_binary_prc == 0 ~ 'tn',
                                     depression_dx_in_next_6m == 0 & predict_binary_prc == 1 ~ 'fp',
                                     depression_dx_in_next_6m == 1 & predict_binary_prc == 1 ~ 'tp',
                                     depression_dx_in_next_6m == 1 & predict_binary_prc == 0 ~ 'fn')) %>%
    select(pred_category, n) %>%
    spread(pred_category, value = n)
  
  roc_ls[[i]] <- test_roc$.estimate
  prc_ls[[i]] <- test_prc$.estimate
  roc_curve_ls[[i]] <- test_roc_thresholds
  prc_curve_ls[[i]] <- test_prc_thresholds
  acc_roc_ls[[i]] <- acc_roc$.estimate
  acc_prc_ls[[i]] <- acc_prc$.estimate
  prec_roc_ls[[i]] <- prec_roc$.estimate
  prec_prc_ls[[i]] <- prec_prc$.estimate
  recall_roc_ls[[i]] <- recall_roc$.estimate
  recall_prc_ls[[i]] <- recall_prc$.estimate
  confusion_roc_ls[[i]] <- confusion_roc
  confusion_prc_ls[[i]] <- confusion_prc
  test_preds_ls[[i]] <- test_preds_binary
}

# -------------
# Results
# -------------

# Spring metrics
mean(unlist(roc_ls)[c(TRUE, FALSE)])
mean(unlist(prc_ls)[c(TRUE, FALSE)])
mean(unlist(prec_roc_ls)[c(TRUE, FALSE)])
mean(unlist(recall_roc_ls)[c(TRUE, FALSE)])

# Fall metrics
mean(unlist(roc_ls)[c(FALSE, TRUE)])
mean(unlist(prc_ls)[c(FALSE, TRUE)])
mean(unlist(prec_roc_ls)[c(FALSE, TRUE)])
mean(unlist(recall_roc_ls)[c(FALSE, TRUE)])

tps_prc <- map_dbl(confusion_prc_ls, function(x) x$tp)
fps_prc <- map_dbl(confusion_prc_ls, function(x) x$fp)
fns_prc <- map_dbl(confusion_prc_ls, function(x) x$fn)

data.frame(dt = test_dts,
           tps = tps_prc,
           fps = fps_prc,
           fns = fns_prc) %>%
  gather(tps, fps, fns, key = "conf_cat", value = "n") %>%
  ggplot(aes(dt, n)) +
  geom_bar(stat = "identity") +
  facet_wrap(~conf_cat, scales = "free")

tps_roc <- map_dbl(confusion_roc_ls, function(x) x$tp)
fps_roc <- map_dbl(confusion_roc_ls, function(x) x$fp)

data.frame(dt = test_dts,
           tps = tps_roc,
           fps = fps_roc) %>%
  gather(tps, fps, key = "conf_cat", value = "n") %>%
  ggplot(aes(dt, n)) +
  geom_bar(stat = "identity") +
  facet_wrap(~conf_cat, scales = "free")

ggplot() +
  aes(test_dts, unlist(roc_ls)) +
  # geom_line() +
  geom_area(color = "blue", fill = "blue", alpha = 0.2) +
  geom_point(color = "blue") +
  ylim(0, 1) +
  labs(title = "Back-tested AUROC")

unlist(prc_ls)
ggplot() +
  aes(test_dts, unlist(prc_ls)) +
  geom_line() +
  geom_point() +
  geom_area(color = "blue", fill = "blue", alpha = 0.2) +
  ylim(0, 0.05) +
  labs(title = "Back-tested AUPRC")

ggplot() +
  aes(test_dts, unlist(prec_roc_ls)) +
  geom_line() +
  geom_point() +
  geom_area(color = "blue", fill = "blue", alpha = 0.2) +
  labs(title = "Back-tested precision")

ggplot() +
  aes(test_dts, unlist(recall_roc_ls)) +
  geom_line() +
  geom_point() +
  ylim(0, 1) +
  labs(title = "Back-tested recall") +
  geom_area(color = "blue", fill = "blue", alpha = 0.2)


# Mean AUROC
unlist(roc_ls)
mean(unlist(roc_ls))
min(unlist(roc_ls))
max(unlist(roc_ls))

# Mean AUPRC
unlist(prc_ls)
mean(unlist(prc_ls))

# Mean precision - PRC
unlist(prec_prc_ls)
mean(unlist(prec_prc_ls))

# Mean recall - PRC
unlist(recall_prc_ls)
mean(unlist(recall_prc_ls))

# Mean precision - ROC
unlist(prec_roc_ls)
mean(unlist(prec_roc_ls))
min(unlist(prec_roc_ls))
max(unlist(prec_roc_ls))

# Mean recall - ROC
unlist(recall_roc_ls)
mean(unlist(recall_roc_ls))
min(unlist(recall_roc_ls))
max(unlist(recall_roc_ls))

# Mean accuracy - PRC
unlist(acc_prc_ls)
mean(unlist(acc_prc_ls))

# Mean accuracy - ROC
unlist(acc_roc_ls)
mean(unlist(acc_roc_ls))
min(unlist(acc_roc_ls))
max(unlist(acc_roc_ls))

# F1 - ROC
f1s <- map2_dbl(unlist(recall_roc_ls), unlist(prec_roc_ls), function(recall, prec) (2*recall*prec)/(recall+prec))
mean(f1s)
min(f1s)
max(f1s)

# Confusion
conf_roc <- bind_rows(confusion_roc_ls)
mean(conf_roc$tn)
mean(conf_roc$fp)
mean(conf_roc$fn, na.rm = T)
mean(conf_roc$tp)

# PRAUC
test_preds_all_df <- bind_rows(test_preds_ls)
pr_auc_vec(test_preds_all_df$depression_dx_in_next_6m, test_preds_all_df$predict, event_level = "second")


# Individual predictions
individ_preds <- test_preds_ls[5][[1]] %>% # 2017-01-01
  filter(predict_binary_prc == 1 & depression_dx_in_next_6m == 1) 

individ_pred_ls <- vector("list", length(test_dts))
individ_pred_df_ls <- vector("list", length(test_dts))

for(i in 1:length(test_dts)) {
  case_study <- test_preds_ls[i][[1]]%>%
    filter(student_id == 103866)
  
  individ_pred_ls[[i]] <- case_study$depress_prob
  individ_pred_df_ls[[i]] <- case_study
}

ggplot() +
  aes(test_dts[1:4], unlist(individ_pred_ls)) +
  geom_point() +
  geom_area(alpha = 0.2, color = "blue")

individ_pred_df <- main %>%
  filter(student_id == 103866)

ggplot(individ_pred_df, aes(pred_dt, A_grades)) +
  geom_point() +
  geom_line() +
  ylim(0, 1) +
  labs(y = '% of courses in prior semester \nin which student achieved an A')

ggplot(individ_pred_df, aes(pred_dt, exc_absence)) +
  geom_point() +
  geom_line() +
  ylim(0, 17.5)

# Let's look at the prediction probabilities over time for all positive individuals
all_preds_df <- bind_rows(test_preds_ls)

individ_preds_df <- all_preds_df %>%
  group_by(student_id) %>%
  mutate(depressed_ever = as.factor(max(as.numeric(as.character(depression_dx_in_next_6m))))) %>%
  ungroup()

unique(individ_preds_df$depressed_ever)

individ_preds_df %>%
  # filter(depressed_ever == 1) %>%
  group_by(student_id) %>%
  arrange(pred_dt) %>%
  mutate(previous_pred = lag(predict),
         one_pred_dt_prior = lead(depression_dx_in_next_6m)) %>%
  ungroup() %>%
  # filter(one_pred_dt_prior == 1 | depression_dx_in_next_6m == 1) %>%
ggplot(aes(pred_dt, predict, group = student_id, color = depression_dx_in_next_6m)) +
  geom_line(alpha = 0.3) +
  geom_point(alpha = 0.3) +
  facet_wrap(~depressed_ever) +
  theme(legend.position = "bottom") +
  ylim(0, 0.3)

individ_preds_df %>%
  # filter(depressed_ever == 1) %>%
  group_by(student_id) %>%
  arrange(pred_dt) %>%
  mutate(previous_pred = lag(predict),
         one_pred_dt_prior = lead(depression_dx_in_next_6m)) %>%
  ungroup() %>%
  filter(one_pred_dt_prior == 1 | depression_dx_in_next_6m == 1) %>%
  ggplot(aes(pred_dt, predict, group = student_id, color = depression_dx_in_next_6m)) +
  geom_line(alpha = 0.3) +
  geom_point(alpha = 0.3) +
  facet_wrap(~depressed_ever) +
  theme(legend.position = "bottom") +
  ylim(0, 0.3)

individ_preds_df %>%
  ggplot(aes(depressed_ever, predict)) +
  geom_boxplot(width = 0.3) +
  ylim(0, 0.2)

individ_preds_df %>%
  ggplot(aes(predict, fill = depression_dx_in_next_6m, color = depression_dx_in_next_6m)) +
  geom_density(alpha = 0.4) +
  xlim(0, 0.1) +
  theme(legend.position = "bottom")

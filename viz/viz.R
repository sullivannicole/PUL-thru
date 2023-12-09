# ---------------
# Packages
# ---------------

library(tidyverse)
library(lubridate)
library(scales)
library(svglite)
library(ggchicklet)

# ---------------
# Aesthetics
# ---------------
salmon_light <- "#ffa4a9"
salmon <- "#ff767c"
pink <- "#ee528b"
purple_light <- "#a970ff"
# purple_dark <- "#36136b"
purple_dark <- "#4f1f96"
blue_bright <- "#0062ff"
blue_light <- "#2fb1ff"
teal_light <- "#00bab6"
teal_dark <- "#006161"
teal_lighter <- "#87eded"
green <- "#56d679"
grey_425 <- "#54585a"
blue_dark <- "#005a70"


theme_bar_narrow <- theme(panel.background = element_rect(fill = "transparent", color = "transparent"),
                          axis.ticks = element_blank(),
                          plot.title = element_text(color = grey_425, face = "bold", size = 16),
                          axis.title = element_text(color = grey_425, size = 16),
                          axis.text = element_text(size = 13, color = grey_425),
                          legend.position = "bottom",
                          legend.title = element_text(color = grey_425, size = 15),
                          legend.text = element_text(color = grey_425, size = 13),
                          legend.key = element_rect(fill = "white"))

theme_bar <- theme(panel.background = element_rect(fill = "transparent", color = "transparent"),
                   axis.ticks = element_blank(),
                   plot.title = element_text(color = blue_dark, face = "bold", size = 25),
                   axis.title = element_text(color = grey_425, size = 20),
                   axis.text = element_text(size = 20, color = grey_425),
                   legend.position = "bottom",
                   legend.title = element_text(color = grey_425, size = 20),
                   legend.text = element_text(color = grey_425),
                   legend.key = element_rect(fill = "white"))

setwd("/Users/nicolesullivan/Documents/Academic/2021-2023/MS_in_DS/Capstone/Hopkins_PS/")

# --------------
# AUROC
# --------------
h2o_roc <- read_csv('results/champions_roc_curves_full.csv')
pul_roc <- read_csv('results/20231129_1030/20231128_roc.csv')
cox_roc <- read_csv('results/20231129_1030/coxph_roc_curves.csv')

cox_roc_tidy <- cox_roc %>%
  mutate(fpr = 1-specificity,
         tpr = sensitivity,
         model_name = paste0('Cox-', substr(test_dt, 3, 7))) %>%
  select(tpr, fpr, model_name)
  
pul_roc_tidy <- pul_roc %>%
  mutate(model_name = paste0('PUL-', substr(test_dt, 3, 7))) %>%
  select(tpr, fpr, model_name)

h2o_roc_tidy <- h2o_roc %>%
  mutate(model_type = str_extract(model_id, "GBM|Deep|StackedEnsemble"),
         model_num = str_extract(model_id, "_41|_22|grid_2|grid_3|_46|_54"),
         model_name = case_when(model_type == 'Deep' & model_num == "grid_2" ~ "ANN-2",
                                model_type == 'Deep' & model_num == "grid_3" ~ "ANN-3",
                                model_type == "StackedEnsemble" ~ "StackedEnsemble",
                                TRUE ~ paste0(model_type, str_replace_all(model_num, "_", "-")))) %>%
  select(tpr, fpr, model_name)

roc_curves <- bind_rows(cox_roc_tidy, pul_roc_tidy, h2o_roc_tidy)

mod_ord <- sort(unique(roc_curves$model_name))

roc_curves %>%
  mutate(group = ifelse(substr(model_name, 1, 3) == 'PUL', model_name, 'Other contender models'),
         linetype_group = ifelse(substr(model_name, 1, 3) == 'PUL', 'champion', 'contender')) %>%
  ggplot(aes(fpr, tpr, group = model_name, color = group, linetype = linetype_group)) +
  # stat_smooth(linewidth = 1.5, se = F, method = 'lm', formula = y ~ poly(x, 4)) +
  geom_line(linewidth = 0.8) +
  geom_abline(intercept = 0, slope = 1.0, linewidth = 1.1, linetype = "dotted", color = grey_425) +
  annotate("text", x = 0.85, y = 0.8, label = "random chance", angle = 39.5, color = grey_425, size = 5) +
  scale_color_manual(values = c('lightgrey', salmon, pink, purple_light, purple_dark, blue_light, blue_bright, teal_dark, teal_light, 
                                teal_lighter, green)) +
  labs(color = '') +
  theme_bar_narrow +
  theme(panel.background = element_rect(fill = "#EEF4F8"),
        panel.grid = element_line(color = "#EEF4F8")) +
  guides(color = guide_legend(ncol = 4),
         linetype = "none")

ggsave("all_ROCS.svg", height = 8, width = 8, units = "in")

# --------------------
# Feature importance
# --------------------

feat_imp <- read_csv("results/20231129_1030/20231128_feat_imp.csv")

feat_imp_tidy <- feat_imp %>%
  gather(exc_absence:F_grades, key = "feature", value = "importance")

ggplot(feat_imp_tidy, aes(fct_reorder(feature, importance), importance)) +
  geom_boxplot() +
  coord_flip()

feat_imp_tidy %>%
  group_by(feature) %>%
  summarize(mean_imp = mean(importance),
            sd_imp = sd(importance)) %>%
ggplot(aes(fct_reorder(feature, mean_imp), mean_imp)) +
  geom_chicklet(stat = "identity", width = 0.5, fill = teal_light, color = "transparent") +
  geom_linerange(aes(x = feature, y = mean_imp, ymin=mean_imp-sd_imp, ymax = mean_imp+sd_imp), color = teal_dark, size = 1.3) +
  coord_flip() +
  theme_bar_narrow +
  labs(x = "",
       y = "permutation importance")

ggsave("feature_importance.svg", height = 6, width = 12, units = "in")

# -------------------
# Counterfactuals
# -------------------


data.frame(student = c("Justin", "Justin", "Justin"),
           change = c("+3 suspensions", "Set excused absences = 0", "+30% above-avg grades"),
           
           pc_change_in_depr_outcome = c(.3, -1.6, -0.7)) %>%
  ggplot() +
  geom_chicklet(aes(change, pc_change_in_depr_outcome), width = 0.3, fill = teal_light) +
  geom_hline(yintercept = 0, color = teal_light, linetype = "dashed") +
  coord_flip() +
  theme_bar_narrow +
  labs(y = '% change in depression outcome',
       x = '') +
  theme_bar_narrow

ggsave("justin_counterfactual.svg", height = 3, width = 5, units = "in")

# --------------------
# Causal DAG
# --------------------

library(visNetwork)
library(igraph)

dag <- read_csv("dag_cpdag_estimator_results.csv")

nodes <- dag %>%
  gather(from, to, key = 'dir', value = 'label') %>%
  distinct(label) %>%
  mutate(label = ifelse(label == 'depression_dx_in_next_6m', 'depression_dx_in_next_12m', label)) %>%
  mutate(id = row_number())

edges <- dag %>%
  filter(type == "Edge Coef.") %>%
  mutate(from = ifelse(from == 'depression_dx_in_next_6m', 'depression_dx_in_next_12m', from),
         to = ifelse(to == 'depression_dx_in_next_6m', 'depression_dx_in_next_12m', to)) %>%
  left_join(nodes, by = c('from' = 'label')) %>%
  left_join(nodes, by = c('to' = 'label')) %>%
  rename(from_nm = from,
         to_nm = to,
         from = id.x,
         to = id.y) %>%
  select(from_nm, to_nm, from, to)

# Entire graph
visNetwork(nodes, edges, width = '100%', height = '1000px', idToLabel = F) %>%
  visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T),
             selectedBy = 'label') %>% # Markov blanket
  visNodes(color = "#069D9A",
           size = 13, borderWidth = 0, physics = T,
           font = list(size = 35)) %>% #fixed = T
  visEdges(color = "#069D9A", #"#84dbda",
           smooth = F,
           arrows = list(to = list(
             scaleFactor = 0.1,
             enabled = T))
           ) %>%
  visIgraphLayout(layout= "layout.sphere", physics = F, smooth = F) %>%
  visEvents(type = 'once', startStabilizing = 'function() {
  this.moveTo({scale:0.3})}')

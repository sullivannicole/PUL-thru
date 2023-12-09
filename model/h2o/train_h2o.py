# ----------------------
# Libraries & settings
# ----------------------

import h2o
import pandas as pd
from h2o.automl import H2OAutoML
import sklearn.metrics
from sklearn.preprocessing import KBinsDiscretizer
from sklearn.compose import ColumnTransformer
from sklearn.pipeline import Pipeline

h2o.init()

# ---------------
# Import data
# ---------------

train_pd = pd.read_csv(<<path>>)
val_pd = pd.read_csv(<<path>>)
test_pd = pd.read_csv(<<path>>)

# -------
# OHE
# -------

# ohe_cols = [col for col in train_pd.columns if col.endswith(('6m', '6mo')) or col in ['age_at_pred', 'yrs_enrolled_in_hopkins_ps']]
# est = KBinsDiscretizer(n_bins = 4, encode = 'ordinal', strategy = 'kmeans')

# col_discretizer = ColumnTransformer(transformers = [('kbins', est, ohe_cols)], remainder = 'passthrough', verbose_feature_names_out = False)
# train_ohe = col_discretizer.fit_transform(train_pd)
# col_names = col_discretizer.get_feature_names_out()

# train = pd.DataFrame(train_ohe)
# train.columns = col_names

# test_ohe = col_discretizer.transform(test_pd)
# test = pd.DataFrame(test_ohe)
# test.columns = col_names

train = train_pd
val = val_pd
test = test_pd

# ----------------------
# Data pre-processing
# ----------------------

x = train.columns
y = 'depression_dx_in_next_6m'
exclude_cols = ['student_id', 'pred_dt', y]
x = [i for i in x if i not in exclude_cols]

# Convert to H2o
train_frame = h2o.H2OFrame(train)
val_frame = h2o.H2OFrame(val)
test_frame = h2o.H2OFrame(test)

train_frame[y] = train_frame[y].asfactor()
val_frame[y] = val_frame[y].asfactor()
test_frame[y] = test_frame[y].asfactor() 

# ----------------------
# Modeling
# ----------------------

# Run automl experiment
aml10 = H2OAutoML(max_runtime_secs = 60*20, seed = 5454, preprocessing = None, exclude_algos = None, balance_classes = True, stopping_metric = 'AUC', nfolds = 0)
aml10.train(x = x, y = y, training_frame = train_frame, validation_frame = val_frame)

# Get leaderboard
lb_all = h2o.automl.get_leaderboard(aml10, extra_columns = 'ALL')
lb_all_pd = lb_all.as_data_frame()

# Extract all mods, get experiment id
all_mods = list(map(lambda x: h2o.get_model(x), lb_all_pd['model_id'].values))
exp_id = '_'.join(aml10.project_name.split('_')[-2:])

# Save leaderboard
lb_all_pd.to_csv(f'<<path>>\\{exp_id}_leaderboard.csv', index = False)

# Save out models
for i in all_mods:
    model_path = h2o.save_model(model = i, path = f'<<path>>\\model\\experiments\\{exp_id}\\{i.model_id}', force = True)

# saved_model1 = h2o.load_model(model_path)

# ----------
# Inference
# ----------

model_ids = []
val_aurocs = []
val_f1s = []
val_recalls = []
val_precisions = []
val_loglosses = []
val_accuracies = []
val_tns = []
val_fps = []
val_fns = []
val_tps = []
val_auprcs = []
test_aurocs = []
test_f1s = []
test_recalls = []
test_precisions = []
test_loglosses = []
test_accuracies = []
test_tns = []
test_fps = []
test_fns = []
test_tps = []
test_auprcs = []

for i in all_mods:

    val_preds = i.predict(val_frame)
    val_preds_df = val_preds.as_data_frame()
    val_preds_df[['student_id', 'pred_dt', 'depression_dx_in_next_6m']] = val[['student_id', 'pred_dt', 'depression_dx_in_next_6m']]
    
    test_preds = i.predict(test_frame)
    test_preds_df = test_preds.as_data_frame()
    test_preds_df[['student_id', 'pred_dt', 'depression_dx_in_next_6m']] = test[['student_id', 'pred_dt', 'depression_dx_in_next_6m']]
    
    model_ids.append(i.model_id)
    
    # ---------
    # Evaluate
    # ---------
    
    # Val metrics
    val_auroc = sklearn.metrics.roc_auc_score(y_true = val_preds_df['depression_dx_in_next_6m'], y_score = val_preds_df['p1'])
    val_f1 = sklearn.metrics.f1_score(y_true = val_preds_df['depression_dx_in_next_6m'], y_pred = val_preds_df['predict'])
    val_recall = sklearn.metrics.recall_score(y_true = val_preds_df['depression_dx_in_next_6m'], y_pred = val_preds_df['predict'])
    val_precision = sklearn.metrics.precision_score(y_true = val_preds_df['depression_dx_in_next_6m'], y_pred = val_preds_df['predict'])
    val_logloss = sklearn.metrics.log_loss(y_true = val_preds_df['depression_dx_in_next_6m'], y_pred = val_preds_df['predict'])
    val_accuracy = sklearn.metrics.accuracy_score(y_true = val_preds_df['depression_dx_in_next_6m'], y_pred = val_preds_df['predict'])
    
    val_tn, val_fp, val_fn, val_tp = sklearn.metrics.confusion_matrix(y_true = val_preds_df['depression_dx_in_next_6m'], y_pred = val_preds_df['predict']).ravel()
    val_precision_, val_recall_, val_pr_thresholds = sklearn.metrics.precision_recall_curve(y_true = val_preds_df['depression_dx_in_next_6m'], probas_pred = val_preds_df['p1'])
    val_auprc = sklearn.metrics.auc(val_recall_, val_precision_)
    val_fpr, val_tpr, val_roc_thresholds = sklearn.metrics.roc_curve(y_true = val_preds_df['depression_dx_in_next_6m'], y_score = val_preds_df['p1'])
    
    
    list(map(lambda x, y: x.append(y), [val_aurocs, val_f1s, val_recalls, val_precisions, val_loglosses, val_accuracies, val_tns, val_fps, val_fns, val_tps, val_auprcs],
          [val_auroc, val_f1, val_recall, val_precision, val_logloss, val_accuracy, val_tn, val_fp, val_fn, val_tp, val_auprc]))
    
    
    # Test metrics
    test_auroc = sklearn.metrics.roc_auc_score(y_true = test_preds_df['depression_dx_in_next_6m'], y_score = test_preds_df['p1'])
    test_f1 = sklearn.metrics.f1_score(y_true = test_preds_df['depression_dx_in_next_6m'], y_pred = test_preds_df['predict'])
    test_recall = sklearn.metrics.recall_score(y_true = test_preds_df['depression_dx_in_next_6m'], y_pred = test_preds_df['predict'])
    test_precision = sklearn.metrics.precision_score(y_true = test_preds_df['depression_dx_in_next_6m'], y_pred = test_preds_df['predict'])
    test_logloss = sklearn.metrics.log_loss(y_true = test_preds_df['depression_dx_in_next_6m'], y_pred = test_preds_df['predict'])
    test_accuracy = sklearn.metrics.accuracy_score(y_true = test_preds_df['depression_dx_in_next_6m'], y_pred = test_preds_df['predict'])
    
    test_tn, test_fp, test_fn, test_tp = sklearn.metrics.confusion_matrix(y_true = test_preds_df['depression_dx_in_next_6m'], y_pred = test_preds_df['predict']).ravel()
    test_precision_, test_recall_, test_pr_thresholds = sklearn.metrics.precision_recall_curve(y_true = test_preds_df['depression_dx_in_next_6m'], probas_pred = test_preds_df['p1'])
    test_auprc = sklearn.metrics.auc(test_recall_, test_precision_)
    test_fpr, test_tpr, test_roc_thresholds = sklearn.metrics.roc_curve(y_true = test_preds_df['depression_dx_in_next_6m'], y_score = test_preds_df['p1'])
    
    list(map(lambda x, y: x.append(y), [test_aurocs, test_f1s, test_recalls, test_precisions, test_loglosses, test_accuracies, test_tns, test_fps, test_fns, test_tps, test_auprcs],
         [test_auroc, test_f1, test_recall, test_precision, test_logloss, test_accuracy, test_tn, test_fp, test_fn, test_tp, test_auprc]))


eval_df = pd.DataFrame({'model_id': model_ids,
                        'val_auroc': val_aurocs,
                        'val_f1': val_f1s,
                        'val_recall': val_recalls,
                        'val_precision': val_precisions,
                        'val_logloss': val_loglosses,
                        'val_accuracy': val_accuracies,
                        'val_tn': val_tns,
                        'val_fp': val_fps,
                        'val_fn': val_fns,
                        'val_tp': val_tps,
                        'val_auprc': val_auprcs,
                        'test_auroc': test_aurocs,
                        'test_f1': test_f1s,
                        'test_recall': test_recalls,
                        'test_precision': test_precisions,
                        'test_logloss': test_loglosses,
                        'test_accuracy': test_accuracies,
                        'test_tn': test_tns,
                        'test_fp': test_fps,
                        'test_fn': test_fns,
                        'test_tp': test_tps,
                        'test_auprc': test_auprcs})

eval_df.to_csv(f'<<path>>\\model\\experiments\\{exp_id}_eval.csv', index = False)
# h2o.shutdown()

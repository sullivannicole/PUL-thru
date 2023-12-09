# ----------------------
# Libraries & settings
# ----------------------
import h2o
import pandas as pd
import sklearn.metrics
from sklearn.calibration import calibration_curve
import matplotlib.pyplot as plt
import os
import json

h2o.init()

# ----------------
# Load models
# ----------------
# dnn = h2o.load_model(<<path>>)
# stack = h2o.load_model(<<path>>)
# gbm = h2o.load_model(<<path>>)
# gbm40 = h2o.load_model(<<path>>)

# ----------------
# Load data
# ----------------
test3 = pd.read_csv(<<path>>)
test_frame3 = h2o.H2OFrame(test3)

test5 = pd.read_csv(<<path>>)
test_frame5 = h2o.H2OFrame(test5)

# ---------------
# Model metrics
# ---------------

# Feature importance
# Stacked ensemble doesn't have feat importance
for i in [gbm22, gbm41, dnn2, dnn3, gbm46, gbm54]:
    feat_imp = i.varimp(use_pandas = True)
    feat_imp.to_csv(<<path>>, index = False)

def get_roc_curve(model, h2o_frame, pd_df):
    
    preds = model.predict(h2o_frame)
    preds_pd = preds.as_data_frame()
    preds_pd[['student_id', 'pred_dt', 'depression_dx_in_next_6m']] = pd_df[['student_id', 'pred_dt', 'depression_dx_in_next_6m']]
    roc_fpr, roc_tpr, roc_thresholds = sklearn.metrics.roc_curve(y_true = preds_pd['depression_dx_in_next_6m'], y_score = preds_pd['p1'])
    auroc = sklearn.metrics.roc_auc_score(y_true = preds_pd['depression_dx_in_next_6m'], y_score = preds_pd['p1'])
    
    mod_type = model.model_id.split('_')[0]
    
    return pd.DataFrame({'model_id': model.model_id, 'model_type': mod_type, 'fpr': roc_fpr, 'tpr': roc_tpr, 'tot_auroc': auroc})

# ROC curves
roc_curves = list(map(lambda x, y, z: get_roc_curve(x, y, z), [gbm22, gbm41, dnn2, dnn3, gbm46, gbm54, stack], 
                      [test_frame3, test_frame3, test_frame5, test_frame5, test_frame5, test_frame5, test_frame5], 
                      [test3, test3, test5, test5, test5, test5, test5]))
roc_df = pd.concat(roc_curves)
roc_df.to_csv(<<path>>, index = False)

# ------------------
# ANN hyperparams
# ------------------

dnn2.params
dnn3.params

dnn2.model_performance()
dnn3.model_performance()
gbm22.model_performance()
gbm41.model_performance()
gbm46.model_performance()
gbm54.model_performance()
stack.model_performance()

# ------------------------------------------------------
# Extract params & calibrate champion model - GBM?
# ------------------------------------------------------

# Load data
train = pd.read_csv(<<path>>)
train_frame = h2o.H2OFrame(train)

# Conver to H2o
cal = pd.read_csv(<<path>>)
cal_frame = h2o.H2OFrame(cal)

x = train.columns
y = 'depression_dx_in_next_6m'
exclude_cols = ['student_id', 'pred_dt', y]
x = [i for i in x if i not in exclude_cols]

train_frame[y] = train_frame[y].asfactor()
cal_frame[y] = cal_frame[y].asfactor()


# Export to json - one-time
# params = json.dumps(gbm46.params)
# f = open(f'model\\champion_models\\{gbm46.model_id}_params.json', 'w')
# f.write(params)
# f.close()

with open(r'model\\champion_models\\GBM_grid_1_AutoML_4_20231025_104540_model_46_params.json') as param_file:
    model_hyperparams = json.load(param_file)

from h2o.estimators import H2OGradientBoostingEstimator

gbm_mod = H2OGradientBoostingEstimator(nfolds = model_hyperparams['nfolds']['actual'],
                                       seed = 23,
                                       
                                       # Algo specific
                                       huber_alpha = model_hyperparams['huber_alpha']['actual'],
                                       learn_rate_annealing = model_hyperparams['learn_rate_annealing']['actual'],
                                       pred_noise_bandwidth = model_hyperparams['pred_noise_bandwidth']['actual'],
                                       quantile_alpha = model_hyperparams['quantile_alpha']['actual'],
                                       
                                       # Tree-based
                                       calibrate_model = True,
                                       calibration_frame = cal_frame,
                                       col_sample_rate = model_hyperparams['col_sample_rate']['actual'],
                                       col_sample_rate_per_tree = model_hyperparams['col_sample_rate_per_tree']['actual'],
                                       histogram_type = model_hyperparams['histogram_type']['actual'],
                                       learn_rate = model_hyperparams['learn_rate']['actual'],
                                       max_abs_leafnode_pred = model_hyperparams['max_abs_leafnode_pred']['actual'],
                                       max_depth = model_hyperparams['max_depth']['actual'],
                                       min_rows = model_hyperparams['min_rows']['actual'],
                                       min_split_improvement = model_hyperparams['min_split_improvement']['actual'],
                                       nbins = model_hyperparams['nbins']['actual'],
                                       nbins_cats = model_hyperparams['nbins_cats']['actual'],
                                       nbins_top_level = model_hyperparams['nbins_top_level']['actual'],
                                       ntrees = model_hyperparams['ntrees']['actual'],
                                       sample_rate = model_hyperparams['sample_rate']['actual'],
                                       sample_rate_per_class = model_hyperparams['sample_rate_per_class']['actual'],
                                       score_tree_interval = model_hyperparams['score_tree_interval']['actual'],
                                       
                                       # Common
                                       auc_type = model_hyperparams['auc_type']['actual'],
                                       balance_classes = model_hyperparams['balance_classes']['actual'],
                                       categorical_encoding = model_hyperparams['categorical_encoding']['actual'],
                                       distribution = model_hyperparams['distribution']['actual'],
                                       gainslift_bins = model_hyperparams['gainslift_bins']['actual'])

gbm_mod.train(x = x, y = y, training_frame = train_frame)

model_path = h2o.save_model(model = gbm_mod, path = 'model\\champion_models\\calibrated\\GBM_grid_1_AutoML_4_20231025_104540_model_46_calibrated', force = True)

gbm46_preds = gbm_mod.predict(test_frame5)
gbm46_preds = gbm46_preds.as_data_frame()
gbm46_preds[['student_id', 'pred_dt', 'depression_dx_in_next_6m']] = test5[['student_id', 'pred_dt', 'depression_dx_in_next_6m']]

gbm46_preds.to_csv('model\\champion_models\\GBM_grid_1_AutoML_4_20231025_104540_model_46_calibrated_preds.csv', index = False)

# ----------------------------------
# Test metrics **excluding 2020**
# ----------------------------------

model_ids = []
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

for i in [dnn, stack, gbm]:
    
    test_preds = i.predict(test_frame)
    test_preds_df = test_preds.as_data_frame()
    test_preds_df[['student_id', 'pred_dt', 'depression_dx_in_next_6m']] = test[['student_id', 'pred_dt', 'depression_dx_in_next_6m']]
    
    model_ids.append(i.model_id)
    
    # ---------
    # Evaluate
    # ---------
    
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



# ----------other code ----------------

# Predictions on validation, test sets
pred_dfs = list(map(lambda x: get_roc_curve(x, val_test_frame, val_test), [gbm, gbm2, stack, xrt, drf]))
pred_df = pd.concat(pred_dfs)
pred_df.to_csv('model\\visualizations\\roc_curves.csv')

# Parameters
gbm.get_params()

def plot_roc(model, h2o_frame, pd_df):
    preds = model.predict(h2o_frame)
    preds_pd = preds.as_data_frame()
    preds_pd[['student_id', 'pred_dt', 'depression_dx_in_next_6m']] = pd_df[['student_id', 'pred_dt', 'depression_dx_in_next_6m']]
    roc_fpr, roc_tpr, roc_thresholds = sklearn.metrics.roc_curve(y_true = preds_pd['depression_dx_in_next_6m'], y_score = preds_pd['p1'])
    
    fig = plt.figure()
    plt.plot(roc_fpr, roc_tpr)
    return fig

plot_roc(dnn, test_frame, test)
# plot_roc(stack, val_test_frame, val_test)
# plot_roc(gbm2, val_test_frame, val_test)
# plot_roc(xrt, val_test_frame, val_test)
# plot_roc(drf, val_test_frame, val_test)


# Predictions on test sets
# plot_roc(gbm, test_frame, test)
# plot_roc(stack, test_frame, test)
# plot_roc(gbm2, test_frame, test)
# plot_roc(xrt, test_frame, test)
# plot_roc(drf, test_frame, test)

# ------------------------------------
# Load all models in an experiment
# ------------------------------------


exp_id = '20230815_185925'
mod_files = os.listdir(f'model\\experiments\\{exp_id}')
mods = list(map(lambda x: h2o.load_model(f'model\\experiments\\{exp_id}\\{x}\\{x}'), mod_files))

pred_dfs = list(map(lambda x: get_roc_curve(x, val_test_frame, val_test), mods))
pred_df = pd.concat(pred_dfs)
pred_df.to_csv(f'model\\experiments\\{exp_id}_roc_val_test.csv')

# ---------------------
# Imports and data
# ---------------------

from pulearn import ElkanotoPuClassifier
from sklearn.svm import SVC
import pandas as pd
from sklearn.metrics import precision_recall_fscore_support
import sklearn.metrics
from sklearn.preprocessing import MinMaxScaler
import numpy as np
from fairlearn.metrics import demographic_parity_difference, demographic_parity_ratio, equalized_odds_difference, equalized_odds_ratio, make_derived_metric, MetricFrame
import random
import dowhy.gcm as gcm
import networkx as nx

main = pd.read_csv('<<path>>.csv')

main['other_race_ethn'] = np.select(
    [
    (main['ai_alaskan_native'] == 1),
    (main['native_hawaiian_pi'] == 1),
    (main['multiracial'] == 1)
    ],
    [1, 1, 1],
    default = 0
    )

# -------------------------
# Custom functions
# -------------------------

def binarize_preds(y_test, y_pred_proba):
    
    y_pred_scaled = MinMaxScaler().fit_transform(y_pred_proba[:, 1].reshape(-1, 1)) # scale predictions from 0 to 1 (SVM is boundary-based method, so can make preds outside this)
    precisions, recalls, prc_thresholds = sklearn.metrics.precision_recall_curve(y_test, y_pred_scaled)
    f1_scores = 2*precisions*recalls/(recalls+precisions)
    f1_threshold = prc_thresholds[np.argmax(f1_scores)] # Get threshold that maximizes F1
    y_pred = np.where(y_pred_scaled > f1_threshold, 1, 0)
    
    return y_pred

def get_f1_from_proba(y_true = np.array, y_pred_proba = np.array):
    """

    Parameters
    ----------
    y_true : np.array
        Actual binary (0, 1) labels for data.
    y_proba : np.array
        Predicted probabilities (continuous from 0 to 1) of labels.

    Returns
    -------
    max_f1 : float
        Maximum F1 possible from the precision-recall curve for the given predictions.

    """
    
    y_pred_scaled = MinMaxScaler().fit_transform(y_pred_proba[:, 1].reshape(-1, 1)) # scale predictions from 0 to 1 (SVM is boundary-based method)
    
    # Evaluate
    precisions, recalls, prc_thresholds = sklearn.metrics.precision_recall_curve(y_true, y_pred_scaled)
    f1_scores = (2*precisions*recalls)/(recalls+precisions)
    max_f1 = np.nanmax(f1_scores)
    
    return max_f1

# from sklearn.inspection import permutation_importance
# tweaked this ^^ sklearn function slightly so that we can use a custom threshold for conversion to binary prediction
# From inria.github.io/scikit-learn-mooc/python_scripts/dev_features_importance.html
 
def get_score_after_permutation(model, X, y, curr_feat):
    
    X_permuted = X.copy()
    col_idx = list(X.columns).index(curr_feat)
    # permute one column
    X_permuted.iloc[:, col_idx] = np.random.permutation(X_permuted[curr_feat].values)
    
    y_pred_proba = model.predict_proba(X_permuted)
    max_f1 = get_f1_from_proba(y, y_pred_proba)
    
    return max_f1

def get_feature_importance(model, X, y, curr_feat):
    
    y_pred_proba = model.predict_proba(X)
    baseline_f1_train = get_f1_from_proba(y, y_pred_proba)
    permuted_f1_train = get_score_after_permutation(model, X, y, curr_feat)
    feature_importance = baseline_f1_train - permuted_f1_train
    
    return feature_importance


def permutation_importance_tweaked(model, X, y, n_repeats = 10):
    
    importances = []
    for curr_feat in X.columns:
        list_feature_importance = []
        for n_round in range(n_repeats):
            list_feature_importance.append(get_feature_importance(model, X, y, curr_feat))
            
        importances.append(list_feature_importance)
        
    return {
        "importances_mean": np.mean(importances, axis = 1),
        "importances_std": np.std(importances, axis = 1),
        "importances": importances}


# ----------------------------
# Temporal cv (back-testing)
# ----------------------------

test_dts = ['2015-01-01', '2015-07-01', '2016-01-01', '2016-07-01',
            '2017-01-01', '2017-07-01', '2018-01-01', '2018-07-01', 
            '2019-01-01', '2019-07-01']

feature_names = ['exc_absence', 'minor_infraction', 'psychiatric_disorder_non_depressive',
                      'white', 'aa_black', 'genderf', 'lang_english', 'age_at_pred_natural',
                      'FR_status', 'yrs_enrolled_in_hopkins_ps', 'A_grades', 'F_grades']

# feature_names = ['exc_absence', 'minor_infraction', 'oss', 'police', 'psychiatric_disorder_non_depressive',
#                      'white', 'aa_black', 'asian', 'hispanic', 'other_race_ethn', 'genderf', 'lang_english', 'lang_spanish', 'age_at_pred_natural',
#                      'FR_status', 'yrs_enrolled_in_hopkins_ps', 'A_grades', 'C_grades', 'F_grades']


test_dt_arr = []
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
test_roc_curves = []
test_pr_curves = []
test_feat_imp = []
test_feat_imp_folds = []
test_preds = []

random.seed(626)

for i in test_dts:
    
    train = main.query(f'pred_dt <= "{i}"').copy(deep = True)
    test = main.query(f'pred_dt == "{i}"').copy(deep = True)
    
    X_train_df = train[feature_names]
    X_train = X_train_df.to_numpy()
    y_train = train['depression_dx_in_next_6m'].to_numpy()
    
    X_test = test[feature_names].to_numpy()
    y_test = test['depression_dx_in_next_6m'].to_numpy()
    
    # Fit classic 2-step PU learner with an SVM classifier
    svc = SVC(C = 10, kernel = 'rbf', gamma = 0.4, probability = True)
    pu_estimator = ElkanotoPuClassifier(estimator = svc, hold_out_ratio = 0.1)
    pu_estimator.fit(X_train, y_train)
    y_pred_proba = pu_estimator.predict_proba(X_test)
    y_pred_scaled = MinMaxScaler().fit_transform(y_pred_proba[:, 1].reshape(-1, 1)) # scale predictions from 0 to 1 (SVM is boundary-based method)
    
    # Evaluate 
    test_fpr, test_tpr, test_roc_thresholds = sklearn.metrics.roc_curve(y_true = y_test, y_score = y_pred_scaled)
    test_prec, test_rec, test_prc_thresholds = sklearn.metrics.precision_recall_curve(y_test, y_pred_scaled)
    test_f1_scores = 2*test_prec*test_rec/(test_rec+test_prec)
    f1_threshold = test_prc_thresholds[np.argmax(test_f1_scores)]
    max_f1 = np.max(test_f1_scores)
    # y_pred = pu_estimator.predict(X_test, threshold = f1_threshold)
    y_pred = np.where(y_pred_scaled > f1_threshold, 1, 0)
     
    test_auroc = sklearn.metrics.roc_auc_score(y_true = y_test, y_score = y_pred_scaled)
    test_f1 = sklearn.metrics.f1_score(y_true = y_test, y_pred = y_pred)
    test_recall = sklearn.metrics.recall_score(y_true = y_test, y_pred = y_pred)
    test_precision = sklearn.metrics.precision_score(y_true = y_test, y_pred = y_pred)
    test_logloss = sklearn.metrics.log_loss(y_true = y_test, y_pred = y_pred)
    test_accuracy = sklearn.metrics.accuracy_score(y_true = y_test, y_pred = y_pred)
    test_tn, test_fp, test_fn, test_tp = sklearn.metrics.confusion_matrix(y_true = y_test, y_pred = y_pred).ravel()
    test_auprc = sklearn.metrics.auc(test_rec, test_prec)
    
    test_roc_curves.append(pd.DataFrame({'test_dt': i, 'fpr': test_fpr, 'tpr': test_tpr}))
    test_pr_curves.append(pd.DataFrame({'test_dt': i, 'recall': test_rec, 'precision': test_prec}))
    
    test['pred_dt'] = i
    test['pu_predict'] = y_pred
    test_preds.append(test)
    
    # Feature importance
    # Computationally expensive, so probably don't want to run for every back-test
    # perm_importance = permutation_importance_tweaked(pu_estimator, X_train_df, y_train)
    # test_feat_imp.append(pd.DataFrame({'test_dt': i,
    #                                    'feature': feature_names,
    #                                    'importance_mean': perm_importance.importances_mean,
    #                                    'importance_sd': perm_importance.importances_std}))
    
    # feat_imp_df = pd.DataFrame(perm_importance.importances.T, columns = feature_names)
    # feat_imp_df['test_dt'] = i
    # test_feat_imp_folds.append(feat_imp_df)
    
    list(map(lambda x, y: x.append(y), [test_dt_arr, test_aurocs, test_f1s, test_recalls, test_precisions, test_loglosses, test_accuracies, test_tns, test_fps, test_fns, test_tps, test_auprcs],
         [i, test_auroc, test_f1, test_recall, test_precision, test_logloss, test_accuracy, test_tn, test_fp, test_fn, test_tp, test_auprc]))
    
eval_df = pd.DataFrame({'backtest_dt': test_dt_arr,
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

eval_df.to_csv(f'<<path>>.csv', index = False)

roc_df = pd.concat(test_roc_curves)
roc_df.to_csv(f'<<path>>.csv', index = False)

prc_df = pd.concat(test_pr_curves)
prc_df.to_csv(f'<<path>>>', index = False)

# ----------------------
# Feature importance
# ----------------------

pred_dt = '2019-07-01'
train = main.query(f'pred_dt <= "{pred_dt}"').copy(deep = True)

X_train_df = train[feature_names]
X_train = X_train_df.to_numpy()
y_train = train['depression_dx_in_next_6m'].to_numpy()

svc = SVC(C = 10, kernel = 'rbf', gamma = 0.4, probability = True)
pu_estimator = ElkanotoPuClassifier(estimator = svc, hold_out_ratio = 0.1)
pu_estimator.fit(X_train, y_train)

feature_importance = permutation_importance_tweaked(pu_estimator, X_train_df, y_train)
np.array(feature_importance['importances']).T

feat_imp_df = pd.DataFrame(np.array(feature_importance['importances']).T, columns = feature_names)
feat_imp_df['test_dt'] = pred_dt
feat_imp_df.to_csv(f'<<path>>', index = False)

# -----------------
# Fairness
# -----------------

test = main.query(f'pred_dt == "{pred_dt}"').copy(deep = True)
X_test_df = test[feature_names]
X_test = X_test_df.to_numpy()
y_test = test['depression_dx_in_next_6m'].to_numpy()

y_pred_proba = pu_estimator.predict_proba(X_test)
y_pred = binarize_preds(y_test, y_pred_proba)

# Demographic parity difference
sensitive_feats = ['genderf', 'genderm', 'white', 'aa_black', 'hispanic', 'ai_alaskan_native', 'asian', 'native_hawaiian_pi',
                   'multiracial', 'lang_spanish', 'lang_somali', 'age_at_pred_natural']

f1_min = make_derived_metric(metric = sklearn.metrics.f1_score, transform = 'group_min')
recall_max = make_derived_metric(metric = sklearn.metrics.recall_score, transform = 'group_max')
recall_diff = make_derived_metric(metric = sklearn.metrics.recall_score, transform = 'difference')
recall_ratio = make_derived_metric(metric = sklearn.metrics.recall_score, transform = 'ratio')
precision_min = make_derived_metric(metric = sklearn.metrics.precision_score, transform = 'group_min')

fairness = list(map(lambda x: {'feat': x,
                               'demographic_parity_diff': demographic_parity_difference(y_test, y_pred, sensitive_features = test[x]),
                               'ee_odds_diff': equalized_odds_difference(y_test, y_pred, sensitive_features = test[x]),
                               'ee_odds_ratio': equalized_odds_ratio(y_test, y_pred, sensitive_features = test[x]),
                               'f1_min': f1_min(y_test, y_pred, sensitive_features = test[x]),
                               'recall_max': recall_max(y_test, y_pred, sensitive_features = test[x]),
                               'recall_diff': recall_diff(y_test, y_pred, sensitive_features = test[x]),
                               'recall_ratio': recall_ratio(y_test, y_pred, sensitive_features = test[x])
                               
                               }, sensitive_feats))

fairness_df = pd.DataFrame.from_dict(fairness)
fairness_df.to_csv(f'<<path>>', index = False)

# Fairness, back-tests combined
backtests = pd.concat(test_preds)

# Re-combine ohe features
backtests['gender'] = np.select(
    [
    (backtests['genderf'] == 1)
    ],
    ['Female'],
    default = 'Male'
    )

backtests['race_ethnicity'] = np.select(
    [
    (backtests['aa_black'] == 1),
    (backtests['hispanic'] == 1),
    (backtests['asian'] == 1),
    (backtests['white'] == 1)
    ],
    ['African Am/Black',
     'Hispanic',
     'Asian',
     'white'],
    default = 'other group'
    )

backtests['age_chr'] = np.select(
    [
    (backtests['age_at_pred_natural'] == 11),
    (backtests['age_at_pred_natural'] == 12),
    (backtests['age_at_pred_natural'] == 13),
    (backtests['age_at_pred_natural'] == 14),
    (backtests['age_at_pred_natural'] == 15),
    (backtests['age_at_pred_natural'] == 16),
    (backtests['age_at_pred_natural'] == 17),
    (backtests['age_at_pred_natural'] == 18),
    (backtests['age_at_pred_natural'] == 19),
    (backtests['age_at_pred_natural'] == 20),
    ],
    ['11',
     '12',
     '13',
     '14',
     '15',
     '16',
     '17',
     '18',
     '19',
     '20', 
     ],
    default = 'other'
    )

backtests['lang'] = np.select(
    [
    (backtests['lang_english'] == 1),
    (backtests['lang_spanish'] == 1),
    (backtests['lang_somali'] == 1),
    ],
    ['English',
     'Spanish',
     'Somalian'
     ],
    default = 'other'
    )


metric_frames = list(map(lambda x: MetricFrame(
    metrics = {'f1': sklearn.metrics.f1_score,
               'recall': sklearn.metrics.recall_score,
               'precision': sklearn.metrics.precision_score},
    y_true = backtests['depression_dx_in_next_6m'],
    y_pred = backtests['pu_predict'],
    sensitive_features = backtests[x]
    ),
    
    ['gender', 'race_ethnicity', 'lang', 'age_chr']
    ))


list(map(lambda x: x.by_group, metric_frames))

# sensitive_feats = ['genderf', 'genderm', 'white', 'aa_black', 'hispanic', 'ai_alaskan_native', 'asian', 'native_hawaiian_pi',
#                    'multiracial', 'lang_spanish', 'lang_somali', 'age_at_pred_natural']

sensitive_feats = ['gender', 'race_ethnicity', 'lang', 'age_chr']

fairness = list(map(lambda x: {'feat': x,
                               'demographic_parity_diff': demographic_parity_difference(backtests['depression_dx_in_next_6m'], backtests['pu_predict'], sensitive_features = backtests[x]),
                               'eq_odds_diff': equalized_odds_difference(backtests['depression_dx_in_next_6m'], backtests['pu_predict'], sensitive_features = backtests[x]),
                               'eq_odds_ratio': equalized_odds_ratio(backtests['depression_dx_in_next_6m'], backtests['pu_predict'], sensitive_features = backtests[x]),
                               'f1_min': f1_min(backtests['depression_dx_in_next_6m'], backtests['pu_predict'], sensitive_features = backtests[x]),
                               'recall_max': recall_max(backtests['depression_dx_in_next_6m'], backtests['pu_predict'], sensitive_features = backtests[x]),
                               'recall_diff': recall_diff(backtests['depression_dx_in_next_6m'], backtests['pu_predict'], sensitive_features = backtests[x]),
                               'recall_ratio': recall_ratio(backtests['depression_dx_in_next_6m'], backtests['pu_predict'], sensitive_features = backtests[x])
                               
                               }, sensitive_feats))
fairness[0]
fairness_df = pd.DataFrame.from_dict(fairness)

# -----------------
# Counterfactual
# -----------------

tetrad = pd.read_csv('<<path>>')
tetrad_data = pd.read_csv("<<path>>")
tetrad_edges = tetrad.query('type == "Edge Coef."')

tetrad_graph = nx.from_pandas_edgelist(tetrad_edges, source = 'from', target = 'to', create_using = nx.DiGraph)
nx.draw(tetrad_graph, with_labels = True)
causal_model = gcm.InvertibleStructuralCausalModel(tetrad_graph)
gcm.auto.assign_causal_mechanisms(causal_model, tetrad_data)
gcm.fit(causal_model, tetrad_data)


# Fill in values for case studies with means
pos_all = tetrad_data.query('depression_dx_in_next_6m == 1')
pos_mean = pos_all.mean()

# White English-speaking 15 yo female, no F/R -> adjust A-grades
alice = pd.DataFrame([pos_mean.values], columns = pos_all.columns)
alice['white'] = 1
alice['hispanic'] = 0
alice['ai_alaskan_native'] = 0
alice['asian'] = 0
alice['aa_black'] = 0
alice['native_hawaiian_pi'] = 0
alice['multiracial'] = 0
alice['genderm'] = 0
alice['genderf'] = 1
alice['lang_english'] = 1
alice['lang_somali'] = 0
alice['lang_spanish'] = 0
alice['FR_status'] = 0
alice['age_at_pred'] = 15
alice['depression_dx_in_next_6m'] = 1

# Hispanic Spanish-speaking 15-yo female, F/R -> adjust A-grades
dyane = pd.DataFrame([pos_mean.values], columns = pos_all.columns)
dyane['white'] = 0
dyane['hispanic'] = 1
dyane['ai_alaskan_native'] = 0
dyane['asian'] = 0
dyane['aa_black'] = 0
dyane['native_hawaiian_pi'] = 0
dyane['multiracial'] = 0
dyane['genderf'] = 1
dyane['genderm'] = 0
dyane['lang_english'] = 0
dyane['lang_somali'] = 0
dyane['lang_spanish'] = 1
dyane['FR_status'] = 1
dyane['age_at_pred'] = 15
dyane['depression_dx_in_next_6m'] = 1

# Black English-speaking 14-yo male, no F/R -> adjust A-grades
justin = pd.DataFrame([pos_mean.values], columns = pos_all.columns)
justin['white'] = 0
justin['hispanic'] = 0
justin['ai_alaskan_native'] = 0
justin['asian'] = 0
justin['aa_black'] = 1
justin['native_hawaiian_pi'] = 0
justin['multiracial'] = 0
justin['genderf'] = 0
justin['genderm'] = 1
justin['lang_english'] = 1
justin['lang_somali'] = 0
justin['lang_spanish'] = 0
justin['FR_status'] = 0
justin['age_at_pred'] = 14
justin['depression_dx_in_next_6m'] = 1

alice_cf1 = gcm.counterfactual_samples(causal_model,
                                       {'grade_bucket1_6mo': lambda x: 0.8},
                                       observed_data = alice)

alice_cf1['depression_dx_in_next_6m']

alice_cf2 = gcm.counterfactual_samples(causal_model,
                                       {'exc_absence_6m': lambda x: 0},
                                       observed_data = alice)

alice_cf2['depression_dx_in_next_6m']

dyane_cf1 = gcm.counterfactual_samples(causal_model,
                                       {'grade_bucket1_6mo': lambda x: 0.8},
                                       observed_data = dyane)

dyane_cf1['depression_dx_in_next_6m']


dyane_cf2 = gcm.counterfactual_samples(causal_model,
                                       {'exc_absence_6m': lambda x: 0},
                                       observed_data = dyane)

dyane_cf2['depression_dx_in_next_6m']


justin_cf2 = gcm.counterfactual_samples(causal_model,
                                       {'exc_absence_6m': lambda x: 0},
                                       observed_data = dyane)

justin_cf2['depression_dx_in_next_6m']

justin_cf3 = gcm.counterfactual_samples(causal_model,
                                       {'suspension_6m': lambda x: 3},
                                       observed_data = dyane)

justin_cf3['depression_dx_in_next_6m']

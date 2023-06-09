import numpy as np
import pandas as pd
import dill
from pathlib import Path
from riskslim.paths import data_dir, results_dir, reporting_dir, reports_dir, repo_dir, templates_dir
from riskslim.data_io import load_processed_data
from scripts.glmnet import fit_glmnet_cv
from scripts.utils import get_results_name, get_baseline_results_name
from sklearn.metrics import roc_auc_score
from reporting.utils import make_report, open_file
pd.options.mode.chained_assignment = None
pd.set_option('display.max_columns', None)

input = {
    'data_name': 'cshock',
    'method_name': 'plr',
    'fold_id': 'K05N01',
    'feature_subset_file': None,
    #'feature_subset_file': results_dir / Path('cshock20_riskslim_C1_L6_K05N01_0').with_suffix('.results'),
    }
input['data_file'] = '%s/%s_processed.pickle' % (data_dir, input['data_name'])

alpha_values = np.linspace(0.0, 1.0, 6, endpoint = True)

# load data
data, cvindices = load_processed_data(file_name = input['data_file'])

# drop features if we have load classifier to use for feature selection  resutls
if input['feature_subset_file'] is not None:
    input['method_name'] = '%s_fixed' % input['method_name']
    with open(input['feature_subset_file'], 'rb') as infile:
        fs_results = dill.load(infile)

    keep_idx = np.flatnonzero(fs_results['coefs'])
    data['X'] = data['X'][:, keep_idx]
    data['X_test'] = data['X_test'][:, keep_idx]
    data['variable_names'] = [n for j, n in enumerate(data['variable_names']) if j in keep_idx]
    data['variable_types'] = {k:v for k,v in data['variable_types'].items() if k in data['variable_names']}

# setup folds
folds = cvindices[input['fold_id']]
fold_values = np.unique(folds)
fold_indices = {k: np.not_equal(k, folds) for k in fold_values}
get_fold_auc_train = lambda x:roc_auc_score(y_true = data['Y'][fold_indices[x[0]]], y_score = data['X'][fold_indices[x[0]],:].dot(x[1]))
get_fold_auc_validation = lambda x: roc_auc_score(y_true = data['Y'][~fold_indices[x[0]]], y_score = data['X'][~fold_indices[x[0]],:].dot(x[1]))

# train model with glmnet
df_cv = pd.concat([fit_glmnet_cv(data, cvindices, nlambda = 101, fold_id = input['fold_id'], intercept = False, alpha = a) for a in alpha_values]).reset_index(drop = True)
df_cv['model_size'] = df_cv['coefs'].apply(func = lambda x: np.sum(np.greater(np.abs(x[1:]), 1e-8)))

# compute K-cv error for each model
df_final = df_cv.query('fold == 0')
df_final['auc_train'] = df_final['coefs'].apply(func = lambda w: roc_auc_score(y_true = data['Y'], y_score = data['X'].dot(w)))
df_final['auc_test'] = df_final['coefs'].apply(func = lambda w: roc_auc_score(y_true = data['Y_test'], y_score = data['X_test'].dot(w)))

df_fold = df_cv.query('fold > 0')
df_fold['auc_train'] = df_fold[['fold', 'coefs']].apply(func = get_fold_auc_train, axis = 1)
df_fold['auc_validation'] = df_fold[['fold', 'coefs']].apply(func = get_fold_auc_validation, axis = 1)
df_fold = df_fold. \
    drop(columns = ['coefs', 'fold']). \
    groupby(['alpha', 'lambda_']). \
    mean(). \
    reset_index(). \
    rename(columns = {
    'auc_train': 'auc_cv_train_mean',
    'auc_validation': 'auc_cv_test_mean',
    })

df = pd.merge(df_final, df_fold, on = ['alpha', 'lambda_'])
df['model_size'] = df['coefs'].apply(func = lambda x: np.sum(np.greater(np.abs(x[1:]), 1e-8)))

#### report creation
input['platt_scaling'] = True
input['max_model_size'] = max(df['model_size'])

if input['feature_subset_file'] is not None:
    input['max_model_size'] = max(df['model_size'])

df_best = df[df['model_size'] > 0]
df_best = df_best[df_best['model_size'] <= input['max_model_size']]
df_best = df_best.iloc[df_best['auc_cv_test_mean'].argmax()]
best_idx = (df_best['alpha'] == df_cv['alpha']) & (df_best['lambda_'] == df_cv['lambda_'])
df_best_cv = df_cv[best_idx]

report_results_file = ''
for idx, row in df_best_cv.iterrows():

    results_header_name = get_baseline_results_name(data_name = input['data_name'],
                                                    method_name = input['method_name'],
                                                    fold_id = input['fold_id'],
                                                    max_L0_value = input['max_model_size'],
                                                    fold_num = row['fold'])

    if input['platt_scaling']:
        results_file = results_dir / ('%s_platt' % results_header_name)
    else:
        results_file = results_dir / ('%s' % results_header_name)

    results_file = results_file.with_suffix('.results')
    if row['fold'] == 0:
        report_results_file = results_file

    # to save
    results = {
        #
        'data_name': input['data_name'],
        'data_file': input['data_file'],
        'method_name': input['method_name'],
        'fold_id': input['fold_id'],
        'feature_subset_file': input['feature_subset_file'],
        #
        'max_L0_value': input['max_model_size'],
        'report_params': {'platt_scaling': input['platt_scaling']},
        'fold_num': row['fold'],
        'results_file': str(results_file),
        'coefs': np.array(row['coefs']),
        #
        #
        'max_coefficient': float('inf'),
        'objective_value': float('nan'),
        'optimality_gap': float('nan'),
        'upper_bound': float('nan'),
        'lower_bound': float('nan'),
        'c0_value': float('nan'),
        }

    with open(results_file, 'wb') as outfile:
        dill.dump(results, outfile, protocol = dill.HIGHEST_PROTOCOL, recurse = True)



# report name
report_pdf = make_report(template_file = templates_dir / 'model_report.Rmd',
                         report_data_file = report_results_file,
                         report_python_dir = repo_dir,
                         creation_script = reporting_dir / 'make_report.R',
                         output_file = reports_dir / ('%s_model_report.pdf' % report_results_file.stem),
                         build_dir = reports_dir / ('%s_model_report' % report_results_file.stem),
                         clean = True,
                         quiet = False,
                         remove_build = False,
                         remove_output = False)


open_file(report_pdf)
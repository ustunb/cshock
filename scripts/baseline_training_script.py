import numpy as np
import pandas as pd
import dill
from pathlib import Path
from riskslim.paths import data_dir, results_dir, reporting_dir, reports_dir, repo_dir
from riskslim.data_io import load_processed_data
from scripts.glmnet import fit_glmnet_cv
from scripts.utils import get_results_name
from sklearn.metrics import roc_auc_score
from reporting.utils import make_report, open_file
pd.options.mode.chained_assignment = None

# setup names
data_name = 'cshock20'
method_name = 'plr'
fold_id = 'K05N01'
alpha_values = np.linspace(0.0, 1.0, 6, endpoint = True)

# output file names
results_name = get_results_name(data_name, method_name, fold_id)
results_file = '%s/%s_all.results' % (results_dir, results_name)

# load data
data_file = '%s/%s_processed.pickle' % (data_dir, data_name)
data, cvindices = load_processed_data(data_file)

# setup folds
folds = cvindices[fold_id]
fold_values = np.unique(folds)
fold_indices = {k: np.not_equal(k, folds) for k in fold_values}
get_fold_auc_train = lambda x:roc_auc_score(y_true = data['Y'][fold_indices[x[0]]], y_score = data['X'][fold_indices[x[0]],:].dot(x[1]))
get_fold_auc_validation = lambda x: roc_auc_score(y_true = data['Y'][~fold_indices[x[0]]], y_score = data['X'][~fold_indices[x[0]],:].dot(x[1]))


# train model and process results
df_cv = pd.concat([fit_glmnet_cv(data, cvindices, nlambda = 100, fold_id = fold_id, alpha = a) for a in alpha_values]).reset_index(drop = True)
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
df['model_size'] = df['coefs'].apply(func = lambda x: np.count_nonzero(x[1:]))

# extract coefficients
coefs = df['coefs'].to_dict()
df = df.drop(columns = ['coefs'])

#
df_fold = df_cv.query('fold > 0').reset_index(drop = True)
coefs_fold = df_fold['coefs'].to_dict()
df_fold = df_fold.drop(columns = ['coefs'])


# save results
results = {
    #
    'method_name': method_name,
    'data_name': data_name,
    'data_file': data_file,
    'results_file': results_file,
    #
    'fold_id': 'K05N01',
    'fold_num': 0,
    'max_L0_value': float('nan'),
    'max_coefficient': float('inf'),
    #
    'objective_value': float('nan'),
    'optimality_gap': float('nan'),
    'upper_bound': float('nan'),
    'lower_bound': float('nan'),
    'c0_value': float('nan'),
    #
    'coefs': coefs,
    'df': df,
    #
    'coefs_fold': coefs_fold,
    'df_fold': df_fold,
    }

with open(results_file, 'wb') as outfile:
    dill.dump(results, outfile, protocol = dill.HIGHEST_PROTOCOL, recurse = True)


# setup file names
output_file = reports_dir / ('%s_model_report.pdf' % Path(results_file).stem)
build_dir = reports_dir / output_file.stem
report_pdf = make_report(template_file = reporting_dir / 'glmnet_report.Rmd',
                         report_data_file = results_file,
                         report_python_dir = repo_dir,
                         creation_script = reporting_dir / 'make_report.R',
                         output_file = output_file,
                         build_dir = build_dir,
                         clean = True,
                         quiet = True,
                         remove_build = False,
                         remove_output = False)

open_file(report_pdf)
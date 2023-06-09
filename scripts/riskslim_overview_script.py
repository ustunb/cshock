import numpy as np
import pandas as pd
import dill
from pathlib import Path
from riskslim.paths import data_dir, results_dir, reporting_dir, reports_dir, repo_dir, templates_dir
from riskslim.data_io import load_processed_data
from riskslim.utils import get_results_name
from reporting.utils import make_report, open_file
pd.options.mode.chained_assignment = None

input = {
    'data_name': 'cshock20',
    'method_name': 'riskslim',
    'platt_scaling': True,
    'max_coefficient': 1,
    'fold_id': 'K05N01',
    'model_size_min': 1,
    'model_size_max': 10,
    'hyperparameter_names': ["max_L0_value", "max_coefficient"],
    }

input['data_file'] = '%s/%s_processed.pickle' % (data_dir, input['data_name'])
if input['platt_scaling']:
    input['results_file'] = '%s/%s_%s_platt_%s_overview.results' % (results_dir, input['data_name'], input['method_name'], input['fold_id'])
else:
    input['results_file'] = '%s/%s_%s_%s_overview.results' % (results_dir, input['data_name'], input['method_name'], input['fold_id'])

data, cvindices = load_processed_data(file_name = input['data_file'])
n_folds = int(input['fold_id'][1:3])

# aggregate results
get_file_name = lambda model_size, fold_num: get_results_name(data_name = input['data_name'], method_name = input['method_name'], max_L0_value = model_size, max_coefficient = input['max_coefficient'], fold_id = input['fold_id'], fold_num = fold_num)
all_results = []
for model_size in np.arange(input['model_size_min'], input['model_size_max']+1):
    for fold in np.arange(0, n_folds+1):
        f = get_file_name(model_size, fold)
        f = results_dir / Path(f).with_suffix('.results')
        with open(f, 'rb') as infile:
            raw = dill.load(infile)
            coefs = np.round(raw['coefs'])
            all_results.append({
                'coefs': coefs,
                'fold': raw['fold_num'],
                'max_L0_value': raw['max_L0_value'],
                'max_coefficient': raw['max_coefficient']
                })

df_all = pd.DataFrame(all_results)

# full model
df_fold = df_all.query('fold > 0').reset_index(drop = True)
coefs_fold = df_fold['coefs'].to_dict()
df_fold = df_fold.drop(columns = ['coefs'])

# final model
df = df_all.query('fold == 0').reset_index(drop = True)
coefs = df['coefs'].to_dict()
df = df.drop(columns = ['coefs'])

# to save
results = dict(input)
results.update({
    'coefs': coefs,
    'df': df,
    'coefs_fold': coefs_fold,
    'df_fold': df_fold,
    })

with open(results['results_file'], 'wb') as outfile:
    dill.dump(results, outfile, protocol = dill.HIGHEST_PROTOCOL, recurse = True)


# create report
report_pdf = make_report(template_file = templates_dir / 'performance_overview.Rmd',
                         report_data_file = results['results_file'],
                         report_python_dir = repo_dir,
                         creation_script = reporting_dir / 'make_report.R',
                         output_file = reports_dir / ('%s_report.pdf' % Path(results['results_file']).stem),
                         build_dir = reports_dir /Path(results['results_file']).stem,
                         clean = True,
                         quiet = True,
                         remove_build = False,
                         remove_output = False)

open_file(report_pdf)
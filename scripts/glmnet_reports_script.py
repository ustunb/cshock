from reporting.utils import open_file, make_report, merge_pdfs
from riskslim.paths import repo_dir, results_dir, reports_dir, reporting_dir
from scripts.utils import get_results_name

data_name = 'cshock'
max_coefficient = 5

data_name = 'cshock'
method_name = 'riskslim'
max_L0_value = 5
max_coefficient = 5
max_runtime = 60
fold_id = 'K05N01'
fold_num = 0

# setup names
all_model_sizes = range(4, 9)
all_results_names = [get_results_name(data_name, method_name, max_coefficient, k, fold_id, fold_num) for k in all_model_sizes]

for model_size in all_model_sizes:

    results_name = get_results_name(data_name, method_name, max_coefficient, model_size, fold_id, fold_num)

    # setup file names
    report_data_file = results_dir / results_name
    report_data_file = report_data_file.with_suffix('.results')

    # output_dir = results_dir / 'reports'
    template_file = reporting_dir / 'model_report.Rmd'
    output_file = reports_dir / ('%s_model_report.pdf' % report_data_file.stem)
    build_dir = reports_dir / output_file.stem

    check_file = make_report(template_file,
                             report_data_file,
                             report_python_dir = repo_dir,
                             creation_script = reporting_dir / 'make_report.R',
                             output_file = output_file,
                             build_dir = build_dir,
                             clean = True,
                             quiet = True,
                             remove_build = False,
                             remove_output = False)

# merged results
report_pdfs = [reports_dir / ('%s_model_report.pdf' % n) for n in all_results_names]
merged_pdf = reports_dir / ('%s_%s_C%02d_%s_all_models.pdf' % (data_name, method_name, max_coefficient, fold_id))
pdf = merge_pdfs(pdf_files = report_pdfs, merged_file = merged_pdf)

open_file(pdf)
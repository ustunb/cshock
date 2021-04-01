from dcptree.paths import *
from dcptree.data import *
from dcptree.data_io import load_raw_data_from_disk, save_data
from dcptree.cross_validation import generate_cvindices, generate_stratified_cvindices
from dcptree.group_helper import get_group_outcome_strata, oversample_by_group, get_group_count_table
import pandas as pd

# directories

data_names = ['compas_arrest', 'compas_violent', 'apnea', 'credit', 'lungcancer']
oversample = True
random_seed = 2338

##### convert protected attributes to categorical
for data_name in data_names:

    dataset_file = '%s/%s/%s_data.csv' % (data_dir, data_name, data_name)

    for oversample in (True, False):

        if oversample:
            save_file_header = '%s/%s_bl_processed' % (data_dir, data_name)
        else:
            save_file_header = '%s/%s_processed' % (data_dir, data_name)

        data = load_raw_data_from_disk(dataset_file = dataset_file)
        data['format'] = FORMAT_NAME_DCP

        for name in get_variable_names(data):
            if data['variable_types'][name] not in ('boolean', 'numeric'):
                print(variable_summary(data, name))

        # checks before saving
        for name in get_partition_names(data):
            assert data['variable_types'][name] == 'categorical', 'variable %s is not categorical' % name

        for name in get_variable_names(data):
            assert data['variable_types'][name] in ('boolean', 'numeric'), 'variable %s must be boolean or numeric' %name

        if oversample:
            df_before = get_group_count_table(data)
            data = oversample_by_group(data, ratio = 'all', random_state = random_seed)
            df_after = get_group_count_table(data)

        cvindices = generate_stratified_cvindices(X = data['X'],
                                                  strata = get_group_outcome_strata(data),
                                                  total_folds_for_cv = [1, 3, 4, 5],
                                                  total_folds_for_inner_cv = [],
                                                  replicates = 3,
                                                  seed = random_seed)

        # save data and cv indices to disk
        save_data(file_name = save_file_header + '.pickle',
                  data = data,
                  cvindices = cvindices,
                  overwrite = True,
                  stratified = True,
                  check_save = True)

        save_data(file_name = save_file_header + '.RData',
                  data = data,
                  cvindices = cvindices,
                  overwrite = True,
                  stratified = True,
                  check_save = False)

        print('data_name: %s saved' % data_name)
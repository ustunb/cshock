from riskslim.paths import *
from riskslim.data import *
from riskslim.data_io import load_raw_data_from_disk, save_data
from riskslim.cross_validation import generate_cvindices, generate_stratified_cvindices

# directories
data_names = [
              'cshock',  'cshock_cts'
              ]
oversample = False
random_seed = 2338

##### convert protected attributes to categorical
for data_name in data_names:

    dataset_file = '%s/raw data files/%s_data.csv' % (data_dir, data_name)
    dataset_test_file = '%s/raw data files/%s_data_test.csv' % (data_dir, data_name)

    if oversample:
        save_file_header = '%s/%s_bl_processed' % (data_dir, data_name)
    else:
        save_file_header = '%s/%s_processed' % (data_dir, data_name)

    data = load_raw_data_from_disk(dataset_file = dataset_file)
    data['format'] = FORMAT_NAME_DEFAULT

    # append dataset
    data_test = load_raw_data_from_disk(dataset_file = dataset_test_file)
    data['X_test'] = data_test['X']
    data['Y_test'] = data_test['Y']

    # add intercept
    add_intercept(data, idx = 0)

    # check training data
    check_data(data, ready_for_training = True)
    for name in get_variable_names(data):
        assert data['variable_types'][name] in ('boolean', 'numeric'), 'variable %s must be boolean or numeric' %name

    # if oversample:
    #     df_before = get_group_count_table(data)
    #     data = oversample_by_group(data, ratio = 'all', random_state = random_seed)
    #     df_after = get_group_count_table(data)
    cvindices = generate_stratified_cvindices(X = data['X'],
                                              strata = data['Y'],
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
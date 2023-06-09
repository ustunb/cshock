
def get_results_name(data_name, method_name, max_L0_value = 5, max_coefficient = 5, fold_id = 'K05N01', fold_num = 0):
    if 'riskslim' in method_name:
        name = '%s_%s_C%01d_L%01d_%s_%d' % (data_name, method_name, max_coefficient, max_L0_value, fold_id, fold_num)

    elif 'plr' in method_name:
        name = '%s_%s_%s' % (data_name, method_name, fold_id)
    return name

def get_baseline_results_name(data_name, method_name, max_L0_value = 5, fold_id = 'K05N01', fold_num = 0):
    name = '%s_%s_L%01d_%s_%d' % (data_name, method_name, max_L0_value, fold_id, fold_num)
    return name

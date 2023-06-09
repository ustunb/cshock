import dill
from riskslim.paths import data_dir, results_dir
from riskslim.data_io import load_processed_data
from riskslim.cross_validation import split_data_by_cvindices
from riskslim.coefficient_set import CoefficientSet
from riskslim.lattice_cpa import setup_lattice_cpa, finish_lattice_cpa
from riskslim.helper_functions import print_model
from riskslim.utils import *

data_name = 'yosemite_st5'
method_name = 'riskslim'
max_coefficient = 1
max_runtime = 60
fold_id = 'K05N01'
fold_num = 0


data_file = '%s/%s_processed.pickle' % (data_dir, data_name)
data, cvindices = load_processed_data(data_file)


def train_riskslim_model(data_name, max_L0_value = 5, max_coefficient = 5, max_runtime = 60, fold_id = 'K05N01', fold_num = 0):

    # setup names
    method_name = 'riskslim'
    data_file = '%s/%s_processed.pickle' % (data_dir, data_name)

    data, cvindices = load_processed_data(data_file)
    data = split_data_by_cvindices(data, cvindices, fold_id = fold_id, fold_num = fold_num)

    # Model classes
    c0_value = 0.001
    coef_set = CoefficientSet(variable_names = data['variable_names'], lb = -max_coefficient, ub = max_coefficient)
    coef_set.update_intercept_bounds(data['X'], data['Y'], max_offset = 100)

    #### Training
    constraints = {
        'L0_min': 0,
        'L0_max': max_L0_value,
        'coef_set':coef_set,
        }

    settings = {
        'c0_value': c0_value,
        #
        # LCPA Settings
        'max_runtime': max_runtime,                    # max runtime for LCPA
        'max_tolerance': 0.001,                  # tolerance to stop LCPA (set to 0 to return provably optimal solution)
        'display_cplex_progress': True,         # print CPLEX progress on screen
        'loss_computation': 'normal',           # how to compute the loss function ('normal','fast','lookup')
        #
        # RiskSLIM MIP settings
        'drop_variables': False,
        #
        # LCPA Improvements
        'display_progress': True,
        'round_flag': False,                                # round continuous solutions with SeqRd
        'polish_flag': False,                               # polish integer feasible solutions with DCD
        'chained_updates_flag': False,                      # use chained updates
        'add_cuts_at_heuristic_solutions': False,            # add cuts at integer feasible solutions found using polishing/rounding
        #
        'initialization_flag': False,                       # use initialization procedure
        'init_max_runtime': 60.0,                          # max time to run CPA in initialization procedure
        #
        # CPLEX Solver Parameters
        'cplex_randomseed': 0,                              # random seed
        'cplex_mipemphasis': 0,                             # cplex MIP strategy
        }

    # initialize MIP for lattice CPA
    mip_objects = setup_lattice_cpa(data, constraints, settings)

    # todo: use commented code to add constraints
    # # add custom constraints
    #
    # mip, indices = mip_objects['mip'], mip_objects['indices']
    #
    # cons = mip.linear_constraints
    # vars = mip.variables
    #
    # variable_names = coef_set.variable_names
    # get_alpha_name = lambda var_name: 'alpha_' + str(variable_names.index(var_name))
    # get_alpha_ind = lambda var_names: [get_alpha_name(v) for v in var_names]
    # get_rho_name = lambda var_name: 'rho_' + str(variable_names.index(var_name))
    # get_rho_ind = lambda var_names: [get_rho_name(v) for v in var_names]

    # pass MIP back to lattice CPA so that it will solve
    model_info, mip_info, lcpa_info = finish_lattice_cpa(data, constraints, mip_objects, settings)
    print_model(model_info['solution'], data)

    #model info contains key results
    results = {
        #
        'data_name': data_name,
        'data_file': data_file,
        'max_L0_value': max_L0_value,
        'max_coefficient': max_coefficient,
        'method_name': method_name,
        'fold_id': 'K05N01',
        'fold_num': fold_num,
        #
        'objective_value': model_info['objective_value'],
        'optimality_gap': model_info['optimality_gap'],
        'upper_bound': lcpa_info['upperbound'],
        'lower_bound': lcpa_info['lowerbound'],
        'c0_value': model_info['c0_value'],
        #
        'coefs': model_info['solution'],
        }


    return results



for t in [7]:
    for fold_num in range(0, 6):

        results_file = results_dir / get_results_name(data_name, method_name, t, max_coefficient, fold_id, fold_num)
        results_file = results_file.with_suffix('.results')

        results = train_riskslim_model(data_name, max_L0_value = t, max_coefficient = max_coefficient, max_runtime = max_runtime, fold_id = fold_id, fold_num = fold_num)

        # save results
        with open(results_file, 'wb') as outfile:
            dill.dump(results, outfile, protocol = dill.HIGHEST_PROTOCOL, recurse = True)

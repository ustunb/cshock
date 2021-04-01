import dill
import pandas as pd
import numpy as np
from riskslim.paths import data_dir, results_dir
from riskslim.defaults import INTERCEPT_NAME
from cplex import SparsePair
from riskslim.data import load_data_from_csv
from riskslim.helper_functions import print_model_cdquest
from riskslim.coefficient_set import CoefficientSet
from riskslim.lattice_cpa import setup_lattice_cpa, finish_lattice_cpa

all_model_classes = ['basic', 'thresholded_01', 'thresholded_03', 'thresholded_05']

### Setup
def train_model(model_class = 'basic', max_questions = 1, max_coefficient = 5, max_runtime = 60.0):

    #model_class = all_model_classes[0]
    #max_questions = 5
    #max_runtime = 60
    data_name = "cdquest"                                       # name of the data
    training_study_name = 'TempleUGs'
    data_csv_file = '%s/%s_processed.csv' % (data_dir, data_name)     # csv file for the dataset
    info_csv_file = '%s/%s_info.csv' % (data_dir, data_name)     # csv file for the dataset
    results_file = '%s/%s_%s_Q%02d_C%02d.results' % (results_dir, data_name, model_class, max_questions, max_coefficient)

    # load data from disk
    all_data = load_data_from_csv(file_name = data_csv_file)

    # filter dataset to temple UGs
    info = pd.read_csv(info_csv_file)
    all_study_names = info['study'].unique()

    data = dict(all_data)
    data['X'] = all_data['X'][info['study'] == training_study_name]
    data['Y'] = all_data['Y'][info['study'] == training_study_name]
    assert np.isin(data['X'], (0, 1)).all()

    # problem parameters
    questions = list(range(1, 15 + 1))
    response_levels = [1, 2, 3, 4, 5]
    question_names = ['q%02d' % k for k in questions]
    response_titles = ['_geq_%d' % k for k in response_levels]

    # Model classes
    c0_value = 0.05
    coef_set = CoefficientSet(variable_names = data['variable_names'], lb = 0, ub = max_coefficient)
    coef_set[INTERCEPT_NAME].ub = 90
    coef_set[INTERCEPT_NAME].lb = -90

    if model_class == 'basic':
        max_thresholds_per_question = 1
        for v in coef_set.variable_names:
            if (v != INTERCEPT_NAME) and ('geq_1' not in v):
                coef_set[v].ub = 0

    elif 'thresholded' in model_class:
        max_thresholds_per_question =  int(model_class.replace('thresholded_', ''))

    # set limits
    max_thresholds_per_question = min(max_thresholds_per_question, len(response_levels))
    max_L0_value = max_questions * max_thresholds_per_question
    max_score = max_L0_value * max_coefficient

    # update intercept
    coef_set[INTERCEPT_NAME].ub = max(coef_set[INTERCEPT_NAME].ub, max_score)
    coef_set[INTERCEPT_NAME].lb = min(coef_set[INTERCEPT_NAME].lb, max_score)

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
        'max_tolerance': 0.01,                  # tolerance to stop LCPA (set to 0 to return provably optimal solution)
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
        'initialization_flag': True,                       # use initialization procedure
        'init_max_runtime': 30.0,                          # max time to run CPA in initialization procedure
        #
        # CPLEX Solver Parameters
        'cplex_randomseed': 0,                              # random seed
        'cplex_mipemphasis': 0,                             # cplex MIP strategy
    }

    # initialize MIP for lattice CPA
    mip_objects = setup_lattice_cpa(data, constraints, settings)

    # add custum constraints
    mip, indices = mip_objects['mip'], mip_objects['indices']
    cons = mip.linear_constraints
    vars = mip.variables

    variable_names = coef_set.variable_names
    get_alpha_name = lambda var_name: 'alpha_' + str(variable_names.index(var_name))
    get_alpha_ind = lambda var_names: [get_alpha_name(v) for v in var_names]
    get_rho_name = lambda var_name: 'rho_' + str(variable_names.index(var_name))
    get_rho_ind = lambda var_names: [get_rho_name(v) for v in var_names]

    q_use_names = {}
    q_use_indices = {}
    for q in question_names:

        # key variables
        q_use_name = 'use_%s' % q
        q_variable_names = ['%s%s' % (q, r) for r in response_titles]
        q_variable_limit = min(len(q_variable_names), max_thresholds_per_question)

        # add variable gamma[k] = 1[any variable from question k is used]
        vars.add(obj = [0.0], types = ['B'], names = [q_use_name])
        q_use_indices[q] = len(vars.get_names()) - 1
        q_use_names[q] = q_use_name

        # add constraint to set gamma[k] = 1 if any variable for question k is used
        # M * gamma[j] >= \sum alpha[j,k]
        # where M = max(\sum alpha[j,k])
        cons.add(names = ["def_%s" % q_use_name],
                 lin_expr = [SparsePair(ind = [q_use_indices[q]] + get_alpha_ind(q_variable_names), val = [q_variable_limit] + [-1.0] * len(q_variable_names))],
                 senses = "G",
                 rhs = [0.0])

        # add constraint to limit number of responds to MAX_THRESHOLDS]
        if max_thresholds_per_question < len(q_variable_names):
            cons.add(names = ['max_thresholds_for_%s' % q],
                     lin_expr = [SparsePair(ind = get_alpha_ind(q_variable_names), val = len(q_variable_names) * [1.0])],
                     senses = "L",
                     rhs = [max_thresholds_per_question])

        # add constraint to ensure that sum of coefficients is capped
        if max_thresholds_per_question > 1:
            cons.add(names = ["sum_coefs_%s" % q_use_name],
                     lin_expr = [SparsePair(ind = get_rho_ind(q_variable_names), val = [1.0] * len(q_variable_names))],
                     senses = "L", rhs = [max_coefficient])

    # add constraint to limit the number of questions
    vars.add(obj = [0.0], lb = [0.0], ub = [max_questions], types = ['I'], names = ['question_norm'])
    question_norm_ind = len(vars.get_names()) - 1
    cons.add(names = ["def_question_norm"],
             lin_expr = [SparsePair(ind = [question_norm_ind] + list(q_use_indices.values()), val = [1.0] + [-1.0] * len(questions))],
             senses = "E",
             rhs = [0.0])

    mip_objects['mip'] = mip

    # pass MIP back to lattice CPA so that it will solve
    model_info, mip_info, lcpa_info = finish_lattice_cpa(data, constraints, mip_objects, settings)

    #model info contains key results
    results = {
        #
        'data_name': data_name,
        'data_file_name': data_csv_file,
        'info_file_name': info_csv_file,
        'results_file_name': results_file,
        'model_class': model_class,
        'max_questions': max_questions,
        'max_coefficient': max_coefficient,
        #
        'training_study_name': training_study_name,
        'all_study_names': all_study_names,
        #
        'objective_value': model_info['objective_value'],
        'optimality_gap': model_info['optimality_gap'],
        'upper_bound': lcpa_info['upperbound'],
        'lower_bound': lcpa_info['lowerbound'],
        'c0_value': model_info['c0_value'],
        #
        'coefs': model_info['solution']
        }

    # save results
    with open(results_file, 'wb') as outfile:
        dill.dump(results, outfile, protocol = dill.HIGHEST_PROTOCOL, recurse = True)

    try:
        print_model_cdquest(model_info['solution'], variable_names = data['variable_names'], X = data['X'])
    except:
        pass

    return results

for model_class in all_model_classes:
    for max_questions in range(1, 11):
        results = train_model(model_class = model_class, max_questions = max_questions, max_runtime = 300)
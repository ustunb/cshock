from copy import deepcopy
from riskslim.cross_validation import *
from riskslim.data import get_variable_indices, has_intercept

import pandas as pd
from rpy2.robjects import numpy2ri, packages, r
from rpy2.robjects.packages import importr
from rpy2.robjects.vectors import FloatVector




# install glmnet if it hasn't already been installed
if not packages.isinstalled(name = 'glmnet'):
    utils = packages.importr('utils')
    utils.chooseCRANmirror(ind = 1)
    utils.install_packages('glmnet')


def fit_glmnet(y, X, family='binomial', alpha = 0.0, nlambda = 100, **kwargs):
    """
    :param y:
    :param X:
    :param family: response type
    :param alpha:
    :param nlambda:
    :param intercept:
    :param kwargs: dictionary of keyword arguments to pass the glmnet function in R.
    :return: pandas dataframe containing the fit model parameters. Each row corresponds to a unique value for lambda.
    """

    # R set-up
    numpy2ri.activate()
    base = importr('base')
    glmnet = importr('glmnet')
    n = X.shape[0]

    # change label to 0 / 1
    rY = np.array(y).flatten()
    rY[rY <= 0] = 0
    rY = np.vstack([1 - rY, rY]).transpose()
    rY = r.matrix(rY, nrow = n)
    rX = r.matrix(X, nrow = n)

    output = glmnet.glmnet(x = rX, y = rY, family = family, alpha = alpha, nlambda = nlambda, intercept = True,  **kwargs)
    coefs = np.array(base.as_matrix(glmnet.coef_glmnet(output))).transpose()
    lambda_ = np.array(output.rx('lambda')[0])

    df = pd.DataFrame(data = {
        'alpha': np.repeat(alpha, len(lambda_)),
        'lambda_': lambda_,
        'coefs': [list(w) for w in coefs],
        })

    return df

#
# def sparse_to_full_matrix(sparse_matrix):
#     """
#     :param sparse_matrix: A sparse R matrix
#     :return: The corresponding full R matrix
#     """
#     func_string = 'as_matrix <- function(x){return(as.matrix(x))}'
#     as_matrix = STAP(func_string, "as_matrix")
#     return as_matrix.as_matrix(sparse_matrix)


def fit_glmnet_cv(data, cvindices, fold_id, family= "binomial", alpha = 0.0, nlambda = 100, **kwargs):
    """
    Runs glmnet with cross-validation and saves all models from each run.
    :param data: The data dictionary.
    :param cvindices: The cross-validation indices as a dictionary.
    :param fold_id: A string specifying the folds to use for cross-validation from the cv_indices dictionary.
    :param family: The response type. Passed to glmnet.
    :param glmnet_kwargs: Keyword arguments to pass to glmnet. See glmnet documentation for details.
    :return: pandas dataframe containing all models and their accuracies across the combined train/test data.
    """
    # train glmnet models on all data first. This will provide a set of values for the lambda parameter
    var_idx = np.arange(data['X'].shape[1])
    if has_intercept(data):
        var_idx = get_variable_indices(data)

    final_df = fit_glmnet(y = data['Y'], X = data['X'][:, var_idx], family=family,  alpha = alpha, nlambda = nlambda, **kwargs)
    final_df['fold'] = 0
    kwargs['lambda'] = FloatVector(final_df['lambda_'])

    # fit glmnet models on folds using the same values of the lambda parameter
    df_list = [final_df]
    for fold in np.unique(cvindices[fold_id]):
        data_fold = split_data_by_cvindices(deepcopy(data), cvindices, fold_id = fold_id, fold_num = int(fold))
        df = fit_glmnet(y = data_fold['Y'], X = data_fold['X'][:, var_idx], family=family,  alpha = alpha, nlambda = nlambda, **kwargs)
        df['fold'] = fold
        df_list.append(df)

    df = pd.concat(df_list)
    return df



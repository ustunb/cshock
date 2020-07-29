#Pipeline Helper Functions
#https://github.com/ustunb/classification-pipeline
#Berk Ustun | www.berkustun.com

#This function contains helper functions used in the following scripts;
#train_models.R
#process_result.R
#aggregate_riskslim_results.R
#algorithms_plots.R

required_packages = c('lubridate', 'gtools', 'Hmisc', 'dplyr')
for (pkg in required_packages){
    suppressPackageStartupMessages(library(pkg, character.only = TRUE, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE));
}

#### constants ####

DEFAULT_OUTCOME_NAME = "Y = +1"
INTERCEPT_NAME = "(Intercept)"

.LINEAR_METHOD_NAMES = c("lars_lasso", "lars_ridge", "lars_elasticnet", "rounded_elasticnet", "svm_linear")
.BLACKBOX_METHOD_NAMES = c("sgb","randomforest","svm_rbf")

get.ahrs.method.names = function(){

    AHRS_FEATURE_SELECTION_ID = c("forward_stepwise"="fwd",
                                  #"backward_stepwise"="bwd",
                                  "elastic_net"="enet")

    AHRS_ROUNDING_ID = c("none" = "none",
                         "unit"="unit",
                         "capped"="rnd",
                         "scaled"="srnd")

    AHRS_RISK_MODEL_ID = c("logit"="logit",
                           "platt"="platt",
                           "histogram"="hist")

    #build table of ahrs combinations to train
    ahrs_method_names =
        expand.grid(fsl = AHRS_FEATURE_SELECTION_ID,
                    rnd = AHRS_ROUNDING_ID,
                    rsk = AHRS_RISK_MODEL_ID,
                    stringsAsFactors = FALSE) %>%
        mutate(method_name = sprintf("ahrs_%s_%s_%s", fsl, rnd, rsk)) %>%
        pull(method_name)

    return(ahrs_method_names)
}

AHRS_METHODS = get.ahrs.method.names()

AHRS_METHODS_CONTINUOUS = grep("none", AHRS_METHODS, value = TRUE)

AHRS_METHODS_DISCRETE = setdiff(AHRS_METHODS, AHRS_METHODS_CONTINUOUS)

METHODS_WITH_HISTOGRAM_MODELS = grep("hist", AHRS_METHODS, value = TRUE)

METHODS_WITH_DISCRETE_SCORE_FUNCTIONS = c("slim",
                                          "risk_slim",
                                          "rounded_elasticnet",
                                          "RSLP_CR",
                                          "RSLP_LR",
                                          "RSLP_SR",
                                          "RSLP_CRLR",
                                          "RSLP_LRCR",
                                          "RSLP_DCD",
                                          "RSLP_LRDCD",
                                          "RSLP_CRDCD",
                                          "RSLP_SRDCD",
                                          AHRS_METHODS_DISCRETE);

METHODS_WITH_SCORE_FUNCTIONS = c("lars_elasticnet",
                                 "lars_lasso",
                                 "lars_ridge",
                                 "svm_linear",
                                 METHODS_WITH_DISCRETE_SCORE_FUNCTIONS,
                                 AHRS_METHODS_CONTINUOUS);



#### general purpose ####

set.if.missing = function(settings, field_name, default_value){
    if (!(field_name %in% names(settings))){
        settings[[field_name]] = default_value;
    }
    return(settings);
}

get.if.missing = function(settings, field_name, default_value){ s
    if (field_name %in% names(settings)){
        return(settings[[field_name]]);
    } else {
        return(default_value);
    }
    return(settings);
}

safe.dir = function(dir_name){
    last_char = substr(dir_name,nchar(dir_name),nchar(dir_name));
    if (last_char != "/") {
        dir_name = paste0(dir_name,"/");
    }
    return(dir_name);
}

safe.extension = function(file_name, extension = ".RData"){
    extension = gsub("\\.", "", extension);
    file_name = gsub(paste0("\\.",extension), "", ignore.case = TRUE, x = file_name);
    file_name = paste0(file_name, ".", extension);
    return(file_name);
}

is.zero = function(x, tol = .Machine$double.eps ^ 0.5){
    return(sapply(x, FUN = function(e) isTRUE(all.equal(e, 0.0, tolerance = tol))))
}

#### data ####

get.folds.from.cvindices = function(cvindices, fold_id = "K05N01", inner_fold_id = "NONE"){

    fold_list = list()

    if (fold_id %in% names(cvindices)){
        fold_list$folds = cvindices[[fold_id]]
    } else {
        fold_size_key = substring(fold_id, 1, 3)
        fold_rep_idx = as.integer(substring(fold_id, 5, 6))
        stopifnot(fold_size_key %in% names(cvindices))
        fold_list$folds = cvindices[[fold_size_key]][,fold_rep_idx]
    }

    if (inner_fold_id != "NONE") {
        inner_fold_key = paste0(fold_id, "_", inner_fold_id)
        stopifnot(inner_fold_key %in% names(cvindices))
        fold_list$inner_folds = cvindices[[inner_fold_key]]
    }

    return(fold_list);
}


load.data.file = function(data_file_name, fold_id = "K05N01", inner_fold_id = "NONE", sample_weight_id = "NONE"){

    #check that hcon_id/inner_fold_id/sample_weight_id are in the correct format
    stopifnot(any(grep("K[[:digit:]]{2}N[[:digit:]]{2}", fold_id)),
              hcon_id == "NONE" || any(grep("[[:alpha:]]{0,}[[:digit:]]{0,3}", hcon_id)),
              inner_fold_id == "NONE" || any(grep("F[[:digit:]]{2}K[[:digit:]]{2}", inner_fold_id)),
              sample_weight_id == "NONE" || any(grep("W[[:digit:]]{1,3}", sample_weight_id)));

    data_environment = new.env();
    load(data_file_name, data_environment);
    data = as.list(data_environment);

    #initialize global variables for helper functions
    X 				= data$X;
    Y               = data$Y;
    if (all(c("X_test", "Y_test") %in% names(data))){
        X_test          = data$X_test;
        Y_test          = data$Y_test;
    }

    if (!is.null(colnames(X))) {
        X_names = colnames(X)
    } else if (("X_names" %in% data) & (ncol(X)==length(data$X_names))){
        X_names = data$X_names
        colnames(X) = data$X_names
    } else {
        X_names = sprintf("V%03d", seq(ncol(X)))
    }

    if (!is.null(colnames(Y))) {
        Y_name = colnames(Y)
    } else if ("Y_name" %in% data){
        Y_name = data$Y_name
    } else {
        Y_name = DEFAULT_OUTCOME_NAME;
    }

    #sample weights
    if (sample_weight_id == "NONE"){
        sample_weights = matrix(rep(1, nrow(X)));
        sample_weights_test = matrix(rep(1, nrow(X_test)));
    } else {
        sid = as.numeric(gsub("W", "", sample_weight_id));
        sample_weights = matrix(data$sample_weights[,sid], ncol = 1);
        sample_weights_test = matrix(data$sample_weights_test[,sid], ncol = 1);
    }
    has_sample_weights = any(sample_weights != 1) || any(sample_weights_test != 1);

    #folds
    fold_list = get.folds.from.cvindices(data$cvindices, fold_id, inner_fold_id)
    folds = fold_list$folds;

    if (inner_fold_id != "NONE"){

        stopifnot("inner_folds" %in% names(fold_list))
        inner_folds = fold_list$inner_folds;
        inner_K = as.double(substring(inner_fold_id, 5, 6))

        stopifnot(max(inner_folds) == inner_K,
                  min(inner_folds) == 1);

        outer_fold_num = as.double(substring(inner_fold_id, 2, 3));
        test_ind = folds == outer_fold_num;
        train_ind = !test_ind;
        X_test = X[test_ind,];
        Y_test = Y[test_ind,];
        X = X[train_ind,];
        Y = Y[train_ind,];
        sample_weights_test = matrix(sample_weights[test_ind,], ncol = 1);
        sample_weights = matrix(sample_weights[train_ind,], ncol = 1);
        folds = inner_folds;

        print.to.console(sprintf("using inner CV folds"));
        print.to.console(sprintf("outer_fold_id: %s", fold_id));
        print.to.console(sprintf("inner_fold_id: %s", inner_fold_id));
        print.to.console(sprintf("%d-fold CV on data in fold %d", inner_K, outer_fold_num));
    }

    K 				= max(folds);
    ind_pos         = Y == 1;
    ind_pos_test    = Y_test == 1;
    ind_neg         = !ind_pos;
    ind_neg_test    = !ind_pos_test;
    Y[ind_neg]      = 0;
    Y_test[ind_neg_test]= 0;

    #return key variables for pipeline
    loaded_data = list(X = X,
                       Y = Y,
                       X_names = X_names,
                       Y_name = Y_name,
                       X_test = X_test,
                       Y_test = Y_test,
                       ind_pos = ind_pos,
                       ind_neg = ind_neg,
                       ind_pos_test = ind_pos_test,
                       ind_neg_test = ind_neg_test,
                       has_test_set = nrow(X_test) > 0,
                       sample_weights = sample_weights,
                       sample_weights_test = sample_weights_test,
                       has_sample_weights = has_sample_weights,
                       folds = folds,
                       K = K);

    return(loaded_data);
}


load.data.file.fast = function(data_file_name, fold_id = "K05N01", inner_fold_id = "NONE", sample_weight_id = "NONE", add_intercept_column = FALSE){

    data = new.env();
    load(data_file_name, data);
    data = as.list(data);
    data = data[c("X", "Y", "X_test", "Y_test", "cvindices", "sample_weights", "sample_weights_test")]
    data_field_names = names(data);

    #sample weights
    data$has_sample_weights = (!is.na(sample_weight_id)) && (sample_weight_id != "NONE") && ("sample_weights" %in% data_field_names) && ("sample_weights_test" %in% data_field_names);
    if (data$has_sample_weights){
        sid = as.numeric(gsub("W", "", sample_weight_id));
        data$sample_weights = matrix(data$sample_weights[,sid], ncol = 1);
        data$sample_weights_test = matrix(data$sample_weights_test[,sid], ncol = 1);
    }

    #folds
    fold_list = get.folds.from.cvindices(data$cvindices, fold_id, inner_fold_id)
    data$folds = fold_list$folds;
    inner_folds_field = paste0(fold_id, "_", inner_fold_id);
    has_inner_folds = inner_folds_field %in% names(data$cvindices);
    if (has_inner_folds){
        inner_K = as.double(substring(inner_fold_id, 5, 6))
        outer_fold_num = as.double(substring(inner_fold_id, 2, 3));

        test_ind = data$folds == outer_fold_num;
        data$X_test = data$X[test_ind,];
        data$Y_test = data$Y[test_ind,];

        train_ind = !test_ind;
        data$X = data$X[train_ind,];
        data$Y = data$Y[train_ind,];

        data$folds = data$cvindices[[inner_folds_field]][,1];

        if (data$has_sample_weights){
            data$sample_weights_test = data$sample_weights[test_ind,];
            data$sample_weights = data$sample_weights[train_ind,];
        }
    }
    data$cvindices = NULL;

    #add intercept
    if (add_intercept_column){
        data$X = cbind('(Intercept)' = 1, data$X);
        if ("X_test" %in% names(data) && nrow(data$X_test) > 0){
            data$X_test = cbind('(Intercept)' = 1, data$X_test);
        }
    }

    neg_idx = data$Y <= 0
    data$Y[neg_idx] = 0
    if ("Y_test" %in% names(data)){
        neg_idx = data$Y_test <= 0
        data$Y_test[neg_idx] = 0
    }

    return(data);
}


get.number.of.variables.in.dataset = function(data_file_name = NULL, data_name = NULL){
    if (is.null(data_file_name)){
        if (exists("data_dir")){
            data_file_name = paste0(data_dir, data_name, "_processed.RData")
        } else {
            data_file_name = paste0(data_name, "_processed.RData")
        }
    }
    safe.extension(data_file_name, ".RData");
    stopifnot(file.exists(data_file_name));
    data = new.env();
    load(data_file_name, data);
    return(ncol(data$X));
}


load.coefficient.set = function(data_file_name,
                                hcon_id = "NONE",
                                use_custom_coefficient_set = FALSE,
                                max_offset = -1,
                                max_coefficient = 10,
                                max_L0_value = -1){

    data_environment = new.env();
    load(data_file_name, data_environment);
    data = as.list(data_environment);
    variable_names = colnames(data$X);

    #drop intercept
    if (INTERCEPT_NAME %in% variable_names){
        to_keep = variable_names != INTERCEPT_NAME
        variable_names = variable_names[to_keep]
        data$X = data$X[, to_keep]
    }

    P = length(variable_names);

    valid_hcon_id = any(grep("[[:alpha:]]{0,}[[:digit:]]{1,}", hcon_id))
    hid = as.numeric(gsub("[a-zA-Z]","", hcon_id));
    valid_hcon_id = valid_hcon_id && (hid > 0);

    if (use_custom_coefficient_set){
        L0_min = 0;
        L0_max = ifelse(max_L0_value == -1, P, max_L0_value);
        coef_lb = -rep(max_coefficient, P);
        coef_ub = rep(max_coefficient, P);
        if (max_offset >= 0){
            intercept_ub = max_offset;
            intercept_lb = -max_offset;
        } else if (max_offset == -1){
            Y_bin = data$Y;
            Y_bin[data$Y == 0,] = -1;
            Z = apply(data$X, MARGIN = 2, function(x){x * Y_bin})
            intercept_lb = min(rowSums(pmin(Z * coef_ub, Z*coef_lb)));
            intercept_ub = max(rowSums(pmax(Z * coef_ub, Z*coef_lb)));
        }
    } else if (valid_hcon_id) {
        L0_min = data$hard_constraints$L0_min[hid];
        L0_max = data$hard_constraints$L0_max[hid];
        coef_lb = data$hard_constraints$coef_lb[, hid];
        coef_ub = data$hard_constraints$coef_ub[, hid];
        intercept_lb = data$hard_constraints$intercept_lb[hid];
        intercept_ub = data$hard_constraints$intercept_ub[hid];
    } else {
        L0_min = 0;
        L0_max = P;
        coef_lb = -rep(Inf, P);
        coef_ub = rep(Inf, P);
        intercept_lb = -Inf;
        intercept_ub = Inf;
    }

    stopifnot(length(intercept_lb) == 1,
              length(intercept_ub) == 1,
              length(coef_lb) == P,
              length(coef_ub) == P,
              all(coef_lb <= coef_ub),
              intercept_lb <= intercept_ub,
              L0_min <= L0_max)

    hard_constraints = list(
        "max_offset" = ifelse(use_custom_coefficient_set, max_offset, max(abs(intercept_lb), abs(intercept_ub))),
        "max_coefficient" = max(max(abs(coef_lb)), max(abs(coef_ub))),
        "max_L0_value" = L0_max,
        "intercept_lb" = intercept_lb,
        "intercept_ub" = intercept_ub,
        "coef_lb" = coef_lb,
        "coef_ub" = coef_ub,
        "L0_min" = L0_min,
        "L0_max" = L0_max,
        "Lj_min" = c(intercept_lb, coef_lb),
        "Lj_max" = c(intercept_ub, coef_ub)
    )
    return(hard_constraints);
}

# Count Tables
create.count.table = function(X, Y, X_test, Y_test, folds){

    #check preconditions
    stopifnot(nrow(X) == nrow(Y),
              nrow(X) == length(folds),
              ncol(X) == ncol(X_test),
              nrow(X_test) == nrow(Y_test))

    #create count table
    pos_ind = Y==1
    neg_ind = !pos_ind
    pos_test_ind = Y_test == 1;
    neg_test_ind = !pos_test_ind;

    n_folds = max(folds)
    count_table = data.frame(k = seq(0, n_folds)) %>%
        mutate(P = ncol(X),
               N = nrow(X),
               N_pos = sum(pos_ind),
               N_neg = sum(neg_ind),
               N_test = nrow(X_test),
               N_test_pos = sum(pos_test_ind),
               N_test_neg = sum(neg_test_ind),
               N_all = N + N_test,
               N_all_pos = N_pos + N_test_pos,
               N_all_neg = N_neg + N_test_neg) %>%
        rowwise() %>%
        mutate(N_train = sum((folds != k)),
               N_train_pos = sum((folds != k) & pos_ind),
               N_train_neg = sum((folds != k) & neg_ind),
               N_valid = sum(folds == k),
               N_valid_pos = sum((folds == k) & pos_ind),
               N_valid_neg = sum((folds == k) & neg_ind)) %>%
        ungroup() %>%
        rename(fold = k)

    stopifnot(check.count.table(count_table))
    return(count_table);
}


create.weighted.count.table = function(X, Y, X_test, Y_test, folds, sample_weights, sample_weights_test){

    #check preconditions
    stopifnot(nrow(X) == nrow(Y),
              nrow(X) == length(folds),
              ncol(X) == ncol(X_test),
              nrow(X_test) == nrow(Y_test),
              nrow(X) == length(sample_weights),
              nrow(X_test) == length(sample_weights_test))

    #create count table
    pos_ind = Y==1;
    neg_ind = !pos_ind
    pos_test_ind = Y_test == 1;
    neg_test_ind = !pos_test_ind;

    n_folds = max(folds);
    count_table = data.frame(k = seq(0, n_folds)) %>%
        mutate(P = ncol(X),
               N = sum(sample_weights),
               N_pos = sum(sample_weights[pos_ind]),
               N_neg = sum(sample_weights[neg_ind]),
               N_test = sum(sample_weights_test),
               N_test_pos = sum(sample_weights_test[pos_test_ind]),
               N_test_neg = sum(sample_weights_test[neg_test_ind]),
               N_all = N + N_test,
               N_all_pos = N_pos + N_test_pos,
               N_all_neg = N_neg + N_test_neg) %>%
        rowwise() %>%
        mutate(N_train = sum(sample_weights[folds != k]),
               N_train_pos = sum(sample_weights[(folds != k) & pos_ind]),
               N_train_neg = sum(sample_weights[(folds != k) & neg_ind]),
               N_valid = sum(sample_weights[folds == k]),
               N_valid_pos = sum(sample_weights[(folds == k) & pos_ind]),
               N_valid_neg = sum(sample_weights[(folds == k) & neg_ind])) %>%
        ungroup() %>%
        select(fold = k, everything())
    stopifnot(check.count.table(count_table))
    return(count_table);
}


check.count.table = function(count_table){

    full_model_idx = count_table$fold == 0
    fold_model_idx = count_table$fold > 0

    stopifnot(
        #total = positive + negative for all sets
        all(count_table$N==count_table$N_pos+count_table$N_neg),
        all(count_table$N_test==count_table$N_test_pos + count_table$N_test_neg),
        all(count_table$N_all == count_table$N + count_table$N_test),
        all(count_table$N_train == count_table$N_train_pos + count_table$N_train_neg),
        all(count_table$N_valid == count_table$N_valid_pos + count_table$N_valid_neg),
        #
        #no validation set for full model
        count_table$N_valid[full_model_idx]==0,
        count_table$N_valid_pos[full_model_idx]==0,
        count_table$N_valid_neg[full_model_idx]==0,
        #
        #total validation set for fold models == total training points for full model
        count_table$N_train[full_model_idx]==sum(count_table$N_valid[fold_model_idx]),
        count_table$N_train_pos[full_model_idx]==sum(count_table$N_valid_pos[fold_model_idx]),
        count_table$N_train_neg[full_model_idx]==sum(count_table$N_valid_neg[fold_model_idx])
    )
    return(TRUE)
}

# Error Checking
check.results.rep =  function(results, count_table){

    fnames = names(results);
    stopifnot("method_name" %in% fnames,
              "results_df" %in% fnames,
              "print_models" %in% fnames,
              "debug_models" %in% fnames,
              "total_runtime" %in% fnames,
              "method_settings" %in% fnames);

    method_name = results$method_name;
    results_df = results$results_df;
    print_models = results$print_models;
    debug_models = results$print_models;
    method_settings = results$method_settings;

    has_test_set = all(count_table$N_test > 0);

    #check that the total number of runs = n_parameter_combinations x n_folds
    n_folds = max(results_df$fold)
    n_runs = results_df %>% select(fold, starts_with("parameter_")) %>% distinct() %>% nrow()
    n_parameter_combinations = results_df %>% select(starts_with("parameter_")) %>% distinct() %>% nrow()
    stopifnot(n_runs == (n_folds + 1) * n_parameter_combinations)

    #check that each fold has the same number of runs
    stopifnot(all(results_df %>% group_by(fold) %>% tally() %>% pull(n) == n_parameter_combinations))

    #check that each parameter combination has K + 1 models
    parameter_colnames = results_df %>% select(starts_with("parameter_")) %>% colnames()
    stopifnot(all(results_df %>% group_by_(.dots = parameter_colnames) %>% tally() %>% pull(n) == (n_folds + 1)))

    #check run time metrics
    stopifnot(results$total_runtime >= 0.0, results_df$runtime >= 0.0)

    #join results_df onto count_table
    results_df = left_join(results_df, count_table, by = "fold")

    #check model size metrics

    if (method_name %in% .LINEAR_METHOD_NAMES){
        stopifnot(results_df %>% rowwise() %>% mutate(chk = between(model_size, 0, P))  %>% ungroup() %>% pull(chk) %>% all())
    } else if (method_name %in% .BLACKBOX_METHOD_NAMES) {
        stopifnot(results_df %>% pull(model_size) %>% is.na() %>% all())
    }

    #check accuracy metrics for fold models
    stopifnot(
        #
        #N_train_pos == TP_train + FN_train
        results_df %>% mutate(chk = N_train_pos == (train_true_positives + train_false_negatives)) %>% pull(chk) %>% all(),
        #
        #N_train_neg == TN_train + FP_train
        results_df %>% mutate(chk = N_train_neg == (train_true_negatives + train_false_positives)) %>% pull(chk) %>% all(),
        #
        #N_valid_pos == TP_valid + FN_valid
        results_df %>% filter(fold > 0) %>% mutate(chk = N_valid_pos == (valid_true_positives + valid_false_negatives)) %>% pull(chk) %>% all(),
        #
        #N_valid_neg == TN_valid + FP_valid
        results_df %>% filter(fold > 0) %>% mutate(chk = N_valid_neg == (valid_true_negatives + valid_false_positives))  %>% pull(chk) %>% all(),
        #
        #make sure no accuracy metrics for fold entries are NA
        !(results_df %>% filter(fold > 0) %>% select(starts_with("train"), starts_with("valid")) %>% is.na() %>% any())
    )

    #make sure accuracy metrics on train/test set for full models are 1) not NA, and 2) are > 0
    chk_table = results_df %>% filter(fold == 0) %>% select(starts_with("train"))
    stopifnot(all(!is.na(chk_table)), all(chk_table >= 0))

    #make sure accuracy metrics on validation set for full models are NA
    stopifnot(results_df %>% filter(fold == 0) %>% select(starts_with("valid")) %>% is.na() %>% all())

    stopifnot(
        #
        #true positives <= all positives
        results_df %>% mutate(chk = (train_true_positives <= N_train_pos)) %>% pull(chk) %>% all(),
        results_df %>% filter(fold > 0) %>% mutate(chk = valid_true_positives <= N_valid_pos) %>% pull(chk) %>% all(),
        #
        #false negatives <= all positives
        results_df %>% mutate(chk = train_false_negatives <= N_train_pos) %>% pull(chk) %>% all(),
        results_df %>% filter(fold > 0) %>% mutate(chk = valid_false_negatives <= N_valid_pos) %>% pull(chk) %>% all(),
        #
        #true negatives <= all negatives
        results_df %>% mutate(chk = train_true_negatives <= N_train_neg) %>% pull(chk) %>% all(),
        results_df %>% filter(fold > 0) %>% mutate(chk = valid_true_negatives <= N_valid_neg) %>% pull(chk) %>% all(),
        #
        #false positivies <= all negatives
        results_df %>% mutate(chk = (train_false_positives <= N_train_neg)) %>% pull(chk) %>% all(),
        results_df %>% filter(fold > 0) %>% mutate(chk = (valid_true_negatives <= N_valid_neg)) %>% pull(chk) %>% all()
    )

    if (has_test_set){

        stopifnot(
            #
            # N_test_pos == TP_test + FN_test
            results_df %>% mutate(chk = N_test_pos == (test_true_positives + test_false_negatives)) %>% pull(chk) %>% all(),
            #
            # N_test_neg == TN_test + FP_test
            results_df %>% mutate(chk = N_test_neg == (test_true_negatives + test_false_positives)) %>% pull(chk) %>% all(),
            #
            # make sure no test metrics for fold entries are NA
            !(results_df %>% filter(fold > 0) %>% select(starts_with("test")) %>% is.na() %>% any()),
            #
            #check that class-based accuracy metrics < total size of class
            results_df %>% mutate(chk = test_false_positives <= N_test_neg) %>% pull(chk) %>% all(),
            results_df %>% mutate(chk = test_true_negatives <= N_test_neg) %>% pull(chk) %>% all(),
            results_df %>% mutate(chk = test_false_negatives <= N_test_pos) %>% pull(chk) %>% all(),
            results_df %>% mutate(chk = test_true_positives <= N_test_pos) %>% pull(chk) %>% all()
        )

        #make sure test metrics on train/test set for full models are 1) not NA, and 2) are > 0
        chk_table = results_df %>% filter(fold == 0) %>% select(starts_with("test"))
        stopifnot(all(!is.na(chk_table)), all(chk_table >= 0))

    }
    return(TRUE);
}


#### loading and merging results files ####


load.processed.results = function(file_names){
    n_files = length(file_names);
    if (n_files == 1){
        file_name = safe.extension(file_names[1], extension = ".RData");
        processed_results = new.env();
        load(file_name, envir = processed_results);
        return(processed_results);
    } else {
        return(combine.processed.results(file_names));
    }
}


combine.processed.results = function(file_names){

    n_files = length(file_names);
    processed_results = vector('list', n_files);
    for (i in 1:n_files){
        file_name = safe.extension(file_names[i], extension = ".RData");
        processed_results[[i]] = new.env();
        load(file_name, envir = processed_results[[i]]);
    }
    processed_results = lapply(X = processed_results, FUN = function(r){as.list(r)});

    #merge
    processed_results = merge.results.and.models(results_df_list = lapply(processed_results, function(r) r$results_df),
                                                 print_models_list = lapply(processed_results, function(r) r$print_models),
                                                 debug_models_list = lapply(processed_results, function(r) r$debug_models));

    #drop other entries
    processed_results = processed_results[c("results_df", "print_models", "debug_models")];

    #recompute summary statistics
    processed_results$stats_df = create.stats.from.fold.results(processed_results$results_df);
    return(processed_results);

}


tabulate.results.files = function(input_file_pattern = ".RData", results_path = getwd()){

    #get all matching files
    raw_matched_files = dir(path = results_path,
                            pattern = input_file_pattern,
                            recursive = TRUE,
                            ignore.case = TRUE);

    raw_matched_files = basename(raw_matched_files);

    #extract identifiers
    matched_files = raw_matched_files;
    all_data_names = unique(gsub('_F_K', '', regmatches(matched_files, regexpr("[[:print:]]{1,}_F_K", matched_files))))
    for (data_name in all_data_names){
        matched_files = gsub(data_name, "", matched_files);
    }

    all_old_weight_names = unique(regmatches(matched_files, regexpr("neg_[[:digit:]]{1,}.[[:digit:]]{0,}_pos_[[:digit:]]{1,}.[[:digit:]]{0,}", matched_files)))
    for (weight_name in all_old_weight_names){
        matched_files = gsub(weight_name, "", matched_files);
    }
    if (length(all_old_weight_names) == 0){
        all_old_weight_names = NA;
    }

    all_new_weight_names = unique(regmatches(matched_files, regexpr("pos_[[:digit:]]{1,}.[[:digit:]]{0,}", matched_files)))
    for (weight_name in all_new_weight_names){
        matched_files = gsub(weight_name, "", matched_files);
    }
    if (length(all_new_weight_names) == 0){
        all_new_weight_names = NA;
    }

    all_fold_ids = unique(gsub('_F_', '', regmatches(matched_files, regexpr("_F_K[[:digit:]]{2}N[[:digit:]]{2}", matched_files))));

    all_inner_fold_ids = unique(gsub('_I_', '', regmatches(matched_files, regexpr("_F[[:digit:]]{2}K[[:digit:]]{2}", matched_files))));
    all_inner_fold_ids = c(all_inner_fold_ids, "NONE", NA);

    all_sample_weight_ids = unique(gsub('_W_', '', regmatches(matched_files, regexpr("_[[:digit:]]{2}", matched_files))));
    all_sample_weight_ids = c(all_sample_weight_ids, "NONE", NA);

    all_hcon_ids = unique(gsub('_L_', '', regmatches(matched_files, regexpr("_L_U[[:digit:]]{3}", matched_files))))

    all_xtra_ids = unique(gsub('_X_', '', c(regmatches(matched_files, regexpr("_X_[[:alnum:]]{0,}", matched_files)),
                                            regmatches(matched_files, regexpr("_X_[[:alnum:]]{1,}(_[[:alnum:]]{1,}){0,}", matched_files)))))

    #list each individual run_name as a table
    results_file_table = expand.grid(data_name = all_data_names,
                                     hcon_id = all_hcon_ids,
                                     xtra_id = all_xtra_ids,
                                     fold_id = all_fold_ids,
                                     inner_fold_id = all_inner_fold_ids,
                                     sample_weight_id = all_sample_weight_ids,
                                     old_weight_name = all_old_weight_names,
                                     new_weight_name = all_new_weight_names) %>%
        mutate(train_name = paste0(data_name,"_F_", fold_id),
               train_dir = safe.dir(paste0(results_path, train_name,"/")),
               file_name="");


    #locate unique files on disk
    located_files_table = list();
    for (i in 1:nrow(results_file_table)){

        file_info = results_file_table %>% slice(i);
        file_regexp = sprintf('%s.*%s', file_info$train_name, input_file_pattern);
        found_on_disk = any(grep(file_regexp, raw_matched_files));

        if (found_on_disk){
            file_names = raw_matched_files[grep(sprintf("%s.*%s",file_info$train_name,input_file_pattern), raw_matched_files)];
            located_files_table[[i]] = data.frame(data_name=file_info$data_name,
                                                  train_dir=file_info$train_dir,
                                                  file_name=file_names, stringsAsFactors = FALSE);
        }
    }

    located_files_table = bind_rows(located_files_table) %>%
        filter(nchar(file_name)>0) %>%
        mutate(file_name = paste0(train_dir, file_name)) %>%
        distinct();

    return(located_files_table);
}


merge.results.and.models = function(results_df_list, print_models_list, debug_models_list, print_model_cnt = 0, debug_model_cnt = 0){

    #convert all logical parameter values to numeric values
    for (method_name in names(results_df_list)){

        param_value_cols = results_df_list[[method_name]] %>%
            select(starts_with("parameter_")) %>%
            select(ends_with("_value")) %>%
            colnames();

        for (c in param_value_cols){
            vals = results_df_list[[method_name]] %>% pull(c)
            if (is.logical(vals)){
                results_df_list[[method_name]][[c]] = as.numeric(vals)
            }
            if (is.factor(vals)){
                results_df_list[[method_name]][[c]] = as.character(vals)
            }
        }
    }

    # merge results and renumber all print/model ids sequentially
    results_df = bind_rows(results_df_list) %>%
        mutate(id = row_number(),
               print_model_id = sprintf("M%08d", print_model_cnt + id),
               debug_model_id = sprintf("M%08d", debug_model_cnt + id))

    #rename print_models and debug_models sequentially
    for (i in 1:length(results_df_list)){
        names(print_models_list[[i]]) = sprintf("M%08d", print_model_cnt + seq(1, length(print_models_list[[i]])));
        names(debug_models_list[[i]]) = sprintf("M%08d", debug_model_cnt + seq(1, length(debug_models_list[[i]])));
        print_model_cnt = print_model_cnt + length(print_models_list[[i]]);
        debug_model_cnt = debug_model_cnt + length(debug_models_list[[i]]);
    }
    names(print_models_list) = NULL #need to remove names of methods from list before unlist
    names(debug_models_list) = NULL #need to remove names of methods from list before unlist
    print_models = unlist(print_models_list, recursive = FALSE, use.names = TRUE);
    debug_models = unlist(debug_models_list, recursive = FALSE, use.names = TRUE);

    #return list with merged entities
    merged = list(results_df = results_df,
                  print_models = print_models,
                  debug_models = debug_models,
                  print_model_cnt = print_model_cnt,
                  debug_model_cnt = debug_model_cnt);

    return(merged);
}


merge.stats.results.and.models = function(stats_df_list, results_df_list, print_models_list, debug_models_list){


    #deal with arrays that have missing entries
    to_drop = sapply(stats_df_list, is.null) | sapply(results_df_list, is.null) | sapply(print_models_list, is.null) | sapply(debug_models_list, is.null);
    to_keep = !to_drop;
    stopifnot(any(to_keep))
    stats_df_list = stats_df_list[to_keep];
    results_df_list = results_df_list[to_keep];
    print_models_list = print_models_list[to_keep];
    debug_models_list = debug_models_list[to_keep];

    #check that all lists have the same length
    n = length(stats_df_list);
    stopifnot(length(results_df_list) == n);
    stopifnot(length(print_models_list) == n);
    stopifnot(length(debug_models_list) == n);

    #merge results and renumber all print/model ids sequentially
    print_model_cnt = 0;
    debug_model_cnt = 0;

    for (i in 1:n){

        old_print_ids = names(print_models_list[[i]]);
        old_debug_ids = names(debug_models_list[[i]]);

        results_df_list[[i]]$print_model_id
        results_df_list[[i]] = results_df_list[[i]] %>%
            filter(print_model_id %in% old_print_ids,
                   debug_model_id %in% old_debug_ids) %>%
            mutate(id = row_number(),
                   print_model_id = sprintf("M%08d", print_model_cnt + id),
                   debug_model_id = sprintf("M%08d", debug_model_cnt + id)) %>%
            select(-id);

        print_id_dictionary = results_df_list[[i]] %>% pull(print_model_id);
        debug_id_dictionary = results_df_list[[i]] %>% pull(debug_model_id);
        names(print_id_dictionary) = old_print_ids;
        names(debug_id_dictionary) = old_debug_ids;

        #rename model objects
        names(print_models_list[[i]]) = print_id_dictionary[old_print_ids];
        names(debug_models_list[[i]]) = debug_id_dictionary[old_debug_ids];

        #rename model ids in stats
        stats_df_list[[i]]$print_model_id.final = print_id_dictionary[stats_df_list[[i]]$print_model_id.final];
        stats_df_list[[i]]$debug_model_id.final = debug_id_dictionary[stats_df_list[[i]]$debug_model_id.final];

        #rename model ids in "related models"
        if ("final_print_model_id" %in% colnames(results_df_list[[i]])){
            results_df_list[[i]]$final_print_model_id = print_id_dictionary[results_df_list[[i]]$final_print_model_id];
        }

        if ("final_debug_model_id" %in% colnames(results_df_list[[i]])){
            results_df_list[[i]]$final_debug_model_id = print_id_dictionary[results_df_list[[i]]$final_debug_model_id];
        }

        #validation
        #stopifnot(all(!is.na(stats_df_list[[i]]$print_model_id.final)))
        #stopifnot(all(stats_df_list[[i]]$print_model_id.final %in%  print_id_dictionary))
        # stopifnot(all(stats_df_list[[i]]$debug_model_id.final %in%  debug_id_dictionary))
        #stopifnot(all(names(print_models_list[[i]])  %in%  print_id_dictionary))
        #stopifnot(all(names(debug_models_list[[i]]) %in%  debug_id_dictionary))

        print_model_cnt = print_model_cnt + length(print_models_list[[i]]);
        debug_model_cnt = debug_model_cnt + length(debug_models_list[[i]]);

    }

    names(print_models_list) = NULL #need to remove names of methods from list before unlist
    names(debug_models_list) = NULL #need to remove names of methods from list before unlist
    print_models = unlist(print_models_list, recursive = FALSE, use.names = TRUE);
    debug_models = unlist(debug_models_list, recursive = FALSE, use.names = TRUE);

    stats_df = bind_rows(stats_df_list)
    results_df = bind_rows(results_df_list)

    #return list with merged entities
    merged = list(stats_df = stats_df,
                  results_df = results_df,
                  print_models = print_models,
                  debug_models = debug_models);

    return(merged);

}


reorder.results.df = function(df){

    #rearranges columns of a results data frame so that all fields after "fold" column is specific to a single model
    df = df %>%
        select(data_name,
               fold_id, inner_fold_id, xtra_id, hcon_id, sample_weight_id,
               w_pos, w_neg, w_pos_norm, w_neg_norm,
               method_name,
               comp_name, date,
               P, N,
               starts_with("N_"),
               starts_with("parameter_"),
               fold,
               print_model_id, debug_model_id,
               runtime,
               #
               #variables that will be summarized
               #
               contains("total_data_time"),
               contains("total_solver_time"),
               contains("nodes_processed"),
               contains("upperbound"),
               contains("lowerbound"),
               contains("relative_gap"),
               contains("model_size"),
               starts_with("train"),
               starts_with("valid"),
               starts_with("test"))

    return(df)
}


fix.parameters.in.stats.or.results.list = function(df_list){

    n_blocks = length(df_list)
    all_parameter_dfs = vector("list", n_blocks)

    strip_parameter_name = function(s){gsub("_name", "", gsub("parameter_", "", s))}
    strip_parameter_value = function(s){gsub("_value", "", gsub("parameter_", "", s))}

    for (n in 1:n_blocks){

        if (!is.null(df_list[[n]])){

            parameter_names_df = df_list[[n]] %>%
                slice(1) %>%
                select(starts_with("parameter_")) %>%
                select(ends_with("name")) %>%
                rename_all(strip_parameter_name) %>%
                t()

            parameter_types_df = df_list[[n]] %>%
                slice(1) %>%
                select(starts_with("parameter_")) %>%
                select(ends_with("value")) %>%
                mutate_if(is.integer, as.numeric) %>%
                mutate_if(is.factor, as.character) %>%
                mutate_all(class) %>%
                rename_all(strip_parameter_value) %>%
                t()

            all_parameter_dfs[[n]] =
                bind_cols(
                    data.frame("parameter_name" = parameter_names_df),
                    data.frame("parameter_type" = parameter_types_df)
                ) %>%
                mutate(
                    block_id = n,
                    parameter_id = row_number()
                )

        }
    }
    all_parameter_dfs = bind_rows(all_parameter_dfs)

    parameter_translator = all_parameter_dfs %>%
        distinct(parameter_name, parameter_type) %>%
        mutate(new_parameter_id = row_number()) %>%
        merge(data.frame(block_id = seq(1, n_blocks))) %>%
        left_join(all_parameter_dfs, by = c("block_id", "parameter_name", "parameter_type")) %>%
        arrange(block_id, new_parameter_id, parameter_name, parameter_type, parameter_id) %>%
        select(block_id, new_parameter_id, parameter_name, parameter_type,  parameter_id) %>%
        distinct(block_id, new_parameter_id, parameter_name, parameter_type, parameter_id) %>%
        mutate(name_colname = sprintf("parameter_%d_name", new_parameter_id),
               value_colname = sprintf("parameter_%d_value", new_parameter_id),
               old_name_colname = sprintf("parameter_%d_name", parameter_id),
               old_value_colname = sprintf("parameter_%d_value", parameter_id))

    parameter_filler = parameter_translator %>%
        select(new_parameter_id, parameter_name, parameter_type, name_colname, value_colname) %>%
        distinct() %>%
        arrange(new_parameter_id) %>%
        select(-new_parameter_id)

    n_total_parameters = parameter_filler %>% nrow()
    parameter_filler_df = data.frame("to_remove" = 1,
                                     stringsAsFactors = FALSE,
                                     row.names = NULL,
                                     check.rows = FALSE,
                                     check.names = FALSE,
                                     fix.empty.names = FALSE)

    for (j in 1:n_total_parameters){

        tb = parameter_filler %>% slice(j)
        parameter_filler_df[[tb$name_colname]] = as.character(tb$parameter_name)
        if (is.na(tb$parameter_type)){
            parameter_filler_df[[tb$value_colname]] = as.numeric(NA)
        } else if (tb$parameter_type == "numeric"){
            parameter_filler_df[[tb$value_colname]] = as.numeric(NA)
        } else if (tb$parameter_type == "factor"){
            parameter_filler_df[[tb$value_colname]] = as.character(NA)
        } else {
            parameter_filler_df[[tb$value_colname]] = as.character(NA)
        }
    }
    parameter_filler_df = parameter_filler_df %>%
        select(-to_remove)

    for (bid in 1:n_blocks){

        if (!is.null(df_list[[n]])){

            df = df_list[[bid]]
            if ("fold" %in% colnames(df)){
                df = reorder.results.df(df)
            }
            df_cols = colnames(df)

            param_idx = which(grepl("parameter", df_cols))

            #generate empty parameter_df
            new_parameter_df = parameter_filler_df %>%
                merge(data.frame(rep = 1:nrow(df))) %>%
                select(-rep)

            if (any(param_idx)){

                param_start_idx = min(param_idx)
                param_end_idx = max(param_idx)
                colnames_before_param = df_cols[1:(param_start_idx-1)]
                colnames_after_param = df_cols[(param_end_idx+1):length(df_cols)]

                generator_df = parameter_translator %>% filter(block_id == bid, !is.na(parameter_id))

                #fill in current values from existing columns
                for (j in 1:nrow(generator_df)){
                    p = generator_df %>% slice(j)
                    new_parameter_df[[p$name_colname]] = df[[p$old_name_colname]]
                    new_parameter_df[[p$value_colname]] = df[[p$old_value_colname]]
                }

            } else {

                if ("fold" %in% df_cols){
                    idx = which(df_cols == "fold")
                    colnames_before_param = df_cols[1:(idx-1)]
                    colnames_after_param = df_cols[idx:length(df_cols)]
                } else if ("N_all_neg" %in% df_cols) {
                    #should be stats_df
                    idx = which(df_cols == "N_all_neg")
                    colnames_before_param = df_cols[1:idx]
                    colnames_after_param = df_cols[(idx+1):length(df_cols)]
                } else {
                    stopifnot("cannot determine type")
                }

            }

            #merge updated parameter with current columns
            df = bind_cols(df %>% select(colnames_before_param),
                           new_parameter_df,
                           df %>% select(colnames_after_param))

            if ("fold" %in% colnames(df)){
                df = reorder.results.df(df)
            }

            df_list[[bid]] = df;
        }
    }
    return(df_list)
}


#### creating stats_df statistics #####

create.stats.from.fold.results = function(df){

    df = reorder.results.df(df)

    #### compute summary stats #####

    #no summary stats for final model (i.e. model trained with ALL training data)
    final_df = df %>%
        filter(fold == 0) %>%
        select(-starts_with("N_valid"),
               -starts_with("N_train"),
               -ends_with("_positives"),
               -ends_with("_negatives"),
               -starts_with("valid"));

    final_fields = colnames(final_df);
    fold_idx = match("fold", final_fields)
    n_final_fields = length(final_fields)

    key_idx = seq(1, fold_idx - 1);
    key_fields = final_fields[key_idx]

    stats_idx = seq(fold_idx + 1, n_final_fields);
    final_fields[stats_idx] = paste(final_fields[stats_idx], "final", sep=".");
    colnames(final_df) = final_fields;

    final_df = final_df %>% select(-fold);

    #compute summary stats for validation (fold-based)
    df = df %>%
        filter(fold > 0) %>%
        select(-starts_with("N_valid"),
               -starts_with("N_train"),
               -ends_with("_positives"),
               -ends_with("_negatives"),
               -debug_model_id,
               -print_model_id);

    stats_df = summarize.over.folds(fold_results_df = df)

    #join validation stats with final model stats
    stats_df = full_join(x=stats_df, y = final_df, by = key_fields);

    return(stats_df);
}


summarize.over.folds = function(fold_results_df){

    fields = colnames(fold_results_df);
    key_ind = seq(1, match("fold", fields) - 1);
    summary_ind = seq(match("fold", fields) + 1 , length(fields));
    key_fields = fields[key_ind];
    summary_fields = fields[summary_ind];

    fold_results_df = fold_results_df %>% select(-fold);

    ### ISSUE HERE
    stats_df = fold_results_df %>%
        group_by_(.dots = key_fields) %>%
        summarise_at(summary_fields, funs(min,median,max,mean,sd,total = sum)) %>%
        ungroup()

    field_naming_function   = function(x) paste(x, c("min","median","max","mean","sd","total"), sep="_")
    summarized_field_names  = as.vector(sapply(summary_fields, field_naming_function, USE.NAMES=FALSE));

    desired_naming_function = function(x) paste(x, c("min","med","max","mean","sd","total"), sep=".")
    desired_field_names     = as.vector(sapply(summary_fields, desired_naming_function, USE.NAMES=FALSE));

    for (i in 1:length(summarized_field_names)){
        current_fname = summarized_field_names[i];
        desired_fname = desired_field_names[i];
        colnames(stats_df) = gsub(current_fname, desired_fname, colnames(stats_df));
    }

    return(stats_df)
}


get.fold.models.from.final.model = function(row_df,
                                            results_df,
                                            has_inner_cv = FALSE,
                                            inner_cv_selected_df = NULL,
                                            stop_when_missing_inner_cv_models = TRUE){

    #returns a table with:
    #data_name, fold_id, inner_fold_id, fold, print_model_id
    stopifnot("print_model_id.final" %in% names(row_df), nrow(row_df) == 1)
    final_model_id = row_df$print_model_id.final
    final_results_df = results_df %>% filter(print_model_id == final_model_id)
    stopifnot(nrow(final_results_df) == 1)

    #get fold model ids from results_df

    if (has_inner_cv){
        stopifnot(!is.null(inner_cv_selected_df))
        inner_cv_helper = inner_cv_selected_df %>%
            filter(data_name == final_results_df$data_name,
                   fold_id == final_results_df$fold_id,
                   hcon_id == final_results_df$hcon_id,
                   xtra_id == final_results_df$xtra_id,
                   sample_weight_id == final_results_df$sample_weight_id,
                   method_name == final_results_df$method_name)

        outer_K = as.numeric(substr(final_results_df$fold_id, 2, 3));

        stopifnot(outer_K >= nrow(inner_cv_helper))
        if (stop_when_missing_inner_cv_models){
            stopifnot(outer_K == nrow(inner_cv_helper));
        }

        inner_K = inner_cv_helper %>%
            rowwise() %>%
            mutate(inner_K = as.numeric(substr(inner_fold_id, 5, 6))) %>%
            ungroup() %>%
            distinct(inner_K) %>%
            pull()
        stopifnot(length(inner_K) == 1);

        fold_model_info_df = inner_cv_helper %>%
            mutate(fold = 0) %>%
            select(data_name, fold_id, inner_fold_id, fold, print_model_id)

    } else {

        helper_df = left_join(x = final_results_df %>% select(data_name, fold_id, hcon_id, xtra_id, sample_weight_id, method_name),
                              y = results_df %>% filter(inner_fold_id == "NONE"),
                              by = c("data_name", "fold_id", "hcon_id", "xtra_id", "sample_weight_id", "method_name"))

        related_model_ids = get.fold.model.ids.from.any.model.id(final_model_id, helper_df)

        fold_model_ids = setdiff(related_model_ids, final_model_id)
        fold_model_info_df = helper_df %>%
            filter(print_model_id %in% fold_model_ids) %>%
            select(data_name, sample_weight_id, fold_id, inner_fold_id, fold, print_model_id)


        # OLD VERSION
        #
        # raw_df = raw_df %>%
        #     select(-P,
        #            -starts_with("N"),
        #            -contains("date"),
        #            -contains("comp_name"),
        #            -contains("model_id"));
        #
        # fold_position = which(names(raw_df) == "fold")
        # join_key = names(raw_df)[seq(1, fold_position)-1]
        # join_key = join_key[!(join_key %in% c("comp_name", "date", "fold"))]
        #
        # fold_model_info_df = raw_df  %>%
        #     select_(.dots = join_key) %>%
        #     left_join(results_df %>% filter(fold > 0)) %>%
        #     mutate(inner_fold_id = "NONE") %>%
        #     select(data_name, sample_weight_id, fold_id, inner_fold_id, fold, print_model_id)


    }
    return(fold_model_info_df);
}


get.fold.model.ids.from.any.model.id = function(print_model_ids, results_df){
    #given a set of "source" model_ids, returns a superset of "target" model_ids
    #"target" ids correspond to models trained on different folds of the data
    #"target" ids include the original source ids

    fold_col_idx = which(colnames(results_df) == "fold")
    stopifnot(fold_col_idx >= 1)

    source_key = results_df %>%
        select(seq(1, fold_col_idx - 1)) %>%
        select(-P, -starts_with("N"), -contains("date"), -contains("comp_name"), -contains("model_id")) %>%
        colnames()

    target_key = c(source_key, "print_model_id")

    target_ids = left_join(x = results_df %>%
                               filter(print_model_id %in% print_model_ids) %>%
                               distinct(print_model_id, .keep_all = TRUE) %>%
                               select_(.dots = source_key),
                           y = results_df %>%
                               select_(.dots = target_key),
                           by = source_key) %>%
        distinct(print_model_id) %>%
        pull(print_model_id)

    return(target_ids)
}


add.final.model.id.to.results.df = function(results_df){
    #for each model in results_df, add a label to the "final model"

    #in general, for all final models, the final model is itself
    final_model_df = results_df %>%
        filter(fold == 0) %>%
        mutate(final_print_model_id = print_model_id,
               final_debug_model_id = debug_model_id) %>%
        select(-P,
               -starts_with("N"),
               -contains("date"),
               -contains("comp_name"))

    #only keep run based information (data_name, fold_id... )
    fold_position = which(names(final_model_df) == "fold")
    join_key = names(final_model_df)[seq(1, fold_position)-1]
    join_key = join_key[!(join_key %in% c("comp_name", "date", "fold"))]
    selection_key = c(join_key, "final_print_model_id", "final_debug_model_id");

    #add in print_model_id and debug_model_id of final model to each row
    results_df = left_join(x = results_df,
                           y = final_model_df %>%  select_(.dots = selection_key),
                           by = join_key);
    stopifnot("final_print_model_id" %in% colnames(results_df));
    stopifnot("final_debug_model_id" %in% colnames(results_df));

    #for final models in an inner CV setup, the real final model is the outer CV final model
    inner_cv_final_model_results_df = results_df %>%
        filter(inner_fold_id != "NONE", fold == 0)  %>%
        select(-final_print_model_id, -final_debug_model_id)

    #safety measure:
    #remove final models of inner CV setup from results
    #model_ids of these models are correctly specified
    results_df = results_df %>% filter((inner_fold_id == "NONE") | (fold > 0))

    #update selection key to remove parameter names
    cols_to_drop = inner_cv_final_model_results_df %>% select(starts_with("parameter_")) %>% colnames();
    cols_to_drop = c(cols_to_drop, "inner_fold_id");
    join_key = setdiff(join_key, cols_to_drop);
    selection_key = setdiff(selection_key, cols_to_drop);

    #add in print_model_id and debug_model_id of final model to each row
    inner_cv_final_model_results_df = left_join(x = inner_cv_final_model_results_df,
                                                y = final_model_df %>% filter(inner_fold_id == "NONE") %>% select_(.dots = selection_key),
                                                by = join_key);

    stopifnot("final_print_model_id" %in% colnames(inner_cv_final_model_results_df));
    stopifnot("final_debug_model_id" %in% colnames(inner_cv_final_model_results_df));

    #merge results_df for both sets of models
    results_df = bind_rows(results_df, #fold based models for inner/outer CV + final models in outer CV
                           inner_cv_final_model_results_df #final models for inner CV
    )

    return(results_df)

}

#### score based computation ####

compute.score.based.metrics = function(scores, probabilities, true_labels, discrete_flag = FALSE){

    #compute helper
    N_CALIBRATION_ERROR_BINS = 10;
    N = length(scores);
    pos_idx = true_labels == 1;
    neg_idx = !pos_idx;
    N_pos = sum(pos_idx)
    N_neg = N - N_pos;

    #discrete calibration errors
    if (discrete_flag){
        distinct_probs = sort(unique(probabilities));
        calibration_breaks = c(-0.5, distinct_probs);
        calibration_indices = findInterval(x = probabilities, vec = calibration_breaks) - 1;
        #stopifnot(all(calibration_breaks[calibration_indices] == probabilities))
        #stopifnot(!any(calibration_indices==1))
        avg_cal_err_distinct = 0;
        max_cal_err_distinct = 0;
        n_distinct = length(distinct_probs);
        for (b in 1:n_distinct){
            ind = calibration_indices == b;
            if (any(ind, na.rm = TRUE)){
                actual = mean(true_labels[ind]);
                predicted = mean(probabilities[ind]);
                cal_err = abs(actual - predicted);
                max_cal_err_distinct = max(cal_err, max_cal_err_distinct);
                avg_cal_err_distinct = avg_cal_err_distinct + sum(ind) * cal_err;
            }
        }
        avg_cal_err_distinct = avg_cal_err_distinct/N;
    } else {
        n_distinct = NA;
        max_cal_err_distinct = NA;
        avg_cal_err_distinct = NA;
    }

    #continuous calibration errors
    calibration_breaks = seq(0, N_CALIBRATION_ERROR_BINS - 1)/N_CALIBRATION_ERROR_BINS;
    calibration_indices = findInterval(x = probabilities, vec = calibration_breaks);
    avg_cal_err_binned = 0;
    max_cal_err_binned = 0;
    for (b in 1:N_CALIBRATION_ERROR_BINS){
        ind = calibration_indices == b;
        if (any(ind, na.rm = TRUE)){
            actual = mean(true_labels[ind]);
            predicted = mean(probabilities[ind]);
            cal_err = abs(actual - predicted);
            max_cal_err_binned = max(cal_err, max_cal_err_binned);
            avg_cal_err_binned = avg_cal_err_binned + sum(ind) * cal_err;
        }
    }
    avg_cal_err_binned = avg_cal_err_binned/N;

    #AUC
    idx = order(probabilities, decreasing = TRUE);
    TP = cumsum(true_labels[idx]==1);
    FP = cumsum(true_labels[idx]==0);
    ## remove fp & tp for duplicated predictions
    #as duplicated keeps the first occurrence, but we want the last, two rev are used.
    keep_idx = !rev(duplicated(rev(probabilities[idx])));
    TP = c(0, TP[keep_idx]);
    FP = c(0, FP[keep_idx]);
    auc = 0;
    for (k in 2:length(FP)) {
        auc <- auc + abs(FP[k] - FP[k-1]) * (TP[k] + TP[k-1]);
    }
    auc = auc / (2 * N_neg * N_pos);

    #compute logistic loss
    scores[neg_idx] = -scores[neg_idx];
    pos_score_ind = scores > 0;
    neg_score_ind = !pos_score_ind;
    exp_scores_pos = exp(-scores[pos_score_ind]);
    exp_scores_neg = exp(scores[neg_score_ind]);
    mxe = (sum(log1p(exp_scores_pos))+ sum(-scores[neg_score_ind] + log1p(exp_scores_neg)))/N;

    #sanity checks for testing (use ROCR)
    #pred_object = prediction(probabilities, labels = true_labels);
    #auc_rocr = performance(pred_object, measure = "auc")@y.values[[1]];
    #mxe_rocr = performance(pred_object, measure = "mxe")@y.values[[1]];
    #print(sprintf("AUC %1.6f, %1.6f", auc, auc_rocr))
    #print(sprintf("MXE %1.6f, %1.6f", mxe, mxe_rocr))

    return(data.frame(auc = auc,
                      mxe = mxe,
                      n_distinct_scores = n_distinct,
                      max_cal_err_distinct = max_cal_err_distinct,
                      avg_cal_err_distinct = avg_cal_err_distinct,
                      n_bins = N_CALIBRATION_ERROR_BINS,
                      max_cal_err_binned = max_cal_err_binned,
                      avg_cal_err_binned = avg_cal_err_binned,
                      stringsAsFactors = FALSE,
                      check.names = FALSE,
                      check.rows = FALSE));

}


compute.weighted.score.based.metrics = function(scores, probabilities, true_labels, sample_weights, discrete_flag = FALSE){

    #compute helper
    N_CALIBRATION_ERROR_BINS = 10;
    pos_idx = true_labels == 1;
    neg_idx = !pos_idx;
    N = sum(sample_weights);
    N_pos = sum(sample_weights[pos_idx])
    N_neg = N - N_pos;

    #discrete calibration errors
    if (discrete_flag){
        distinct_probs = sort(unique(probabilities));
        calibration_breaks = c(-0.5, distinct_probs);
        calibration_indices = findInterval(x = probabilities, vec = calibration_breaks) - 1;
        #stopifnot(all(calibration_breaks[calibration_indices] == probabilities))
        #stopifnot(!any(calibration_indices==1))
        avg_cal_err_distinct = 0;
        max_cal_err_distinct = 0;
        n_distinct = length(distinct_probs);
        for (b in 1:n_distinct){
            ind = calibration_indices == b;
            if (any(ind,na.rm = TRUE)){
                total = sum(sample_weights[ind]);
                actual = sum(sample_weights[ind]*true_labels[ind])/total;
                predicted = mean(probabilities[ind]);
                cal_err = abs(actual - predicted);
                max_cal_err_distinct = max(cal_err, max_cal_err_distinct);
                avg_cal_err_distinct = avg_cal_err_distinct + total * cal_err;
            }
        }
        avg_cal_err_distinct = avg_cal_err_distinct/N;
    } else {
        n_distinct = NA;
        max_cal_err_distinct = NA;
        avg_cal_err_distinct = NA;
    }

    #continuous calibration errors
    calibration_breaks = seq(0, N_CALIBRATION_ERROR_BINS - 1)/N_CALIBRATION_ERROR_BINS;
    calibration_indices = findInterval(x = probabilities, vec = calibration_breaks);
    avg_cal_err_binned = 0;
    max_cal_err_binned = 0;
    for (b in 1:N_CALIBRATION_ERROR_BINS){
        ind = calibration_indices == b;
        if (any(ind,na.rm = TRUE)){
            total = sum(sample_weights[ind]);
            actual = sum(sample_weights[ind]*true_labels[ind])/total;
            predicted = mean(probabilities[ind]);
            cal_err = abs(actual - predicted);
            max_cal_err_binned = max(cal_err, max_cal_err_binned);
            avg_cal_err_binned = avg_cal_err_binned + total * cal_err;
        }
    }
    avg_cal_err_binned = avg_cal_err_binned/N;

    #AUC
    idx = order(probabilities, decreasing = TRUE);
    TP = cumsum(sample_weights[idx] * (true_labels[idx] == 1));
    FP = cumsum(sample_weights[idx] * (true_labels[idx] == 0));
    ## remove fp & tp for duplicated predictions
    #as duplicated keeps the first occurrence, but we want the last, two rev are used.
    keep_idx = !rev(duplicated(rev(probabilities[idx])));
    TP = c(0, TP[keep_idx]);
    FP = c(0, FP[keep_idx]);
    auc = 0;
    for (k in 2:length(FP)) {
        auc <- auc + abs(FP[k] - FP[k-1]) * (TP[k] + TP[k-1]);
    }
    auc = auc / (2 * N_neg * N_pos);

    #compute logistic loss
    scores[neg_idx] = -scores[neg_idx];
    pos_score_idx = scores > 0;
    neg_score_idx = !pos_score_idx;
    exp_scores_pos = exp(-scores[pos_score_idx]);
    exp_scores_neg = exp(scores[neg_score_idx]);
    mxe = (sum(sample_weights[pos_score_idx] * log1p(exp_scores_pos)) + sum(sample_weights[neg_score_idx] * (-scores[neg_score_idx] + log1p(exp_scores_neg))))/N

    #sanity checks for testing (use ROCR)
    #pred_object = prediction(probabilities, labels = true_labels);
    #auc_rocr = performance(pred_object, measure = "auc")@y.values[[1]];
    #mxe_rocr = performance(pred_object, measure = "mxe")@y.values[[1]];
    #print(sprintf("AUC %1.6f, %1.6f", auc, auc_rocr))
    #print(sprintf("MXE %1.6f, %1.6f", mxe, mxe_rocr))

    return(data.frame(auc = auc,
                      mxe = mxe,
                      n_distinct_scores = n_distinct,
                      max_cal_err_distinct = max_cal_err_distinct,
                      avg_cal_err_distinct = avg_cal_err_distinct,
                      n_bins = N_CALIBRATION_ERROR_BINS,
                      max_cal_err_binned = max_cal_err_binned,
                      avg_cal_err_binned = avg_cal_err_binned,
                      stringsAsFactors = FALSE,
                      check.names = FALSE,
                      check.rows = FALSE));

}


get.scores.logit = function(X, model){
    return(X %*% model);
}


get.probabilities.logit = function(scores, model){
    return(inv.logit(scores));
}


get.probabilities.histogram = function(scores, model){
    idx = findInterval(x = scores, vec = model$score_bins);
    return(model$risk_df %>% select("risk_pdf") %>% slice(idx) %>% pull());
}


get.scores.histogram = function(X, model){
    return(X %*% model$coefficients);
}




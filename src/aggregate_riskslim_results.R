#RiskSLIM results helper
#https://github.com/ustunb/classification-pipeline
#Berk Ustun | www.berkustun.com
#
#This script will aggregate all results files for a particular train_name
#compute accuracy related metrics and save all processed results as an
#RData file in the directory:${home_dir}/Run/${train_name}/${save_name}
#
#Note: this file must be run from home_dir to work correctly

##### parse command line arguments and setup directories #####
cat(sprintf("running aggregate_riskslim_results.R in directory\n%s\n", getwd()))
source("setup_pipeline.R");
args = commandArgs(TRUE);

batch_mode = (length(args) >= 1);
if (batch_mode){
    train_name = args[1];
    files_to_aggregate_regexp = args[2];
    aggregated_file_suffix = args[3];
    compute_all_metrics = ifelse(length(args)>3, ifelse(args[4]=="TRUE", TRUE, FALSE), FALSE);
    log_name = ifelse(length(args) > 4, args[5], NA);
} else {
    train_name = "pcl_c28_F_K10N01";
    files_to_aggregate_regexp = "*results_raw.RData";
    aggregated_file_suffix = "RSCV_results.RData";
    compute_all_metrics = FALSE;
    log_name = NA;
}
rm(args);

#set directories
log_file = set.log(log_dir = log_dir, log_name = log_name)
source(paste0(code_dir, "pipeline_helper_functions.R"));

#script helper functions
predict.for.logistic = function(coefs, newx){
    scores = coefs[1] + (newx %*% coefs[2:length(coefs)]);
    probabilities = inv.logit(scores);
    predictions = round(probabilities);
    return(predictions);
}

compute.accuracy.metrics = function(model, data, folds, foldnum){

    accuracy_stats = list(
        "train_true_positives" = NA,
        "train_true_negatives" = NA,
        "train_false_positives" = NA,
        "train_false_negatives" = NA,
        "valid_true_positives" = NA,
        "valid_false_positives" = NA,
        "valid_false_negatives" = NA,
        "valid_true_negatives" = NA,
        "test_true_positives" = NA,
        "test_true_negatives" = NA,
        "test_false_positives" = NA,
        "test_false_negatives" = NA
    );

    model = t(as.matrix(model));

    valid_ind = folds == foldnum;
    train_ind = !valid_ind;
    has_validation_set = any(valid_ind);
    has_testing_set = ("X_test" %in% names(data) && "Y_test" %in% names(data));

    if (has_testing_set){
        has_sample_weights = all(c("sample_weights", "sample_weights_test") %in% names(data));
    } else {
        has_sample_weights = "sample_weights" %in% names(data);
    }

    if (has_sample_weights){
        sample_weights = data$sample_weights;
        if (has_validation_set){
            sample_weights_valid = sample_weights[valid_ind];
            sample_weights = sample_weights[train_ind];
        }
        if (has_testing_set){
            sample_weights_test = data$sample_weights_test;
        }
    }

    #accuracy metrics on training set
    X_train = as.matrix(data$X[train_ind,])
    Y_train = as.matrix(data$Y[train_ind])
    ind_pos_train = Y_train == 1;
    ind_neg_train = !ind_pos_train;

    y_hat = predict.for.logistic(model, newx = X_train)

    if (has_sample_weights){

        accuracy_stats$train_true_positives = sum(sample_weights[ind_pos_train] * y_hat[ind_pos_train]==1);
        accuracy_stats$train_false_negatives = sum(sample_weights[ind_pos_train] * y_hat[ind_pos_train]!=1);
        accuracy_stats$train_true_negatives = sum(sample_weights[ind_neg_train] * y_hat[ind_neg_train]!=1);
        accuracy_stats$train_false_positives = sum(sample_weights[ind_neg_train] * y_hat[ind_neg_train]==1);

        #accuracy metrics on validation set
        if (has_validation_set){

            X_valid = as.matrix(data$X[valid_ind,]);
            Y_valid = as.matrix(data$Y[valid_ind]);
            ind_pos_valid = Y_valid == 1;
            ind_neg_valid = !ind_pos_valid;

            y_hat = predict.for.logistic(model, newx = X_valid)

            accuracy_stats$valid_true_positives = sum(sample_weights_valid[ind_pos_valid] * y_hat[ind_pos_valid]==1);
            accuracy_stats$valid_false_negatives = sum(sample_weights_valid[ind_pos_valid] * y_hat[ind_pos_valid]!=1);
            accuracy_stats$valid_true_negatives = sum(sample_weights_valid[ind_neg_valid] * y_hat[ind_neg_valid]!=1);
            accuracy_stats$valid_false_positives = sum(sample_weights_valid[ind_neg_valid] * y_hat[ind_neg_valid]==1);
        }

        #accuracy metrics on testing set
        if (has_testing_set){
            X_test = as.matrix(data$X_test);
            Y_test = as.matrix(data$Y_test);
            ind_pos_test = Y_test == 1;
            ind_neg_test = !ind_pos_test;

            y_hat = predict.for.logistic(model, newx = X_test);

            accuracy_stats$test_true_positives = sum(sample_weights_test[ind_pos_test] * y_hat[ind_pos_test]==1);
            accuracy_stats$test_false_negatives = sum(sample_weights_test[ind_pos_test] * y_hat[ind_pos_test]!=1);
            accuracy_stats$test_true_negatives = sum(sample_weights_test[ind_neg_test] * y_hat[ind_neg_test]!=1);
            accuracy_stats$test_false_positives = sum(sample_weights_test[ind_neg_test] * y_hat[ind_neg_test]==1);
        }

        } else {

        accuracy_stats$train_true_positives = sum(y_hat[ind_pos_train]==1);
        accuracy_stats$train_false_negatives = sum(y_hat[ind_pos_train]!=1);
        accuracy_stats$train_true_negatives = sum(y_hat[ind_neg_train]!=1);
        accuracy_stats$train_false_positives = sum(y_hat[ind_neg_train]==1);

        #accuracy metrics on validation set
        if (has_validation_set){

            X_valid = as.matrix(data$X[valid_ind,]);
            Y_valid = as.matrix(data$Y[valid_ind]);
            ind_pos_valid = Y_valid == 1;
            ind_neg_valid = !ind_pos_valid;

            y_hat = predict.for.logistic(model, newx = X_valid)

            accuracy_stats$valid_true_positives = sum(y_hat[ind_pos_valid]==1);
            accuracy_stats$valid_false_negatives = sum(y_hat[ind_pos_valid]!=1);
            accuracy_stats$valid_true_negatives = sum(y_hat[ind_neg_valid]!=1);
            accuracy_stats$valid_false_positives = sum(y_hat[ind_neg_valid]==1);
        }

        #accuracy metrics on testing set
        if (has_testing_set){
            X_test = as.matrix(data$X_test);
            Y_test = as.matrix(data$Y_test);
            ind_pos_test = Y_test == 1;
            ind_neg_test = !ind_pos_test;

            y_hat = predict.for.logistic(model, newx = X_test);

            accuracy_stats$test_true_positives = sum(y_hat[ind_pos_test]==1);
            accuracy_stats$test_false_negatives = sum(y_hat[ind_pos_test]!=1);
            accuracy_stats$test_true_negatives = sum(y_hat[ind_neg_test]!=1);
            accuracy_stats$test_false_positives = sum(y_hat[ind_neg_test]==1);
        }
    }
    return(accuracy_stats);

}

##### identify all files for a given data + fold_id that have results for a full set of folds #####
start_time = proc.time();
train_dir = paste0(results_dir, train_name, '/')
files_to_aggregate_regexp = safe.extension(file_name = files_to_aggregate_regexp, extension = "RData");
matched_files = dir(path=train_dir, files_to_aggregate_regexp, ignore.case=TRUE);

##### aggregate all files for training instances with the right number of folds #####
n_files = length(matched_files)
all_results_files = paste0(train_dir, matched_files);
all_results_df = data.frame();
all_print_models = list();
all_debug_models = list();

for (id in 1:n_files){

    pydata = new.env();
    load(all_results_files[id], pydata);

    #LOAD DATA
    data_name = as.character(pydata$info$data_name);
    data_file_name = paste0(data_dir, data_name, "_processed.RData");
    data_environment = new.env();
    load(data_file_name, data_environment);
    data = as.list(data_environment);
    rm(data_environment);

    if ("sample_weight_id" %in% names(pydata$info)){
        sample_weight_id = as.character(pydata$info$sample_weight_id);
    } else {
        sample_weight_id = "NONE"
    }

    if (sample_weight_id != "NONE"){
        sid = as.numeric(substr(sample_weight_id, start = 2, stop = nchar(sample_weight_id)));
        stopifnot(sid>=1);
        data$sample_weights = data$sample_weights[,sid];
        data$sample_weights_test = data$sample_weights_test[,sid];
    }

    #LOAD FOLDS (BACKWARDS COMPATIBLE)
    py_fold_id = as.character(pydata$info$fold_id);

    if ("inner_fold_id" %in% names(pydata$info)){
        inner_fold_id = as.character(pydata$info$inner_fold_id);
    } else {
        inner_fold_id = "NONE"
    }

    if (inner_fold_id == "NONE"){
        folds = data$cvindices[[substring(py_fold_id,1,3)]][,as.double(substr(py_fold_id,5,6))];
        if (is.null(folds)){
            folds = data$cvindices[[py_fold_id]]
        }
    } else {
        py_inner_fold_id = paste0(py_fold_id, "_", inner_fold_id)
        folds = data$cvindices[[py_inner_fold_id]]
    }

    if ("method_name" %in% names(pydata$info)){
        method_name = as.character(pydata$info$method_name);
    } else {
        method_name = "risk_slim";
    }

    #MODEL
    R_model_id = sprintf("M%08d", id)
    #mip_model = array(pydata$mip_model)
    #py_model_id = (pydata$results %>% filter(row_number() == n()) %>% select(incumbent_model_id))[[1]];
    #model = pydata$models[py_model_id,];
    #model_stats = pydata$model_stats %>% filter_(sprintf("model_id == %d", py_model_id));

    model = pydata$mip_model;
    variable_names = gsub("X.Intercept.", "(Intercept)", names(model));
    model = array(as.numeric(model));
    names(model) = variable_names;
    variable_ind = variable_names != "(Intercept)"
    intercept_ind = !variable_ind;
    model_size = sum(model[variable_ind] != 0.0);

    #MODEL STATISTICS
    if ("fold" %in% names(pydata$info)){
        foldnum = as.numeric(pydata$info$fold);
    } else {
        foldnum = as.numeric(pydata$info$fold_num)
    }

    hcon_id = as.character(pydata$info$hcon_id);
    hid = as.numeric(substr(hcon_id,2,nchar(hcon_id)));
    row_df = compute.accuracy.metrics(model, data, folds, foldnum);

    #info fields
    row_df$comp_name = as.character(pydata$info$comp_name);
    row_df$date = format(Sys.time(),"%m/%d/%y");
    row_df$data_name = as.character(pydata$info$data_name);
    row_df$method_name = method_name;
    row_df$fold_id = as.character(pydata$info$fold_id);
    row_df$inner_fold_id = inner_fold_id;
    row_df$sample_weight_id = sample_weight_id;
    row_df$hcon_id = as.character(pydata$info$hcon_id);
    row_df$xtra_id = as.character(pydata$info$xtra_id);
    row_df$w_pos = pydata$info$w_pos;
    row_df$w_neg = 1.0;
    tryCatch({row_df$date = format(parse_date_time(info$date))}, error = function(e) {row_df$date = format(Sys.time(),"%m/%d/%y")});

    #model-related fields
    row_df$fold = foldnum;

    #parameters
    row_df$parameter_1_name = "C_0";
    if (("smallest_C0_value_flag" %in% names(pydata$info)) && (pydata$info$smallest_C0_value_flag)){
        row_df$parameter_1_value = -1;
    } else if ("c0_value" %in% names(pydata$info)){
        row_df$parameter_1_value = pydata$info$c0_value;
    } else if ("C_0" %in% names(pydata$info)){
        row_df$parameter_1_value = pydata$info$C_0;
    }
    if (row_df$parameter_1_name <= 1e-6){
        row_df$parameter_1_name = -1
    }

    row_df$parameter_2_name = "max_L0";
    if ("max_L0_value" %in% names(pydata$info)){
        row_df$parameter_2_value = pydata$info$max_L0_value;
    } else {
        row_df$parameter_2_value = pydata$info$L0_max;
    }

    row_df$parameter_3_name = "max_coefficient";
    if ("max_coefficient" %in% names(pydata$info)){
        row_df$parameter_3_value = pydata$info$max_coefficient;
    } else {
        row_df$parameter_3_value = max(abs(data$hard_constraints$Lj_max[variable_ind,hid]), abs(data$hard_constraints$Lj_min[variable_ind,hid]))
    }

    row_df$parameter_4_name = "max_offset";
    if ("max_offset" %in% names(pydata$info)){
        row_df$parameter_4_value = pydata$info$max_offset;
    } else {
        row_df$parameter_4_value = max(abs(data$hard_constraints$Lj_max[intercept_ind,hid]), abs(data$hard_constraints$Lj_min[intercept_ind,hid]));
    }

    row_df$model_size = model_size
    row_df$runtime = pydata$info$total_run_time;
    row_df$total_data_time = pydata$info$total_data_time;
    row_df$total_solver_time = pydata$info$total_solver_time;
    row_df$nodes_processed = pydata$info$nodes_processed;
    row_df$upperbound = pydata$info$upperbound;
    row_df$lowerbound = pydata$info$lowerbound;
    row_df$relative_gap = pydata$info$relative_gap;
    row_df$id = id;
    row_df$print_model_id = R_model_id;
    row_df$debug_model_id = R_model_id;

    #append results to larger containers
    all_results_df = bind_rows(all_results_df, as.data.frame(row_df, stringsAsFactors = FALSE));
    all_debug_models[[R_model_id]] = list();
    all_print_models[[R_model_id]] = model;
}

#### save results in generic pipeline format for easy processing ####
outer_cv_fold_set = all_results_df %>%
    filter(inner_fold_id == "NONE") %>%
    mutate(n_folds_expected = as.numeric(substr(fold_id,2,3)) + 1) %>%
    group_by(data_name, method_name, w_pos, sample_weight_id, xtra_id, hcon_id, fold_id, inner_fold_id,
             parameter_1_name, parameter_1_value,
             parameter_2_name, parameter_2_value,
             parameter_3_name, parameter_3_value,
             parameter_4_name, parameter_4_value) %>%
    summarise(n_folds = n_distinct(fold),
              n_folds_expected = max(n_folds_expected),
              date = max(date)) %>%
    filter(n_folds == n_folds_expected) %>%
    ungroup() %>%
    select(-n_folds, -n_folds_expected);

inner_cv_fold_set = all_results_df %>%
    filter(inner_fold_id != "NONE") %>%
    mutate(n_folds_expected = as.numeric(substr(inner_fold_id, 5, 6)) + 1) %>%
    group_by(data_name, method_name, w_pos, sample_weight_id, xtra_id, hcon_id, fold_id, inner_fold_id,
             parameter_1_name, parameter_1_value,
             parameter_2_name, parameter_2_value,
             parameter_3_name, parameter_3_value,
             parameter_4_name, parameter_4_value) %>%
    summarise(n_folds = n_distinct(fold),
              n_folds_expected = max(n_folds_expected),
              date = max(date)) %>%
    filter(n_folds == n_folds_expected) %>%
    ungroup() %>%
    select(-n_folds, -n_folds_expected);

#make sure that if we have all inner_fold results for a given fold_id,
if (nrow(inner_cv_fold_set) > 0){
    validated_inner_folds = list()
    for (id in 1:nrow(inner_cv_fold_set)){
        inner_cv_info = inner_cv_fold_set %>% slice(id);
        outer_K = as.numeric(substr(inner_cv_info$fold_id, 2,3));
        inner_K = as.numeric(substr(inner_cv_info$inner_fold_id, 5, 6));
        expected_inner_fold_ids = sprintf('F%02dK%02d', seq(1:outer_K), inner_K);
        all_inner_folds_present = all(expected_inner_fold_ids %in% inner_cv_fold_set$inner_fold_id);
        outer_fold_present = nrow(outer_cv_fold_set %>% left_join(inner_cv_info %>% select(-inner_K))) > 0;
        if (outer_fold_present && all_inner_folds_present){
            validated_inner_folds[[id]] = inner_cv_info;
        }
    }
    inner_cv_fold_set = bind_rows(validated_inner_folds)
}
full_fold_set = bind_rows(inner_cv_fold_set, outer_cv_fold_set);
full_save_set = full_fold_set %>% select(-method_name, -date) %>% distinct()

#for each full fold set, create a new file
n_aggregated_files = nrow(full_save_set)
for (new_idx in 1:n_aggregated_files){

    results_df = full_save_set %>% slice(new_idx) %>% left_join(all_results_df);

    info = full_save_set %>% slice(new_idx) %>% as.list();
    info$comp_name = results_df %>% head(1) %>% select(comp_name) %>% unlist(use.names = FALSE)
    info$date = results_df %>% head(1) %>% select(date) %>% unlist(use.names = FALSE)

    all_methods_in_file = results_df %>% select(method_name) %>% distinct() %>% unlist(use.names = FALSE)

    output = list();
    for (method_name in all_methods_in_file){
        expected_number_of_folds = as.numeric(substr(info$fold_id,2,3)) + 1
        filter_string = sprintf("method_name == '%s'", method_name);
        method_results = results_df %>% filter_(filter_string);
        stopifnot(nrow(method_results) == expected_number_of_folds)

        output[[method_name]]$method_name = method_name;
        output[[method_name]]$method_settings = list();
        output[[method_name]]$results_df = method_results
        output[[method_name]]$total_runtime = method_results %>% tally(runtime) %>% unlist(use.names = FALSE);
        output[[method_name]]$print_models = all_print_models[names(all_print_models) %in% method_results$print_model_id]
        output[[method_name]]$debug_models = all_debug_models[names(all_debug_models) %in% method_results$debug_model_id]
    }

    aggregated_file_name = sprintf("%s_F_%s_I_%s_W_%s_L_%s_X_%s_pos_%1.9f_%s",
                                   info$data_name,
                                   info$fold_id,
                                   info$inner_fold_id,
                                   info$sample_weight_id,
                                   info$hcon_id,
                                   info$xtra_id,
                                   info$w_pos,
                                   aggregated_file_suffix);

    aggregated_file_name = safe.extension(paste0(train_dir, aggregated_file_name), extension = "RData");
    save(file=aggregated_file_name, "output", "info")
    print.to.console(sprintf("saved aggregate file: %s", aggregated_file_name))

}


##### quit #####
total_runtime = proc.time() - start_time;
print.to.console(sprintf("aggregation ran in %1.0f seconds", total_runtime[["elapsed"]]));

if (batch_mode){
    print.to.console("quitting R");
    quit(save = "no", status = 0);
}
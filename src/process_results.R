# Results Processing and Aggregration Script
# https://github.com/ustunb/classification-pipeline
# Berk Ustun | www.berkustun.com
#
#This script aggregates all results files for a given ${train_name} and
#then computes summary statistics. The processed results are saved as
#{home_dir}/results/${train_name}/${processed_file_name}
#
#Note: this file must be run from home_dir to work correctly

#### parse command line arguments and setup directories #####
cat(sprintf("running process_results.R in directory\n%s\n", getwd()))
source("setup_pipeline.R");
source(paste0(code_dir, "pipeline_helper_functions.R"));

#parse command line arguments
args = commandArgs(TRUE);
batch_mode = (length(args) >= 1);
if (batch_mode){
    train_name = args[1];
    files_to_process_regexp = args[2];
    processed_file_name = args[3];
    compute_all_metrics =  ifelse(length(args) > 3,
                                  yes = ifelse(args[4]=="TRUE", TRUE, FALSE),
                                  no = FALSE);
    log_name = ifelse(length(args) > 4, args[5], NA);
    run_dir = paste0(results_dir, train_name, "/");
} else {
    train_name = "regulated_F_K05N01";
    files_to_process_regexp = "*_results.RData";
    processed_file_name = paste0(train_name, "_processed.RData");
    compute_all_metrics = FALSE;
    log_name = NA;
    run_dir = paste0(results_dir, train_name, "/");
}
rm(args);

#show setup
select <- dplyr::select    #EDIT
log_file = set.log(log_dir = log_dir, log_name = log_name)
print.to.console("reading all files in: %s", run_dir);
print.to.console("saving aggregated results to: %s", processed_file_name);

#### constants ####
MIP_METRICS = c("total_data_time",
                "total_solver_time",
                "nodes_processed",
                "nodes_remaining",
                "upperbound",
                "lowerbound",
                "relative_gap");

#### aggregate results from all files ####
start_time = proc.time();

files_to_process_regexp = safe.extension(files_to_process_regexp, extension = "RData")
raw_results_files = dir(path=run_dir, pattern = files_to_process_regexp, ignore.case=TRUE)
print.to.console("script will process results from the following files in directory %s:", run_dir)
for (results_file in raw_results_files){print.to.console(sprintf("- %s", results_file))};
raw_results_files = paste0(run_dir, raw_results_files);


n_files = length(raw_results_files)
all_results = vector("list", n_files)
all_print_models = vector("list", n_files)
all_debug_models = vector("list", n_files)
all_print_model_cnt = 0;
all_debug_model_cnt = 0;

print.to.console("loading result files")
for (i in 1:n_files){

    print.to.console("loading file %d/%d:\t %s", i, n_files, basename(raw_results_files[i]))

    #load saved results
    raw_environment = new.env();
    load(raw_results_files[i], raw_environment);
    raw_info = raw_environment$info;
    raw_output = raw_environment$output;
    rm(raw_environment);

    #create count table
    data_name = raw_info$data_name;
    fold_id = raw_info$fold_id;
    hcon_id = raw_info$hcon_id;
    has_class_weights = ifelse("has_class_weights" %in% names(raw_info), raw_info$has_class_weights, raw_info$w_pos != 1.0);
    has_sample_weights = ifelse("has_sample_weights" %in% names(raw_info), raw_info$has_sample_weights, FALSE);
    raw_info$inner_fold_id = ifelse("inner_fold_id" %in% names(raw_info), raw_info$inner_fold_id, "NONE");
    raw_info$sample_weight_id = ifelse("sample_weight_id" %in% names(raw_info), raw_info$sample_weight_id, "NONE");

    #load data file
    data_file_name = paste0(data_dir, data_name, "_processed.RData");
    data = load.data.file.fast(data_file_name, fold_id, inner_fold_id = raw_info$inner_fold_id, sample_weight_id = raw_info$sample_weight_id);

    if (has_sample_weights){
        count_table = create.weighted.count.table(X = data$X,
                                                  Y = data$Y,
                                                  X_test = data$X_test,
                                                  Y_test = data$Y_test,
                                                  folds = data$folds,
                                                  sample_weights = data$sample_weights,
                                                  sample_weights_test = data$sample_weights_test);
    } else {
        count_table = create.count.table(X = data$X,
                                         Y = data$Y,
                                         X_test = data$X_test,
                                         Y_test = data$Y_test,
                                         folds = data$folds);
    }
    rm(data);

    #add method_name to results_df
    for (raw_method_name in names(raw_output)){
        raw_output[[raw_method_name]]$results_df = raw_output[[raw_method_name]]$results_df %>% mutate(method_name = raw_method_name);
    }

    #consolidate all of the raw output
    raw_output = merge.results.and.models(results_df_list = lapply(raw_output, function(r) r$results_df),
                                          print_models_list = lapply(raw_output, function(r) r$print_models),
                                          debug_models_list = lapply(raw_output, function(r) r$debug_models),
                                          print_model_cnt = all_print_model_cnt,
                                          debug_model_cnt = all_debug_model_cnt)

    #gather print_models and debug models
    all_print_models[[i]] = raw_output$print_models;
    all_debug_models[[i]] = raw_output$debug_models;
    all_print_model_cnt = raw_output$print_model_cnt;
    all_debug_model_cnt = raw_output$debug_model_cnt;

    #augment results_df with count data and other run-level information
    results_df = raw_output$results_df %>%
        select(-id) %>%
        mutate(data_name = raw_info$data_name[[1]],
               fold_id = raw_info$fold_id[[1]],
               xtra_id = raw_info$xtra_id[[1]],
               hcon_id = raw_info$hcon_id[[1]],
               inner_fold_id = raw_info$inner_fold_id[[1]],
               sample_weight_id = raw_info$sample_weight_id[[1]],
               comp_name = raw_info$comp_name[[1]],
               date = raw_info$date[[1]],
               w_pos = raw_info$w_pos[[1]],
               w_neg = 1.0,
               w_pos_norm = 2.0 * w_pos/(w_pos+w_neg),
               w_neg_norm = 2.0 * w_neg/(w_pos+w_neg))

    all_results[[i]] = left_join(x = results_df, y = count_table, by = "fold");
    rm(results_df, raw_output, raw_info);
}
print.to.console("finished loading");

print.to.console("attempting to aggregate files");
all_results = fix.parameters.in.stats.or.results.list(all_results)
results_df = bind_rows(all_results);

stopifnot(nrow(results_df) == all_debug_model_cnt, nrow(results_df) == all_print_model_cnt)
print_models = unlist(all_print_models, recursive =  FALSE);
debug_models = unlist(all_debug_models, recursive =  FALSE);
rm(all_results, all_print_models, all_debug_models);
print.to.console("finished aggregating");

#### compute score-based metrics ####
print.to.console("computing score based metrics");

model_helper = results_df %>%
    filter(method_name %in% METHODS_WITH_SCORE_FUNCTIONS) %>%
    mutate(discrete_flag = method_name %in% METHODS_WITH_DISCRETE_SCORE_FUNCTIONS,
           model_type = ifelse(method_name %in% METHODS_WITH_HISTOGRAM_MODELS, "histogram", "logit")) %>%
    select(data_name, sample_weight_id, fold_id, inner_fold_id, fold, method_name, discrete_flag, model_type, print_model_id) %>%
    ungroup()

loop_helper = model_helper %>%
    distinct(data_name, sample_weight_id, fold_id, inner_fold_id, fold, model_type) %>%
    arrange(data_name, sample_weight_id, fold_id, inner_fold_id, fold, model_type) %>%
    mutate(data_file_name = paste0(data_dir, data_name, "_processed.RData"))

n_loops = nrow(loop_helper);
all_train_score_metrics = vector("list", n_loops);
all_valid_score_metrics = vector("list", n_loops);
all_test_score_metrics = vector("list", n_loops);

print.to.console("will compute score based metrics for %d blocks", n_loops);
for (i in 1:n_loops){

    #loop settings
    loop_info = loop_helper %>% slice(i) %>% as.list(use.names = TRUE);
    loop_model_ids = suppressMessages(loop_helper %>% slice(i) %>% left_join(model_helper) %>% pull(print_model_id));
    loop_discrete_flags = suppressMessages(loop_helper %>% slice(i) %>% left_join(model_helper) %>% pull(discrete_flag));
    n_models = length(loop_model_ids);

    #message
    print.to.console("computing score metrics for %s | %s | %s | fold = %d | model_type = %s",
                     loop_info$data_name,
                     loop_info$fold_id,
                     loop_info$inner_fold_id,
                     loop_info$fold,
                     loop_info$model_type);

    #load data for each data_name, fold_id, inner_fold_id combination
    data = load.data.file.fast(data_file_name = loop_info$data_file_name,
                               fold_id = loop_info$fold_id,
                               inner_fold_id = loop_info$inner_fold_id,
                               sample_weight_id = loop_info$sample_weight_id,
                               add_intercept_column = TRUE);

    #data flags
    has_sample_weights = data$has_sample_weights;
    has_test_set = "X_test" %in% names(data) && nrow(data$X_test) > 0;
    has_validation_set = loop_info$fold > 0;

    #get data;
    X = data$X;
    Y = data$Y;

    if (has_test_set){
        X_test = data$X_test;
        Y_test = data$Y_test;
    }

    if (has_validation_set){
        valid_idx = data$folds == loop_info$fold;
        Y_valid = Y[valid_idx];
        Y = Y[!valid_idx];
    }

    #set function handle to get scores and probabilities
    if (loop_info$model_type == "logit"){
        get.scores = get.scores.logit
        get.probabilities = get.probabilities.logit
    } else {
        get.scores = get.scores.histogram
        get.probabilities = get.probabilities.histogram
    }

    #get score based metrics for general problems (no sample weights)
    if (!has_sample_weights){

        #test metrics subloop
        if (has_test_set){
            test_metrics = vector("list", n_models);
            for (m in 1:n_models){
                model = print_models[[loop_model_ids[m]]];
                scores = get.scores(X_test, model);
                probabilities = get.probabilities(scores, model);
                test_metrics[[m]] = compute.score.based.metrics(scores = scores,
                                                                probabilities = probabilities,
                                                                true_labels = Y_test,
                                                                discrete_flag = loop_discrete_flags[m]);
            }
            all_test_score_metrics[[i]] = bind_rows(test_metrics) %>% mutate(print_model_id = loop_model_ids);
        }

        #training / validation subloop
        if (has_validation_set){
            train_metrics = vector("list", n_models);
            valid_metrics = vector("list", n_models);
            for (m in 1:n_models){
                model = print_models[[loop_model_ids[m]]];
                scores = get.scores(X, model);
                probabilities = get.probabilities(scores, model);
                train_metrics[[m]] = compute.score.based.metrics(scores = scores[!valid_idx],
                                                                 probabilities = probabilities[!valid_idx],
                                                                 true_labels = Y,
                                                                 discrete_flag = loop_discrete_flags[m]);

                valid_metrics[[m]] = compute.score.based.metrics(scores = scores[valid_idx],
                                                                 probabilities = probabilities[valid_idx],
                                                                 true_labels = Y_valid,
                                                                 discrete_flag = loop_discrete_flags[m]);
            }
            all_train_score_metrics[[i]] = bind_rows(train_metrics) %>% mutate(print_model_id = loop_model_ids);
            all_valid_score_metrics[[i]] = bind_rows(valid_metrics) %>% mutate(print_model_id = loop_model_ids);
        }

        if (!has_validation_set){
            train_metrics = vector("list", n_models);
            for (m in 1:n_models){
                model = print_models[[loop_model_ids[m]]];
                scores = get.scores(X, model);
                probabilities = get.probabilities(scores, model);
                train_metrics[[m]] = compute.score.based.metrics(scores = scores,
                                                                 probabilities = probabilities,
                                                                 true_labels = Y,
                                                                 discrete_flag = loop_discrete_flags[m]);
            }
            all_train_score_metrics[[i]] = bind_rows(train_metrics) %>% mutate(print_model_id = loop_model_ids);
        }
    }

    #get score based metrics for problems with sample weights
    if (has_sample_weights){

        #test metrics subloop
        if (has_test_set){
            test_metrics = vector("list", n_models);
            for (m in 1:n_models){
                model = print_models[[loop_model_ids[m]]]
                scores = get.scores(X_test, model);
                probabilities = get.probabilities(scores, model);
                test_metrics[[m]] = compute.weighted.score.based.metrics(scores = scores,
                                                                         probabilities = probabilities,
                                                                         true_labels = Y_test,
                                                                         sample_weights = data$sample_weights_test,
                                                                         discrete_flag = loop_discrete_flags[m]);
            }
            all_test_score_metrics[[i]] = bind_rows(test_metrics) %>% mutate(print_model_id = loop_model_ids);
        }

        #training / validation subloop
        if (has_validation_set){
            train_metrics = vector("list", n_models);
            valid_metrics = vector("list", n_models);
            sample_weights = data$sample_weights[!valid_idx];
            sample_weights_valid = data$sample_weights[valid_idx];
            for (m in 1:n_models){
                model = print_models[[loop_model_ids[m]]]
                scores = get.scores(X, model);
                probabilities = get.probabilities(scores, model);
                train_metrics[[m]] = compute.weighted.score.based.metrics(scores = scores[!valid_idx],
                                                                          probabilities = probabilities[!valid_idx],
                                                                          true_labels = Y,
                                                                          sample_weights = sample_weights,
                                                                          discrete_flag = loop_discrete_flags[m]);

                valid_metrics[[m]] = compute.weighted.score.based.metrics(scores = scores[valid_idx],
                                                                          probabilities = probabilities[!valid_idx],
                                                                          true_labels = Y_valid,
                                                                          sample_weights = sample_weights_valid,
                                                                          discrete_flag = loop_discrete_flags[m]);
            }
            all_train_score_metrics[[i]] = bind_rows(train_metrics) %>% mutate(print_model_id = loop_model_ids);
            all_valid_score_metrics[[i]] = bind_rows(valid_metrics) %>% mutate(print_model_id = loop_model_ids);
        }

        if (!has_validation_set){
            train_metrics = vector("list", n_models);
            for (m in 1:n_models){
                model = print_models[[loop_model_ids[m]]]
                scores = get.scores(X, model);
                probabilities = get.probabilities(scores, model);
                train_metrics[[m]] = compute.weighted.score.based.metrics(scores = scores,
                                                                          probabilities = probabilities,
                                                                          true_labels = Y,
                                                                          sample_weights = sample_weights,
                                                                          discrete_flag = loop_discrete_flags[m]);
            }
            all_train_score_metrics[[i]] = bind_rows(train_metrics) %>% mutate(print_model_id = loop_model_ids);
        }
    }
}

#add score metrics back into main results
all_train_score_metrics = bind_rows(all_train_score_metrics);
colnames(all_train_score_metrics) = paste0("train_", colnames(all_train_score_metrics));
all_train_score_metrics = all_train_score_metrics %>% rename(print_model_id = train_print_model_id);
results_df = left_join(results_df, all_train_score_metrics, by = c("print_model_id"))

all_test_score_metrics = bind_rows(all_test_score_metrics);
if (nrow(all_test_score_metrics) > 0){
    colnames(all_test_score_metrics) = paste0("test_", colnames(all_test_score_metrics));
    all_test_score_metrics = all_test_score_metrics %>% rename(print_model_id = test_print_model_id);
    results_df = left_join(results_df, all_test_score_metrics, by = c("print_model_id"))
}

all_valid_score_metrics = bind_rows(all_valid_score_metrics);
colnames(all_valid_score_metrics) = paste0("valid_", colnames(all_valid_score_metrics));
all_valid_score_metrics = all_valid_score_metrics %>% rename(print_model_id = valid_print_model_id);
results_df = left_join(results_df, all_valid_score_metrics, by = c("print_model_id"));
rm(all_test_score_metrics, all_train_score_metrics, all_valid_score_metrics);

#### compute accuracy metrics ####
if (compute_all_metrics){
    print.to.console("computing full set of accuracy metrics");
    results_df = results_df %>%
        mutate(
            #
            # ACCURACY METRICS
            #
            #error
            #weighted error
            #true positive rate
            #true negative rate
            #false positive rate
            #false negative rate
            #positive likelihood ratio (TPR/FPR)
            #negative likelihood ratio (FNR/TNR)
            #diagnostic odds ratio (LRPos/LRNeg)
            #predicted positive rate (= (TP+FP)/N)
            #predicted negative rate (= (TN+FN)/N)
            #trivial model flag  (= predicted positive rate == 1 or predicted negative rate == 1)
            #positive predictive value (TP/ Predicted Positive
            #negative predictive value (FP/Predicted Negative)
            #false discovery rate (= FP/Predicted Positive)
            #false omission rate (= FN/Predicted Negative)
            #
            #### TRAINING
            #
            train_error = (train_false_negatives + train_false_positives)/N_train,
            train_weighted_error = (w_pos_norm*train_false_negatives + w_neg_norm*train_false_positives)/N_train,
            train_tpr = train_true_positives/N_train_pos,
            train_fpr = train_false_positives/N_train_neg,
            train_fnr = 1 - train_tpr,
            train_tnr = 1 - train_fpr,
            train_lrp = train_tpr/train_fpr,
            train_lrn = train_fnr/train_tnr,
            train_dor = train_lrp/train_lrn,
            train_ppr = (train_true_positives + train_false_positives) / N_train,
            train_npr = (train_true_negatives + train_false_negatives) / N_train,
            train_trivial_flag = ifelse((train_ppr==1.0)|(train_npr==1.0), 1, 0),
            train_ppv = train_true_positives/(train_true_positives + train_false_positives),
            train_npv = train_false_positives/(train_true_negatives + train_false_negatives),
            train_fdr = train_false_positives/(train_true_positives + train_false_positives),
            train_for = train_false_negatives/(train_true_negatives + train_false_negatives),
            #
            #### TESTING
            #
            test_error = ifelse(N_test==0, NA, (test_false_negatives + test_false_positives)/N_test),
            test_weighted_error = ifelse(N_test==0, NA, (w_pos_norm*test_false_negatives + w_neg_norm*test_false_positives)/N_test),
            test_tpr = ifelse(N_test==0, NA, test_true_positives/N_test_pos),
            test_fpr = ifelse(N_test==0, NA, test_false_positives/N_test_neg),
            test_fnr = ifelse(N_test==0, NA, 1 - test_tpr),
            test_tnr = ifelse(N_test==0, NA, 1 - test_fpr),
            test_lrp = ifelse(N_test==0, NA, test_tpr/test_fpr),
            test_lrn = ifelse(N_test==0, NA, test_fnr/test_tnr),
            test_dor = ifelse(N_test==0, NA, test_lrp/test_lrn),
            test_ppr = ifelse(N_test==0, NA, (test_true_positives + test_false_positives) / N_test),
            test_npr = ifelse(N_test==0, NA, (test_true_negatives + test_false_negatives) / N_test),
            test_trivial_flag = ifelse((test_ppr==1.0)|(test_npr==1.0), 1, 0),
            test_ppv = ifelse(N_test==0, NA, test_true_positives/(test_true_positives + test_false_positives)),
            test_npv = ifelse(N_test==0, NA, test_false_positives/(test_true_negatives + test_false_negatives)),
            test_fdr = ifelse(N_test==0, NA, test_false_positives/(test_true_positives + test_false_positives)),
            test_for = ifelse(N_test==0, NA, test_false_negatives/(test_true_negatives + test_false_negatives)),
            #
            #### VALIDATION
            #
            valid_error = ifelse(N_valid==0, NA, (valid_false_negatives + valid_false_positives)/N_valid),
            valid_weighted_error = ifelse(N_valid==0, NA, (w_pos_norm*valid_false_negatives + w_neg_norm*valid_false_positives)/N_valid),
            valid_tpr = ifelse(N_valid==0, NA, valid_true_positives/N_valid_pos),
            valid_fpr = ifelse(N_valid==0, NA, valid_false_positives/N_valid_neg),
            valid_fnr = ifelse(N_valid==0, NA, 1 - valid_tpr),
            valid_tnr = ifelse(N_valid==0, NA, 1 - valid_fpr),
            valid_lrp = ifelse(N_valid==0, NA, valid_tpr/valid_fpr),
            valid_lrn = ifelse(N_valid==0, NA, valid_fnr/valid_tnr),
            valid_dor = ifelse(N_valid==0, NA, valid_lrp/valid_lrn),
            valid_ppr = ifelse(N_valid==0, NA, (valid_true_positives + valid_false_positives) / N_valid),
            valid_npr = ifelse(N_valid==0, NA, (valid_true_negatives + valid_false_negatives) / N_valid),
            valid_trivial_flag = ifelse((valid_ppr==1.0)|(valid_npr==1.0), 1, 0),
            valid_ppv = ifelse(N_valid==0, NA, valid_true_positives/(valid_true_positives + valid_false_positives)),
            valid_npv = ifelse(N_valid==0, NA, valid_false_positives/(valid_true_negatives + valid_false_negatives)),
            valid_fdr = ifelse(N_valid==0, NA, valid_false_positives/(valid_true_positives + valid_false_positives)),
            valid_for = ifelse(N_valid==0, NA, valid_false_negatives/(valid_true_negatives + valid_false_negatives))
        )
} else {
    print.to.console("computing basic accuracy metrics");
    results_df = results_df %>%
        mutate(
            #
            # ACCURACY METRICS
            #
            #error
            #weighted error
            #true positive rate
            #false positive rate
            #trivial model flag  (= predicted positive rate == 1 or predicted negative rate == 1)
            #
            #### TRAINING
            train_error = (train_false_negatives + train_false_positives)/N_train,
            train_weighted_error = (w_pos_norm*train_false_negatives + w_neg_norm*train_false_positives)/N_train,
            train_tpr = train_true_positives/N_train_pos,
            train_fpr = train_false_positives/N_train_neg,
            train_trivial_flag = ifelse((train_tpr==1.0 && train_fpr==1.0) || (train_tpr==0.0 && train_fpr==0.0), 1, 0),
            #
            #### TESTING
            test_error = ifelse(N_test==0, NA, (test_false_negatives + test_false_positives)/N_test),
            test_weighted_error = ifelse(N_test==0, NA, (w_pos_norm*test_false_negatives + w_neg_norm*test_false_positives)/N_test),
            test_tpr = ifelse(N_test==0, NA, test_true_positives/N_test_pos),
            test_fpr = ifelse(N_test==0, NA, test_false_positives/N_test_neg),
            test_trivial_flag = ifelse((test_tpr==1.0 && test_fpr==1.0) || (test_tpr==0.0 && test_fpr==0.0), 1, 0),
            #
            #### VALIDATION
            valid_error = ifelse(N_valid==0, NA, (valid_false_negatives + valid_false_positives)/N_valid),
            valid_weighted_error = ifelse(N_valid==0, NA, (w_pos_norm*valid_false_negatives + w_neg_norm*valid_false_positives)/N_valid),
            valid_tpr = ifelse(N_valid==0, NA, valid_true_positives/N_valid_pos),
            valid_fpr = ifelse(N_valid==0, NA, valid_false_positives/N_valid_neg),
            valid_trivial_flag = ifelse((valid_tpr==1.0 && valid_fpr==1.0) || (valid_tpr==0.0 && valid_fpr==0.0), 1, 0)
        )
}
print.to.console("accuracy metric computation complete")

#include optional missing metrics
for (metric in MIP_METRICS){
    if (metric %nin% colnames(results_df)){
        print.to.console("adding column for %s", metric)
        results_df[[metric]] = NA;
    }
}

print.to.console("computing summary stats (warning: this takes a while)")
stats_df = create.stats.from.fold.results(results_df)
print.to.console("summary stats computation complete!")

#### save and exit #####
output_file = paste0(run_dir, processed_file_name);

print.to.console("saving results in file %s", output_file);
save(stats_df, results_df, print_models, debug_models, file = output_file);
print.to.console("saved results in file %s", output_file);

total_runtime = proc.time() - start_time;
print.to.console("processing script ran in %1.0f seconds", total_runtime[["elapsed"]]);

#exit
if (batch_mode){
    print.to.console("quitting R");
    quit(save = "no", status = 0);
}
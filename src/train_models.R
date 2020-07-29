# Model Training Script
# https://github.com/ustunb/classification-pipeline
# Berk Ustun | www.berkustun.com

##### Setup Directories and Parse Command Line Arguments #####
cat(sprintf("running train_models.R in directory\n%s\n", getwd()))
source("setup_pipeline.R");

args = commandArgs(TRUE);
batch_mode = length(args) > 0
if (batch_mode){
    train_name = args[1];
    run_name = args[2];
    results_file_name = ifelse(length(args)>=3, args[3], paste0(run_name, "_results.RData"))
    log_name = ifelse(length(args)>=4, args[4], NA)
} else {
    train_name = "regulated"
    run_name = "regulated_F_K05N01_pos_1.00000000"
    results_file_name = paste0(run_name, "_results.RData")
    log_name = NA;
    setwd("/Users/berk/Dropbox (Harvard University)/Research/SLIM/classification-pipeline/");
}
rm(args);

log_file = set.log(log_dir = log_dir, log_name = log_name);
print_flag = .PIPELINE_DEFAULTS$print_flag

#setup run related files
run_dir = paste0(results_dir, train_name, "/");
dir.create(run_dir, showWarnings =  FALSE, recursive = FALSE);
results_file_name = paste0(run_dir, basename(results_file_name))

#print key variables
print.to.console(sprintf("comp_name: %s", comp_name));
print.to.console(sprintf("current working directory: %s", paste0(getwd(),"/")));
print.to.console(sprintf("run directory: 'run_dir' = %s", run_dir));
print.to.console(sprintf("run identifier: 'run_name' = %s", run_name));
print.to.console(sprintf("results file: %s", results_file_name));
print.to.console(sprintf("log file: %s", log_file));

###### LOAD SETTINGS ######
#read run settings from disk
#each setting variable is read from a text file in 'run_dir'
#text file for setting variable 'example_setting' is named:
#paste0(run_name, example_setting, ".setting")
#if this text file is not found, then we set the setting variable to a default value

data_name 				            = load.or.set.to.default("data_name", "seizure2");
data_extension                      = load.or.set.to.default("data_extension", "processed")
fold_id 				            = load.or.set.to.default("fold_id", "K05N01");
inner_fold_id 			            = load.or.set.to.default("inner_fold_id", "NONE");
xtra_id 				            = load.or.set.to.default("xtra_id","NA");
w_pos                               = load.or.set.to.default("w_pos", 1.0);
sample_weight_id                    = load.or.set.to.default("sample_weight_id", "NONE");
standardize_flag                    = load.or.set.to.default("standardize_flag", FALSE);
check_results_flag 					= load.or.set.to.default("check_results_flag", TRUE);

#hard constraints
hcon_id 				            = load.or.set.to.default("hcon_id","U001");
use_custom_coefficient_set          = load.or.set.to.default("use_custom_coefficient_set", TRUE);
max_coefficient                     = load.or.set.to.default("max_coefficient", 10);
max_offset                          = load.or.set.to.default("max_offset", -1);
max_L0_value                        = load.or.set.to.default("max_L0_value", -1);

#choice of methods
settings                            = list();
settings$run_cart                   = load.or.set.to.default("run_cart", FALSE);
settings$run_c50_rule               = load.or.set.to.default("run_c50_rule", FALSE);
settings$run_c50_tree               = load.or.set.to.default("run_c50_tree", FALSE);
settings$run_one_rule               = load.or.set.to.default("run_one_rule", FALSE);
settings$run_ripper                 = load.or.set.to.default("run_ripper", FALSE);
settings$run_part                   = load.or.set.to.default("run_part", FALSE);
settings$run_lars_lasso             = load.or.set.to.default("run_lars_lasso", FALSE);
settings$run_lars_ridge             = load.or.set.to.default("run_lars_ridge", FALSE);
settings$run_lars_elasticnet        = load.or.set.to.default("run_lars_elasticnet", TRUE);
settings$run_rounded_elasticnet     = load.or.set.to.default("run_rounded_elasticnet", FALSE);
settings$run_randomforest           = load.or.set.to.default("run_randomforest", FALSE);
settings$run_sgb                    = load.or.set.to.default("run_sgb", FALSE);
settings$run_svm_linear             = load.or.set.to.default("run_svm_linear", FALSE);
settings$run_svm_rbf                = load.or.set.to.default("run_svm_rbf", FALSE);
settings$run_ahrs                   = load.or.set.to.default("run_ahrs", FALSE);

#method-specific settings
settings$cart$param$minsplit                = load.or.set.to.default("cart_minsplit", c(3,5,10,15,20));
settings$cart$param$minbucket               = load.or.set.to.default("cart_minbucket", c(3,5));
settings$cart$param$cp                      = load.or.set.to.default("cart_cp", c(0.0001, 0.001, 0.01));
settings$cart$save_print_models             = load.or.set.to.default("cart_save_print_models", TRUE);
settings$cart$save_debug_models             = load.or.set.to.default("cart_save_debug_models", FALSE);

settings$c50_tree$param$CF                  = load.or.set.to.default("c50_tree_confidence_factor", c(0.25));
settings$c50_tree$param$minCases            = load.or.set.to.default("c50_tree_min_cases", c(1, 5, 10));
settings$c50_tree$param$noGlobalPruning     = load.or.set.to.default("c50_tree_no_global_pruning", TRUE);
settings$c50_tree$save_print_models         = load.or.set.to.default("c50_tree_save_print_models", TRUE);
settings$c50_tree$save_debug_models         = load.or.set.to.default("c50_tree_save_debug_models", FALSE);

settings$c50_rule$param$CF                  = load.or.set.to.default("c50_rule_confidence_factor", c(0.25));
settings$c50_rule$param$minCases            = load.or.set.to.default("c50_rule_min_cases", c(1, 5, 10));
settings$c50_rule$param$noGlobalPruning     = load.or.set.to.default("c50_rule_no_global_pruning", TRUE);
settings$c50_rule$save_print_models         = load.or.set.to.default("c50_rule_save_print_models", TRUE);
settings$c50_rule$save_debug_models         = load.or.set.to.default("c50_rule_save_debug_models", FALSE);

settings$one_rule$param$binning             = load.or.set.to.default("one_rule_binning", c("logreg", "naive", "infogain"));
settings$one_rule$param$omit_na             = load.or.set.to.default("one_rule_omit_na", c(TRUE, FALSE));
settings$one_rule$param$ties_method         = load.or.set.to.default("one_rule_ties_method", c("first", "chisq"));
settings$one_rule$save_print_models         = load.or.set.to.default("one_rule_save_print_models", TRUE);
settings$one_rule$save_debug_models         = load.or.set.to.default("one_rule_save_debug_models", FALSE);

settings$ripper$param$N                     = load.or.set.to.default("ripper_split_weights", c(2.0)) #Set the minimal weights of instances within a split.  (default 2.0) -N
settings$ripper$param$O                     = load.or.set.to.default("ripper_opt_runs", 2) #Set the number of runs of optimizations. (Default: 2)
settings$ripper$param$P                     = load.or.set.to.default("ripper_pruning", c(TRUE, FALSE)) #Set number of folds for REP One fold is used as pruning set. -P
settings$ripper$param$F                     = load.or.set.to.default("ripper_folds_for_pruning", 3) #Set number of folds for REP One fold is used as pruning set. -F
settings$ripper$save_print_models           = load.or.set.to.default("ripper_save_print_models", TRUE);
settings$ripper$save_debug_models           = load.or.set.to.default("ripper_save_debug_models", FALSE);

settings$part$param$C                       = load.or.set.to.default("part_pruning_confidence", c(0.12, 0.25))  #Set confidence threshold for pruning.  (default 0.25)
settings$part$param$O                       = load.or.set.to.default("part_opt_runs", 2) #Minimum objects per leaf
settings$part$param$U                       = !load.or.set.to.default("part_pruning", c(TRUE, FALSE)) #generate pruned decision list.
settings$part$param$F                       = load.or.set.to.default("part_folds_for_pruning", 3)
settings$part$save_print_models             = load.or.set.to.default("part_save_print_models", TRUE);
settings$part$save_debug_models             = load.or.set.to.default("part_save_debug_models", FALSE);

settings$lars_lasso$param$nlambda           = load.or.set.to.default("lars_lasso_nlambda", 1000);
settings$lars_lasso$param$standardize       = load.or.set.to.default("lars_lasso_standardize", standardize_flag);
settings$lars_lasso$save_print_models       = load.or.set.to.default("lars_lasso_save_print_models", TRUE);
settings$lars_lasso$save_debug_models       = load.or.set.to.default("lars_lasso_save_debug_models", FALSE);

settings$lars_ridge$param$nlambda           = load.or.set.to.default("lars_ridge_nlambda", 500);
settings$lars_ridge$param$standardize       = load.or.set.to.default("lars_ridge_standardize", standardize_flag);
settings$lars_ridge$save_print_models       = load.or.set.to.default("lars_ridge_save_print_models", TRUE);
settings$lars_ridge$save_debug_models       = load.or.set.to.default("lars_ridge_save_debug_models", FALSE);

settings$lars_elasticnet$alpha_values       = load.or.set.to.default("lars_elasticnet_alpha_values", seq(0,5)/5);
settings$lars_elasticnet$param$nlambda      = load.or.set.to.default("lars_elasticnet_nlambda", 100);
settings$lars_elasticnet$param$standardize  = load.or.set.to.default("lars_elasticnet_standardize", standardize_flag);
settings$lars_elasticnet$save_print_models  = load.or.set.to.default("lars_elasticnet_save_print_models", TRUE);
settings$lars_elasticnet$save_debug_models  = load.or.set.to.default("lars_elasticnet_save_debug_models", FALSE);

settings$rounded_elasticnet$alpha_values       = load.or.set.to.default("rounded_elasticnet_alpha_values", seq(0,10)/10);
settings$rounded_elasticnet$param$nlambda      = load.or.set.to.default("rounded_elasticnet_nlambda", 100);
settings$rounded_elasticnet$param$standardize  = load.or.set.to.default("rounded_elasticnet_standardize", standardize_flag);
settings$rounded_elasticnet$save_print_models  = load.or.set.to.default("rounded_elasticnet_save_print_models", TRUE);
settings$rounded_elasticnet$save_debug_models  = load.or.set.to.default("rounded_elasticnet_save_debug_models", FALSE);

settings$randomforest$param$sampsize        = load.or.set.to.default("randomforest_sampprop", c(0.632, 0.4, 0.2));
settings$randomforest$param$nodesize        = load.or.set.to.default("randomforest_nodesize", c(1,5,10,20));
settings$randomforest$save_print_models     = load.or.set.to.default("randomforest_save_print_models", FALSE);
settings$randomforest$save_debug_models     = load.or.set.to.default("randomforest_save_debug_models", FALSE);

settings$svm_linear$param$cost              = load.or.set.to.default("svm_linear_costs", 10^seq(-3,3,by=0.5));
settings$svm_linear$param$scale             = load.or.set.to.default("svm_linear_scale", standardize_flag);
settings$svm_linear$save_print_models       = load.or.set.to.default("svm_linear_save_print_models", TRUE);
settings$svm_linear$save_debug_models       = load.or.set.to.default("svm_linear_save_debug_models", FALSE);

settings$svm_rbf$param$cost                 = load.or.set.to.default("svm_rbf_costs", 10^seq(-3,3,by=0.5));
settings$svm_rbf$param$scale                = load.or.set.to.default("svm_rbf_scale", standardize_flag);
settings$svm_rbf$save_print_models          = load.or.set.to.default("svm_rbf_save_print_models", FALSE);
settings$svm_rbf$save_debug_models          = load.or.set.to.default("svm_rbf_save_debug_models", FALSE);

settings$sgb$param$interaction.depth        = load.or.set.to.default("sgb_interaction_depth", c(2));
settings$sgb$param$shrinkage                = load.or.set.to.default("sgb_shrinkage", c(0.001,0.01,0.1));
settings$sgb$param$n.trees                  = load.or.set.to.default("sgb_n_trees", c(100, 500, 1500, 3000));
settings$sgb$save_print_models              = load.or.set.to.default("sgb_save_print_models", FALSE);
settings$sgb$save_debug_models              = load.or.set.to.default("sgb_save_debug_models", FALSE);

settings$ahrs$save_print_models                 = load.or.set.to.default("ahrs_save_print_models", TRUE);
settings$ahrs$save_debug_models                 = load.or.set.to.default("ahrs_save_debug_models", FALSE);
settings$ahrs$rounding_type                     = load.or.set.to.default("ahrs_rounding_type", c("none"))
#settings$ahrs$rounding_type                     = load.or.set.to.default("ahrs_rounding_type", c("none", "unit", "capped","scaled"))
#settings$ahrs$feature_selection_type            = load.or.set.to.default("ahrs_feature_selection_type", c("forward_stepwise", "backward_stepwise", "elastic_net"))
settings$ahrs$feature_selection_type            = load.or.set.to.default("ahrs_feature_selection_type", c("forward_stepwise"))
#settings$ahrs$risk_model_type                   = load.or.set.to.default("ahrs_risk_model_type", c("logit","histogram"))
settings$ahrs$risk_model_type                   = load.or.set.to.default("ahrs_risk_model_type", c("logit"))
settings$ahrs$histogram_estimation_pct          = load.or.set.to.default("ahrs_histogram_estimation_pct", 0.0);
settings$ahrs$histogram_estimation_maxbins      = load.or.set.to.default("ahrs_histogram_estimation_maxbins", 10);
settings$ahrs$stepwise_trace_flag               = load.or.set.to.default("ahrs_stepwise_trace_flag", FALSE);
settings$ahrs$elastic_net_nlambda               = load.or.set.to.default("ahrs_elastic_net_nlambda", 100);
settings$ahrs$elastic_net_alpha_values          = load.or.set.to.default("ahrs_elastic_net_alpha_values", seq(0,10)/10);
settings$ahrs$elastic_net_set_size_limit        = load.or.set.to.default("ahrs_elastic_net_set_size_limit", FALSE);
settings$ahrs$elastic_net_set_coef_limit        = load.or.set.to.default("ahrs_elastic_net_set_coef_limit", FALSE);

###### PIPELINE VARIABLES ######

#load helper functions
source(paste0(code_dir, "training_helper_functions.R"));
source(paste0(code_dir, "pipeline_helper_functions.R"));

#load data
data_file_name = paste0(data_dir, data_name, "_", data_extension, ".RData");
data = load.data.file(data_file_name, fold_id, inner_fold_id, sample_weight_id);
list2env(x = data, envir = globalenv());
rm(data);

#load coefficients
load_coefficient_set = any(settings$run_lars_lasso,
                           settings$run_lars_ridge,
                           settings$run_lars_elasticnet,
                           settings$run_rounded_elasticnet,
                           settings$run_ahrs)
if (load_coefficient_set){
    coefficient_set = load.coefficient.set(data_file_name,
                                           hcon_id = hcon_id,
                                           use_custom_coefficient_set = use_custom_coefficient_set,
                                           max_offset = max_offset,
                                           max_coefficient = max_coefficient,
                                           max_L0_value = max_L0_value);

}

#setup weights
w_neg = 1.0;
class_weights = c(w_neg, w_pos);
class_weights = 2 * class_weights / sum(class_weights);
has_class_weights = !(is.null(class_weights) || class_weights[1]==class_weights[2]);
print.to.console(ifelse(has_class_weights, "found custom class_weights", "did not find custom class_weights"));
print.to.console(sprintf("w- = %1.3f w+ = %1.3f", class_weights[1], class_weights[2]));
print.to.console(ifelse(has_sample_weights, "found custom sample_weights", "did not find custom sample_weights"));

#setup count table to check results tables
results.are.ok = function(r) return(TRUE);
if (check_results_flag){
    if (has_sample_weights) {
        count_table = create.weighted.count.table(X = X,
                                                  Y = Y,
                                                  X_test = X_test,
                                                  Y_test = Y_test,
                                                  folds = folds,
                                                  sample_weights = sample_weights,
                                                  sample_weights_test = sample_weights_test);
    } else {
        count_table = create.count.table(X = X,
                                         Y = Y,
                                         X_test = X_test,
                                         Y_test = Y_test,
                                         folds = folds);
    }
    results.are.ok = function(r) return(check.results.rep(r, count_table));
}

##### MODEL TRAINING #####

output = list();
start_time = proc.time();
set.seed(1337);
random_seed = .Random.seed;

if (settings$run_ahrs){
    all_results = train.ahrs(settings[["ahrs"]], check_ahrs_flag = TRUE);
    for (method_name in names(all_results)){
        if (results.are.ok(all_results[[method_name]])){
            output[[method_name]] = all_results[[method_name]]
        } else {
            print.to.console(sprintf("%s results failed check", method_name))
        }
    }
}

if (settings$run_cart){
    method_name = "cart";
    results = train.cart(method_settings = settings[[method_name]])
    if (results.are.ok(results)){
        output[[method_name]] =  results;
    }
}

if (settings$run_c50_rule){
    method_name = "c50_rule";
    results = train.c50(method_settings = settings[[method_name]], type = "rule")
    if (results.are.ok(results)){
        output[[method_name]] =  results;
    }
}

if (settings$run_c50_tree){
    method_name = "c50_tree";
    results = train.c50(method_settings = settings[[method_name]], type = "tree")
    if (results.are.ok(results)){
        output[[method_name]] =  results;
    }
}

if (settings$run_one_rule){
    method_name = "one_rule";
    results = train.one_rule(method_settings = settings[[method_name]])
    if (results.are.ok(results)){
        output[[method_name]] = results;
    }
}

if (settings$run_ripper){
    method_name = "ripper";
    results = train.weka(method_settings = settings[[method_name]], method_name)
    if (results.are.ok(results)){
        output[[method_name]] =  results;
    }
}

if (settings$run_part){
    method_name = "part";
    results = train.weka(method_settings = settings[[method_name]], method_name)
    if (results.are.ok(results)){
        output[[method_name]] =  results;
    }
}

if (settings$run_lars_lasso){
    method_name = "lars_lasso";
    results = train.lars(method_settings = settings[[method_name]], alpha_value = 1.0);
    if (results.are.ok(results)){
        output[[method_name]] =  results;
    }
}

if (settings$run_lars_ridge){
    method_name = "lars_ridge";
    results = train.lars(method_settings = settings[[method_name]], alpha_value = 0.0);
    if (results.are.ok(results)){
        output[[method_name]] =  results;
    }
}

if (settings$run_lars_elasticnet){
    method_name = "lars_elasticnet";
    results = train.lars.elasticnet(method_settings = settings[[method_name]], rounding_type = "none");
    if (results.are.ok(results)){
        output[[method_name]] =  results;
    }
}

if (settings$run_rounded_elasticnet){
    method_name = "rounded_elasticnet";
    results = train.lars.elasticnet(method_settings = settings[[method_name]], rounding_type = "scaled");
    if (results.are.ok(results)){
        output[[method_name]] =  results;
    }
}

if (settings$run_randomforest){
    method_name = "randomforest";
    results = train.randomforest(method_settings = settings[[method_name]]);
    if (results.are.ok(results)){
        output[[method_name]] =  results;
    }
}

if (settings$run_sgb){
    method_name = "sgb";
    results = train.sgb(method_settings = settings[[method_name]]);
    if (results.are.ok(results)){
        output[[method_name]] =  results;
    }
}

if (settings$run_svm_linear){
    method_name = "svm_linear"
    results = train.svm(method_settings = settings[[method_name]], kernel_type = "linear")
    if (results.are.ok(results)){
        output[[method_name]] =  results;
    }
}

if (settings$run_svm_rbf){
    method_name = "svm_rbf"
    results = train.svm(method_settings = settings[[method_name]], kernel_type = "radial")
    if (results.are.ok(results)){
        output[[method_name]] =  results;
    }
}

###### SAVE RESULTS ######

info = list(
    #
    #training info
    #
    data_name = data_name,
    w_pos = w_pos,
    fold_id = fold_id,
    inner_fold_id = inner_fold_id,
    sample_weight_id = sample_weight_id,
    has_sample_weights = has_sample_weights,
    has_class_weights = has_class_weights,
    hcon_id = hcon_id,
    xtra_id = xtra_id,
    #
    #training info
    #
    comp_name = comp_name,
    date = format(Sys.time(),"%m/%d/%y"),
    end_time = format(Sys.time()),
    data_file_name = data_file_name,
    results_file_name = results_file_name,
    run_name = run_name,
    run_dir = run_dir,
    sys_info = Sys.info(),
    random_seed = random_seed,
    settings = settings
)

#save data in R format
save(file=results_file_name, "output", "info")
print.to.console(sprintf("saved results in file %s", results_file_name))

#### exit #####
if (batch_mode){
    print.to.console("quitting R")
    quit(save = "no", status = 0)
}
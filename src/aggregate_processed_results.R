#Processed Results Aggregation Script
#https://github.com/ustunb/classification-pipeline
#Berk Ustun | www.berkustun.com
#
#This script will aggregate all processed results files produced by ProcessedResults.R
#into a single RData file in the directory:${home_dir}/Run/${train_name}/${save_name}

##### parse command line arguments and setup directories #####
cat(sprintf("running aggregate_processed_results.R in directory\n%s\n", getwd()))
source("setup_pipeline.R");
source(paste0(code_dir, "pipeline_helper_functions.R"));

args = commandArgs(TRUE);
batch_mode = length(args) >= 3;
if (batch_mode){
    train_name = args[1];
    files_to_process_regexp = args[2];
    processed_file_name = args[3];
    remove_files_after = args[4];
    log_name = ifelse(length(args) > 4, args[5], NA);
} else {
    train_name = "seizure_F_K05N01"
    files_to_process_regexp = sprintf("%s_L_U[[:digit:]]{3}_processed.RData", train_name);
    processed_file_name = paste0(train_name, "_processed.RData");
    remove_files_after = FALSE;
    log_name = NA;
}
rm(args);

#show setup
run_dir = paste0(results_dir, train_name, "/");
log_file = set.log(log_dir = log_dir, log_name = log_name)
print.to.console(sprintf("comp_name: %s", comp_name));
print.to.console(sprintf("log file: %s", log_file));
print.to.console(sprintf("reading all files in: %s", run_dir));
print.to.console(sprintf("saving aggregated results to: %s", processed_file_name));

##### match files #####
start_time = proc.time();

files_to_process_regexp = safe.extension(files_to_process_regexp, extension = "RData")
processed_results_files = dir(path=run_dir, files_to_process_regexp, ignore.case=TRUE)
processed_results_files = paste0(run_dir, processed_results_files);
print.to.console(sprintf("script will aggregate and process the following files in directory %s:", run_dir))
for (results_file in processed_results_files){print.to.console(sprintf("- %s", basename(results_file)))};

##### combine files #####
list2env(x = combine.processed.results(processed_results_files), envir = .GlobalEnv);

##### save and exit #####
save_file = paste0(run_dir, processed_file_name);
save_time = Sys.time();
save(stats_df, results_df, print_models, debug_models, file = save_file);

# delete component files
if (file.exists(save_file)){
    print.to.console(sprintf("saved results in file %s", save_file));
    if (remove_files_after){
        file_saved_after_start_time = as.double(difftime(time1 = file.info(save_file)$ctime, time2 = save_time)) > -1.0;
        if (file_saved_after_start_time){
            print.to.console(sprintf("deleting component files"))
            for (results_file in processed_results_files){
                print.to.console(sprintf(" - deleting %s", basename(results_file)))
                file.remove(results_file);
            }
        }
    }
}

# quit
total_runtime = proc.time() - start_time;
print.to.console(sprintf("script ran in %1.0f seconds", total_runtime[["elapsed"]]));

if (batch_mode){
    print.to.console("quitting R");
    quit(save = "no", status = 0);
}
#Startup Script
#https://github.com/ustunb/classification-pipeline
#Berk Ustun | www.berkustun.com
#
#This script contains variables and helper functions to setup directories,
#libraries, and logging. It should be called as source(setup_pipeline.R)
#
#The pipeline depends on "environmental" variables stored in .PIPELINE_DEFAULTS.
#These can be set beforehand via a list named .PIPELINE_DEFAULTS, or
#changed permanently by editting get.pipeline.defaults()

#### INITIAL FUNCTIONS ####
safe.dir = function(dir_name){
    last_char = substr(dir_name,nchar(dir_name),nchar(dir_name));
    if (last_char != "/") {
        dir_name = paste0(dir_name,"/");
    }
    return(dir_name);
}

get.pipeline.defaults = function(load_defaults_from_disk = TRUE){

    ds = list();

    file_location = tryCatch(paste0(normalizePath(dirname(sys.frame(1)$ofile)),"/"), error = function(e) {getwd()})
    defaults_file = paste0(file_location, ".PIPELINE_DEFAULTS");

    if (file.exists(defaults_file) && load_defaults_from_disk){
        defaults = read.csv(defaults_file, header = FALSE, stringsAsFactors = FALSE);
        for (n in 1:nrow(defaults)){
            name = defaults[n,1];
            val = defaults[n,2];
            val = ifelse(val %in% c("TRUE", "FALSE"), as.logical(val), val);
            val = ifelse(val == "NA", as.numeric(val), val);
            ds[[name]] = val
            cat(sprintf('%s | loaded value of %s from disk as: %s\n', format(Sys.time(),"%X"), name, val))
        }
    }

    set.default = function(ds, name, value){
        if (!(name %in% names(ds))){
            ds[[name]] = value;
            cat(sprintf('%s | setting %s to default value: %s\n', format(Sys.time(),"%X"), name, value))
        }
        return(ds);
    }

    #default directory of the repository
    ds = set.default(ds, "home_dir", file_location);

    #default directory for the library
    ds = set.default(ds, "lib_dir", .Library);

    #default file to store the log, set as NA if no log is to be produced
    ds = set.default(ds, "log_name", NA);

    #logical: if TRUE, then helpful information will be printed by the pipeline function by default
    ds = set.default(ds, "print_flag", TRUE);

    #logical: if TRUE, output will be sent to the new sink and to the current output stream (same as `split` for `sink` {base})
    ds = set.default(ds, "split_flag", FALSE);

    return(ds)
}

check.pipeline.defaults = function(ds){

    stopifnot(is.list(ds),
              "home_dir" %in% names(ds),
              "lib_dir" %in% names(ds),
              "print_flag" %in% names(ds),
              "split_flag" %in% names(ds),
              dir.exists(ds$home_dir),
              dir.exists(ds$lib_dir),
              is.logical(ds$print_flag),
              is.logical(ds$split_flag))

    return(ds);
}

if (exists(".PIPELINE_DEFAULTS")) {
    .PIPELINE_DEFAULTS = check.pipeline.defaults(.PIPELINE_DEFAULTS)
} else {
    .PIPELINE_DEFAULTS =  get.pipeline.defaults()
}

#### FUNCTIONS THAT DEPEND ON .PIPELINE_DEFAULTS ####

get.directories = function(home_dir = .PIPELINE_DEFAULTS$home_dir,
                           lib_dir = .PIPELINE_DEFAULTS$lib_dir,
                           show_directories = .PIPELINE_DEFAULTS$print_flag,
                           load_defaults_from_disk = TRUE){

    fs = list();

    defaults_file = paste0(home_dir, ".PIPELINE_DIRECTORIES");
    if (load_defaults_from_disk){
        defaults = read.csv(defaults_file, header = FALSE, stringsAsFactors = FALSE);
        for (n in 1:nrow(defaults)){
            name = defaults[n,1];
            val = defaults[n,2];
            fs[[name]] = val
            cat(sprintf('%s | loaded directory %s from disk as: %s\n', format(Sys.time(),"%X"), name, val))
        }
    }

    set.default = function(ds, name, value){
        if (!(name %in% names(ds))){
            ds[[name]] = value;
            cat(sprintf('%s | setting %s to default value: %s', format(Sys.time(),"%X"), name, value))
        }
        return(fs);
    }

    fs = set.default(fs, "home_dir", paste0(getwd(),"/"))
    fs = set.default(fs, "lib_dir", .Library)
    fs = set.default(fs, "code_dir", paste0(fs$home_dir, "src/"));
    fs = set.default(fs, "log_dir", paste0(fs$home_dir, "logs/"));
    fs = set.default(fs, "results_dir", paste0(fs$home_dir, "results/"));
    fs = set.default(fs, "data_dir", paste0(fs$home_dir, "data/"));
    fs = set.default(fs, "raw_data_dir", paste0(fs$data_dir, "raw data files/"));
    fs = set.default(fs, "report_dir", paste0(fs$code_dir, "reports/"));

    if (show_directories){
        print.to.console(paste0(rep("-",50),collapse=""));
        print.to.console("directories set as:");
        for (dname in names(fs)){
            print.to.console(sprintf("%s: %s", dname, fs[[dname]]));
        }
        print.to.console(paste0(rep("-",50),collapse=""));
    }
    return(fs);
}

# print.to.console = function(print_string, flag = .PIPELINE_DEFAULTS$print_flag){
#     if (flag){
#         cat(sprintf('%s | %s\n', format(Sys.time(),"%X"), print_string))
#     }
# }

print.to.console = function(print_string, ..., flag = .PIPELINE_DEFAULTS$print_flag){
    if (flag){
        if (length(list(...))==0){
            for (n in 1:length(print_string)){
                cat(sprintf('%s | %s\n', format(Sys.time(),"%X"), print_string))
            }
        } else {
            n_print_strs = length(print_string)
            if (n_print_strs == 1){
                cat(sprintf('%s | %s\n', format(Sys.time(),"%X"), sprintf(print_string, ...)), sep = "")
            } else {
                for (n in 1:n_print_strs){
                    cat(sprintf('%s | %s\n', format(Sys.time(),"%X"), sprintf(print_string[n], ...)), sep = "")
                }
            }
        }
    }
}

set.library = function(lib_dir, show_flag = .PIPELINE_DEFAULTS$print_flag){
    if (!is.na(lib_dir)){
        if (!dir.exists(lib_dir)){
            print.to.console("library directory (lib_dir): %s does not exist", lib_dir);
            print.to.console("will create the directory");
            suppressWarnings(dir.create(path = lib_dir, showWarnings = FALSE, recursive = TRUE));
        }
        .libPaths(lib_dir)
    }
    if (show_flag){
        print.to.console("libPaths set as:");
        for (p in .libPaths()){ print.to.console(sprintf("- %s",p));}
        print.to.console(paste0(rep("-",50),collapse=""));
    }
}

set.log = function(log_dir, log_name, split_log = .PIPELINE_DEFAULTS$split_flag){
    if (!is.na(log_name)){
        log_name = basename(log_name);
        correct_log_file_extension = (substr(log_name, nchar(log_name)-3, nchar(log_name)) == ".log")
        if (correct_log_file_extension){
            log_file = paste0(log_dir, log_name);
        } else {
            log_file = paste0(log_dir, log_name, ".log");
        }
        sink(file = log_file, append = FALSE, split = split_log);
        return(log_file);
    } else {
        return(log_name);
    }
}

load.or.set.to.default = function(setting_name, default_value, my_name = run_name, my_dir = run_dir){

    print.to.console('loading setting file for: %s',setting_name);
    if (my_name==""){
        setting_file = paste0(my_dir,setting_name,".setting");
        printable_setting_file = paste0(setting_name,".setting");
    } else {
        last_char = substr(my_name,nchar(my_name),nchar(my_name));
        if (last_char!="_"){
            setting_file = paste0(my_dir,my_name,"_",setting_name,".setting");
            printable_setting_file = paste0(my_name,"_",setting_name,".setting");
        } else {
            setting_file = paste0(my_dir,my_name,setting_name,".setting");
            printable_setting_file = paste0(my_name,setting_name,".setting");
        }
    }

    if (file.exists(setting_file)){
        setting_type  = typeof(default_value);
        scanned_value = scan(setting_file,what=setting_type,sep="", quiet = TRUE);
        scanned_type  = typeof(scanned_value);

        if (setting_type=="double" && scanned_type=="character"){
            scanned_value = as.double(scanned_value);
        } else if (setting_type=="logical" && scanned_type=="character"){
            scanned_value = as.logical(scanned_value)
        }

        print.to.console('setting file found');

        if (length(scanned_value)>1){
            print.to.console(sprintf('scanned values: (type: %s) %s', typeof(scanned_value), paste0("(",paste(scanned_value,collapse=" "),")")));
        } else {
            print.to.console(sprintf('scanned value:  (type: %s) %s', typeof(scanned_value), scanned_value))
        }

        if (length(default_value)>1){
            print.to.console(sprintf('default values: (type: %s) %s\n', typeof(default_value), paste0("(",paste(default_value,collapse=" "),")")));
        } else {
            print.to.console(sprintf('default value: (type: %s) %s\n', typeof(default_value), default_value))
        }
        return(scanned_value);

    } else {
        print.to.console('setting file %s does not exist', setting_file);
        if (length(default_value)>1){
            print.to.console(sprintf('using default values %s (type: %s)\n', paste0("(",paste(default_value,collapse=" "),")"), typeof(default_value)));
        } else {
            print.to.console(sprintf('using default value %s (type: %s)\n', default_value, typeof(default_value)));
        }
        return(default_value);
    }
}

setup.pipeline = function(env = globalenv(),
                          home_dir = .PIPELINE_DEFAULTS$home_dir,
                          lib_dir = .PIPELINE_DEFAULTS$lib_dir,
                          log_name = .PIPELINE_DEFAULTS$log_name,
                          print_flag = .PIPELINE_DEFAULTS$print_flag,
                          split_flag = .PIPELINE_DEFAULTS$split_flag){

    fs = get.directories(home_dir, lib_dir, print_flag);
    list2env(fs, env)
    set.library(fs$lib_dir, show_flag = print_flag);
    print.to.console("pipeline setup complete");
}

#### EXECUTION ####
comp_name = Sys.info()["machine"];
setup.pipeline()
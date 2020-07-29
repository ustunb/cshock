#Pipeline Installation Script
#Berk Ustun | www.berkustun.com

##### Setup Directories and Parse Command Line Arguments #####
source("setup_pipeline.R");

#setup logging
args = commandArgs(TRUE);
batch_mode = length(args) > 0
log_name = ifelse(batch_mode, args[1], NA);
log_file = set.log(log_dir = log_dir, log_name = log_name);

#install required packages for pipeline
required_pkgs = read.table(file = paste0(home_dir, "required_packages.txt"),
                           header = FALSE,
                           check.names = FALSE, colClasses = "character",
                           blank.lines.skip = TRUE);
required_pkgs = array(unique(t(required_pkgs)))

installed_pkgs = installed.packages()[,1];
update.packages(instlib = lib_dir, repos = "https://cran.revolutionanalytics.com/", ask = FALSE, checkBuilt = TRUE);

installed_idx = required_pkgs %in% installed_pkgs;
pkgs_to_install = required_pkgs[!installed_idx];

attempt = 0;
max_attempts = 5;
installed_all_packages = all(installed_idx);
while (!installed_all_packages){

    #install packages
    install.packages(pkgs = pkgs_to_install, lib = lib_dir, repos = "https://cran.revolutionanalytics.com/");

    #check if all packages have been installed properly
    installed_pkgs = installed.packages()[,1]
    installed_idx = pkgs_to_install %in% installed_pkgs;
    installed_all_packages = all(installed_idx);
    pkgs_to_install = pkgs_to_install[!installed_idx];

    #keep track of # of attempts and break loop to prevent hanging
    if (attempt >= max_attempts){
        print.to.console("Could not install all packages")
        break;
    }
    attempt = attempt + 1;
}


print.to.console(sprintf("Finished running install_pipeline in directory: %s", getwd()))

if (batch_mode){
    quit("no")
}
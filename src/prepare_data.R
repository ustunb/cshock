#Data Preparation Script
#https://github.com/ustunb/classification-pipeline
#Berk Ustun | www.berkustun.com
#
#This file shows how to create a .RData file that will be used in model training
#It contains key error-checking to make sure that the pipeline works
#The data file is called data_name"_processed.RData" and is stored under /Data/
#
#Each data file contains the following variables:
#
#- X            P x N matrix of features
#- Y            1 x N vector of outcomes with entries %in% (0,1)
#- X_test       P x N_test matrix of features for testing only
#- Y_test       1 x N_test vector of outcomes for testing only
#- X_names      P x 1 character array with names of the features
#- Y_name       1 x 1 character array with the name of the outcome
#- cvindices    list containing indices used for cross-validation
#- info         list with some information about the dataset
#

##### Setup Directories and Parse Command Line Arguments #####
cat(sprintf("running prepare_data.R in directory\n%s\n", getwd()))
source("setup_pipeline.R");

args = commandArgs(TRUE);
batch_mode = length(args) > 0
if (batch_mode){
    data_name = args[1];
    log_name =  ifelse(length(args)>1, args[2], NA);
} else {
    data_name = "cshock"
    log_name = NA;
}

log_file = set.log(log_dir = log_dir, log_name = log_name)

#files
csv_file_name_data = paste0(raw_data_dir, data_name, "_data.csv")

load_test_data_from_file=TRUE
csv_file_name_test_data = paste0(raw_data_dir, data_name, "_data_test.csv")

load_cvindices_from_file = FALSE
cvindices_file_name = paste0(raw_data_dir, data_name, "_processed.RData")

#fold generation
K_max = 10
n_cv_repeats = 5
prop_data_used_for_testing = 0.0           #proportion of data to be saved for testing
max_difference_in_train_test_balance = 0.05 #max difference in P(y=+1) between train/test set

# random seed
random_seed = 1337
set.seed(random_seed)

#### Read Data from Disk ####
data = read.csv(csv_file_name_data)
if (load_test_data_from_file){
    test_data = read.csv(csv_file_name_test_data)
    test_row_indices = c(rep(FALSE, nrow(data)), rep(TRUE, nrow(test_data)))
    stopifnot(all(colnames(data)==colnames(test_data)))
    data = rbind(data, test_data)
}

#extract X and Y matrices from data
Y_all 			= as.matrix(data[,1]);
X_all 			= as.matrix(data[,2:ncol(data)]);

#check that X and Y are not missing values
stopifnot(is.numeric(X_all)) #entries should be numeric
stopifnot(!is.na(X_all))     #entries should not contain NAs
stopifnot(is.numeric(Y_all)) #entries should be numeric
stopifnot(!is.na(Y_all))     #entries should not contain NAs

#check that all entries of Y are +1 or -1
N_all           = nrow(X_all)
N_pos           = sum(Y_all==1)
N_neg           = sum(Y_all==0)
stopifnot(N_pos+N_neg==N_all) #entries of Y should be in (0,1)

#extract variable names from data
var_names       = colnames(data)
Y_name          = var_names[1]
X_names         = var_names[2:length(var_names)]
stopifnot(!is.null(X_names)) #did not find names for feature variables (X)
stopifnot(!is.null(Y_name))  #did not find names for outcome variable (Y)

colnames(X_all) = X_names;

#check for uniqueness among variables AND variable names
stopifnot(all(!duplicated(t(X_all))))
stopifnot(!(X_names == length(unique(X_names)))) #feature names should be unique

#### Split Data into Training and Testing with Similar P(Y==+1) #####
if (load_test_data_from_file){

    N_test = sum(test_row_indices)
    N = N_all - N_test;
    X = as.matrix(X_all[!test_row_indices,])
    Y = as.matrix(Y_all[!test_row_indices,])
    X_test = as.matrix(X_all[test_row_indices,])
    Y_test = as.matrix(Y_all[test_row_indices,])

} else {

    N_test = floor(N_all * prop_data_used_for_testing)
    N = N_all - N_test;

    difference_in_balance = Inf
    X = as.matrix(X_all[,])
    Y = as.matrix(Y_all[,])
    X_test = as.matrix(X_all[0,])
    Y_test = as.matrix(Y_all[0,])

    if (N_test > 0){

        while (difference_in_balance > max_difference_in_train_test_balance){

            row_perm = sample(nrow(X_all), replace = FALSE)
            test_row_indices = sort(row_perm[1:N_test])
            train_row_indices = sort(row_perm[N_test+1:N_all])

            X = as.matrix(X_all[train_row_indices,])
            Y = as.matrix(Y_all[train_row_indices,])
            X_test = as.matrix(X_all[test_row_indices,])
            Y_test = as.matrix(Y_all[test_row_indices,])

            pos_prop_test = mean(Y_test==1)
            pos_prop_train = mean(Y == 1)
            difference_in_balance = abs(pos_prop_test - pos_prop_train)/(pos_prop_train)

        }
    }
}


#### Create List with folds for repeated K-fold CV for K = 1.. K_max #####
cvindices = list()
for (k in seq(1, K_max)){
    fold_matrix = matrix(nrow = N, ncol = n_cv_repeats)
    if (k == 1){
        fold_matrix[,] = 1.0
    } else {
        for (r in 1:n_cv_repeats){
            fold_matrix[,r] = cut(sample(N), k, labels = FALSE)
        }
    }
    cv_index_name = sprintf("K%02d",k)
    cvindices[[cv_index_name]] = fold_matrix
}

##### Save File #####
data_file_name = paste0(data_dir, data_name, "_processed.RData")

info = list()
info$data_name = data_name
info$date = format(Sys.time(),"%m/%d/%y at %H:%M")
info$sys_info = Sys.info()
info$random_seed = random_seed;
info$data_file_name = data_file_name;
info$csv_file_name_data = csv_file_name_data;
info$csv_file_name_test_data = csv_file_name_test_data;

save(file = data_file_name, "X_names", "Y_name", "X", "X_test", "Y", "Y_test", "cvindices", "info")
print.to.console(sprintf("Saved data variables in file\n%s\n", data_file_name))
if (batch_mode){
    print.to.console("quitting R");
    quit(save = "no", status = 0);
}
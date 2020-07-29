# Risk-SLIM
- pushing this soon

# Pipeline (to fit models with basic ML methods)
## Directory Structure 

- /data/                 	processed RData files of datasets
- /data/raw data files/  	raw csv files of datasets
- /results/              	RData files containing run results
- /logs/                 	Log files for pipeline runs
- /src/                  	R code 
- /src/reports				Rnw-files to create reports

## Pipeline Setup

To setup the pipeline on your machine:

1. Run `./install_pipeline.sh` to install required R packages

## Dataset Creation

1. Add a raw file to data/raw data files/. The data file should be named `{data_name}.csv` where `{data_name}` is a unique name for the dataset. The file must adhere to some rough guidelines, namely:

- column names must be at the top row
- no duplicate columns
- no duplicate column names
- outcome variable must be in column 1
- outcome variable must be composed of all 0s and 1s
- all entries must be numeric

2. Run `./src/prepare_data.R` to create the `{data_name}_processed.RData` file

## Running the Pipeline

Once the data file is in the proper format, users can run the pipeline to produce classification models. 

Each "call" to the pipeline runs multiple classification methods for a single dataset (`data_name`) and a single weight on the positive examples (`w_pos`). 

Methods that are currently supported include:

- CART (cart)
- C5.0 Tree (c50_tree)
- C5.0 Rule (c50_rule)
- L1-Penalized Logistic Regression (lars_lasso)
- L2-Penalized Logistic Regression (lars_ridge)
- L1+L2-Penalized Logistic Regression (lars_elastic_net)
- Random Forests (randomforest)
- Stochastic Gradient Boosting (sgb)
- SVM Linear Kernel (svm_linear)
- SVM RBF (svm_rbf)

To run the pipeline:

1. Edit `train_models.sh` with the `data_name`, `fold_id`, range of weights, methods and free parameters for each method.

2. Execute `train_models.sh` in Bash
#Dataset Processing Script for CSHOCK
#Berk Ustun | www.berkustun.com
#-------------------------------------------------------------------------------
#This file loads the raw dataset, does some cleanup, and generates CSV files
#for the training and testing datasets

data_name = "cshock"
outcome_name = "hospital_mortality"
version_code = 10

#set directories
data_dir = "/Users/berk/Dropbox (Harvard University)/repos/cshock/data/"
raw_data_dir = "/Users/berk/Dropbox (Harvard University)/repos/cshock/data/raw data files/"

#load libraries
source(paste0(raw_data_dir,"processing_functions.R"));

#load data
raw_data_old = read.csv(file = paste0(raw_data_dir, "/cshock/cshock_raw_2020.csv"), sep = ',')
raw_data = read.csv(file = paste0(raw_data_dir, "/cshock/cshock_raw.csv"), sep = ',')

a = raw_data %>% select(-scai_shock) %>% select(-any_of('y'))
b = raw_data_old %>% select(-scai_shock) %>% select(-any_of('y'))

stopifnot(colnames(a) == colnames(b))
stopifnot(a==b)

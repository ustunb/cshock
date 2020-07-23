#Dataset Processing Script for CSHOCK
#Berk Ustun | www.berkustun.com
#-------------------------------------------------------------------------------
#This file loads the raw dataset, does some cleanup, and generates CSV files
#for the training and testing datasets

#set names
data_name = "cshock"
outcome_name = "y"

#set directories
data_dir = "/Users/berk/Dropbox (Harvard University)/repos/cshock/data/"
data_file_name = "/Users/berk/Dropbox (Harvard University)/repos/cshock/data/cshock_raw.csv"

#load libraries
source(paste0(data_dir,"processing_functions.R"));

#load data
raw_data = read.csv(file = data_file_name, sep = ',')

# basic formatting
df = raw_data %>%
    clean_names("snake") %>%
    mutate(ethnicity = as.factor(tolower(as.character(ethnicity)))) %>%
    rename(sex = gender,
           hr_min = heart_rate_min,
           hr_max = heart_rate_max,
           hr_mean = heart_rate_mean,
           ckd = chronic_kidney_disease,
           acd = acute_cerebrovascular_disease,
           copdb = chronic_obstructive_pulmonary_disease_and_bronchiectasis) %>%
    select(-pt_max) %>% #MDs said "ignore pt_max "
    select(-any_pressor) %>% #dropping because we will recode
    select(-testing) %>% #dropping because it's redundant
    select(-scai_shock) %>% #we aren't using this?
    select(-mld, -msld, -pud, -aids, -dementia, -rheumd) %>%
    select(y, training, everything()) #dropping because all features are = 0 in test_set


# convert categotical attributes
df = df %>%
    dummy_cols(select_columns = 'sex', remove_most_frequent_dummy = TRUE) %>%
    rename(female = sex_F)

# convert numerical attributes
bool_df = df %>%
    mutate(hr_min_leq_60 = hr_min <= 60,
           hr_mean_geq_100 = hr_mean >= 100,
           #
           total_pressors_geq_1 = total_pressors>=1,
           #
           charlson_score_geq_1 = charlson_score>=1,
           charlson_score_geq_2 = charlson_score>=2,
           charlson_score_geq_3 = charlson_score>=3,
           #
           sys_bp_min_leq_80 = sys_bp_min <= 80,
           sys_bp_min_geq_140 = sys_bp_max <= 140,
           sys_bp_min_geq_180 = sys_bp_max <= 180,
           #
           dias_bp_min_leq_40 = dias_bp_min <= 40,
           dias_bp_max_geq_90 = dias_bp_max >= 90,
           dias_bp_max_geq_110 = dias_bp_max >= 110,
           #
           resp_rate_mean_leq_20 =  resp_rate_mean <= 20,
           resp_rate_min_leq_12 = resp_rate_min <= 12,
           resp_rate_max_geq_25 = resp_rate_max >= 25,
           #
           sp_o2_min_leq_88 = sp_o2_min <= 88,
           sp_o2_mean_geq_95 = sp_o2_mean >= 95, #Q1 df %>% filter(training == 1) %>% select(sp_o2_mean) %>% summary()
           sp_o2_mean_geq_98 = sp_o2_mean >= 98,
           #
           temp_c_mean_geq_38 = temp_c_mean >= 38,
           #
           age_geq_60 = age >= 60,
           age_geq_70 = age >= 70,
           age_geq_80 = age >= 80,
           #
           aniongap_max_geq_14 = aniongap_max >= 14,
           #
           bicarbonate_min_leq_22 = bicarbonate_min <= 22,
           #
           chloride_max_geq_110 = chloride_max >= 110,
           #
           glucose_min_leq_78 = glucose_min <= 78,
           #
           hematocrit_max_geq_50 = hematocrit_max >= 50,
           #
           hemoglobin_min_leq_8 = hemoglobin_min <= 8,
           hemoglobin_min_leq_10 = hemoglobin_min <= 10,
           #
           platelet_min_leq_150 = platelet_min<= 150,
           platelet_min_geq_400 = platelet_max>= 400,
           #
           potassium_max_geq_5 = potassium_max >= 5,
           #
           inr_max_geq_2 = inr_max >= 2.0,
           #
           sodium_min_leq_135 = sodium_min <= 135,
           #
           bun_max_geq_25 = bun_max >= 25,
           #
           wbc_max_leq_11 = wbc_max <= 11,
           wbc_max_geq_20 = wbc_max >= 20,
           #
           rdw_max_leq_14 = rdw_max <= 14,
           #
           shock_index_geq_1 = shock_index >= 1.0,
           #
           gcs_leq_8 = gcs <= 8) %>%
    select(-total_pressors) %>%
    select(-charlson_score) %>%
    select(-hr_min, -hr_mean, -hr_max) %>%
    select(-sys_bp_min, -sys_bp_max, -sys_bp_mean) %>%
    select(-dias_bp_min, -dias_bp_max, -dias_bp_mean) %>%
    select(-resp_rate_min, -resp_rate_mean, -resp_rate_max) %>%
    select(-mean_bp_mean, -mean_bp_min, -mean_bp_max) %>%
    select(-sp_o2_min, -sp_o2_mean) %>%
    select(-temp_c_mean) %>%
    select(-aniongap_max) %>%
    select(-bicarbonate_min) %>%
    select(-chloride_max) %>%
    select(-glucose_min) %>%
    select(-hematocrit_max) %>%
    select(-hemoglobin_min) %>%
    select(-platelet_min, -platelet_max) %>%
    select(-potassium_max) %>%
    select(-sodium_min) %>%
    select(-bun_max) %>%
    select(-inr_max) %>%
    select(-wbc_max) %>%
    select(-rdw_max,-rdw_min) %>%
    select(-shock_index) %>%
    select(-gcs)

#final formatting
bool_df = bool_df %>%
    mutate_if(is.logical, as.numeric) %>%
    arrange(desc(training), y, ethnicity, sex, age) %>%
    select(training, ethnicity, sex, age,  y, everything())

cts_df = df %>%
    arrange(desc(training), y, ethnicity, sex, age) %>%
    select(training, ethnicity, sex, age,  y, everything())


#save datasets
for (v in c(0, 1)){

    #set file names
    file_header = paste0(data_dir, data_name)
    if (v == 1){
        data_file = paste0(file_header, "_data.csv")
        cts_data_file = paste0(file_header, "_cts_data.csv")
    } else {
        data_file = paste0(file_header, "_data_test.csv")
        cts_data_file = paste0(file_header, "_cts_data_test.csv")
    }

    data_df = bool_df %>%
        filter(training == v) %>%
        select(-training)

    info_df = data_df %>%
        select(sex, age, ethnicity)

    data_df = data_df %>%
        select(-sex, -age, -ethnicity) %>%
        select(outcome_name, everything())

    cts_df = df %>%
        filter(training == v) %>%
        select(-training, -sex, -age, -ethnicity) %>%
        select(outcome_name, everything())

    ## check outputs
    X = data_df %>% select(-y)
    stopifnot(!any(duplicated(t(X))))

    #save binarized data
    write.csv(x = data_df, file = data_file, row.names = FALSE, quote = FALSE);

    #save continuous data
    write.csv(x = cts_df, file = cts_data_file, row.names = FALSE, quote = FALSE);

    #save helper file
    #write.csv(x = get.header.descriptions(data, outcome_name), file = paste0(file_header, "_helper.csv"), row.names = FALSE, quote = FALSE);
}


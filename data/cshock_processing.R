#Dataset Processing Script for CSHOCK
#Berk Ustun | www.berkustun.com
#-------------------------------------------------------------------------------
#This file loads the raw dataset, does some cleanup, and generates CSV files
#for the training and testing datasets
data_name = "cshock"
outcome_name = "hospital_mortality"

#set directories
data_dir = "/Users/berk/Dropbox (Harvard University)/repos/cshock/data/"
raw_data_dir = "/Users/berk/Dropbox (Harvard University)/repos/cshock/data/raw data files/"
raw_data_file = paste0(raw_data_dir, "/cshock/cshock_raw.csv")

# load libraries
source(paste0(raw_data_dir,"processing_functions.R"));

# load data
raw_data = read.csv(file = raw_data_file, sep = ',')

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
    select(-scai_shock) %>% #we aren't using this
    select(-mld, -msld, -pud, -aids, -dementia, -rheumd) %>% #dropping because all features are = 0 in test_set
    select(-any_of('y')) %>%
    dummy_cols(select_columns = 'sex', remove_most_frequent_dummy = TRUE, remove_selected_columns = FALSE) %>% rename(female = sex_F) %>%
    select(outcome_name, training, everything())

demographic_group_names = c("female", "asian", "black", "hispanic", "white", "other_ethnicity")

#### summary statistics ####

# create a data.frame of flattened summary statistics
flat_df = df %>%
    # add dummies
    select(-sex) %>%
    #
    # add dummies for ethnicity
    dummy_cols(select_columns = 'ethnicity', remove_most_frequent_dummy = FALSE, remove_selected_columns = TRUE) %>%
    rename_at(vars(starts_with("ethnicity_")), list(sub=function(x){gsub("ethnicity_", "", x)})) %>%
    select(female, asian, black, hispanic, white, other_ethnicity = other, outcome_name, training, everything()) %>%
    #
    # change indicator for training
    mutate(study = as.character(ifelse(training, "train", "test"))) %>%
    select(-training) %>%
    rename_all(.funs = function(x) str_replace_all(x, "_", "raw data files/cshock")) %>%
    group_by(study) %>%
    summarize_all(.funs = list("mean" = mean, "sd" = sd, "min" = min, "median" = median,  "max" = max))  %>%
    select(sort(tidyselect::peek_vars())) %>%
    select(study, starts_with("hospital_mortality"), everything()) %>%
    pivot_longer(cols = !study,
                 names_sep = "_",
                 names_to= c("name", "stat"),
                 values_to = "value") %>%
    mutate(name = str_replace_all(name, "\\.", "_"))

summary_df = flat_df %>%
    pivot_wider(names_from = c(study, stat), values_from = value) %>%
    select(name, ends_with("mean"), ends_with("sd"), ends_with("min"), ends_with("median"), ends_with("max")) %>%
    select(name, starts_with("train"), starts_with("test")) %>%
    mutate(is_outcome = as.logical(ifelse(name==outcome_name, 1, 0)),
           is_predictor = as.logical(ifelse(name %in% c(outcome_name, demographic_group_names), 0, 1))) %>%
    select(name, is_outcome, is_predictor, everything())

summary_df = bind_rows(
    summary_df %>% filter(!is_outcome,!is_predictor),
    summary_df %>% filter(is_outcome),
    summary_df %>% filter(is_predictor)
    ) %>%
    mutate_if(is.logical, as.numeric)

summary_csv_file = paste0(raw_data_dir, data_name, "_summary.csv")
write.csv(x = summary_df, file = summary_csv_file, row.names = FALSE, quote = FALSE);

#### training data ####

# convert numerical attributes
bool_df = df %>%
    mutate(#
        age_geq_60 = age >= 60,
        age_geq_70 = age >= 70,
        age_geq_80 = age >= 80,
        #
        # hr mean replacements
        # hr_mean_geq_100 = hr_mean >= 100,
        hr_min_geq_100 = hr_min >= 100, #12-15
        hr_max_geq_100 = hr_max >= 100, #12-15
        hr_max_leq_60 = hr_max <= 60, #1-13
        #
        total_pressors_geq_1 = total_pressors >= 1,
        #
        sys_bp_min_lt_80 = sys_bp_min < 80, #2-1
        sys_bp_min_geq_140 = sys_bp_min >= 140, #1-13
        #
        sys_bp_max_leq_80 = sys_bp_max <= 80,
        #
        # resp_rate mean replacements (12-15)
        resp_rate_max_leq_12 = resp_rate_max <= 12, #1-13
        resp_rate_min_geq_20 = resp_rate_min >= 20,
        resp_rate_min_geq_25 = resp_rate_min >= 25,
        #
        sp_o2_min_lt_88 = sp_o2_min < 88, #1-13
        sp_o2_min_geq_95 = sp_o2_min >= 95,
        #
        #
        aniongap_max_geq_14 = aniongap_max >= 14,
        bicarbonate_min_leq_22 = bicarbonate_min <= 22,
        chloride_max_geq_110 = chloride_max >= 110,
        glucose_min_leq_78 = glucose_min <= 78,
        hematocrit_max_geq_50 = hematocrit_max >= 50,
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
        ) %>%
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
    arrange(desc(training), outcome_name, ethnicity, sex, age) %>%
    select(training, ethnicity, sex, age,  outcome_name, everything())

cts_df = df %>%
    arrange(desc(training), outcome_name, ethnicity, sex, age) %>%
    select(training, ethnicity, sex, age,  outcome_name, everything())

#save datasets
for (v in c(0, 1)){

    #set file names
    file_header = paste0(raw_data_dir, data_name)

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

    # check outputs
    X = data_df %>% select(-outcome_name)
    stopifnot(!any(duplicated(t(X))))

    #save binarized data
    write.csv(x = data_df, file = data_file, row.names = FALSE, quote = FALSE);

    #save continuous data
    write.csv(x = cts_df, file = cts_data_file, row.names = FALSE, quote = FALSE);

    #save helper file
    #write.csv(x = get.header.descriptions(data, outcome_name), file = paste0(file_header, "_helper.csv"), row.names = FALSE, quote = FALSE);
}


#!/bin/bash

#all_data_names=("adult" "apnea" "diabetes" "credit" "compas_arrest" "compas_violent" "recidivism")
#all_data_names=("adult_bl" "census_bl" "compas_arrest_bl" "compas_violent_bl" "apnea_bl" "lungcancer_bl")

all_data_names=("census" "compas_arrest" "compas_violent" "apnea" "lungcancer" "census_bl" "compas_arrest_bl" "compas_violent_bl" "apnea_bl" "lungcancer_bl")

#all_data_names=("adult_bl" "census_bl") #"compas_arrest_bl" "compas_violent_bl" "apnea_bl" "lungcancer_bl")
#all_method_names=("dccp_blind_lr")
all_method_names=("tree_01" "tree_lr")
fold_id="K04N01"
max_runtime=300

# riskslim job info
submit_missing_jobs_only='FALSE'
job_script_name="job_decoupled_training"
job_runner_name="batch/experiment_runner.py"
results_file_suffix="results"

# setup directories
source "setup_directories.sh"
source "submit_helper.sh"

#### JOB SUBMISSION ####

for data_name in ${all_data_names[*]}; do
for method_name in ${all_method_names[*]}; do


case ${method_name} in
    tree*)
        all_attribute_names=("all")
    ;;
    *)
        case ${data_name} in
            adult|adult_bl)
                all_attribute_names=("all" "sex" "maritalstatus" "immigrant")
            ;;
            credit|credit_bl)
                all_attribute_names=("all" "gender" "age" "marital")
            ;;
            diabetes|diabetes_bl)
                all_attribute_names=("all" "gender" "age" )
            ;;
            apnea|apnea_bl)
                all_attribute_names=("all" "gender" "age" )
            ;;
            lungcancer|lungcancer_bl)
                all_attribute_names=("all" "gender" "age")
            ;;
            compas_arrest|compas_arrest_bl)
                all_attribute_names=("all" "race" "sex" "age")
            ;;
            compas_violent|compas_violent_bl)
                all_attribute_names=("all" "race" "sex" "age")
            ;;
            recidivism|recidivism_bl)
                all_attribute_names=("all" "race" "gender" "age")
            ;;
            *)
                all_attribute_names=("all")
            ;;
        esac
esac


for attr_id in ${all_attribute_names[*]}; do

job_name="${data_name}_${attr_id}_${fold_id}_${method_name}"
results_file="${base_run_dir}${job_name}_${results_file_suffix}.pickle"

script_parameters=("comp_name=${comp_name}"
		"data_name=${data_name}"
		"attr_id=${attr_id}"
		"fold_id=${fold_id}"
		"method_name=${method_name}"
		"results_file=${results_file}"
		"max_runtime=${max_runtime}"
		"job_runner_name=${job_runner_name}"
		)

script_parameters=$(joinStrings , "${script_parameters[@]}")
	
if [ "${submit_missing_jobs_only}" = "TRUE" ]; then
	if [ ! -f "${results_file}" ]; then
		sendJob job_number ${job_name} "${job_script_name}" "${script_parameters}"  ""
	fi
else
	sendJob job_number ${job_name} "${job_script_name}" "${script_parameters}"
fi


done
done
done

exit

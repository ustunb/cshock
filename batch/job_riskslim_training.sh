#!/bin/bash
#SBATCH
# This is a subscript for jobs should be called through a superscript 
#
# Current superscripts: 
#- submit_pipeline_training_jobs.sh
#- submit_pipeline_processing_jobs.sh
#- submit_pipeline_training_and_processing_jobs.sh

#Parse Command Line Input
IFS=',' read -ra COMMAND_ARGS <<< "$1"
for script_parameter in "${COMMAND_ARGS[@]}"; do
    eval ${script_parameter}
done

#Print Expected Inputs to Screen
echo "comp_name: "${comp_name}
echo "data_name: "${data_name}
echo "attr_id: "${attr_id}
echo "fold_id: "${fold_id}
echo "method_name: "${method_name}
echo "results_file: "${results_file}
echo "max_runtime: "${max_runtime}
echo "job_runner_name: "${job_runner_name}

#setup
current_dir="$(pwd)"
source "setup_directories.sh"
source "setup_python.sh"
source "job_helper.sh"

#run directory and 
run_name="${data_name}_${method_name}_${attr_id}"
run_dir="${base_run_dir}$"
now=$(date +"%m_%d_%Y_%H_%M_%S")
run_log="${run_name}_${now}.log"

# run job
cd "${repo_dir}"
echo "$(pwd)"
echo "running ${job_runner_name}"
echo "SCRIPT: ${repo_dir}${job_runner_name}" 
echo "arg[0] = ${comp_name}" 
echo "arg[1] = ${data_name}"
echo "arg[2] = ${fold_id}" 
echo "arg[3] = ${attr_id}" 
echo "arg[4] = ${method_name}" 
echo "arg[5] = ${results_file}" 
echo "arg[6] = ${max_runtime}"
python3 "${repo_dir}${job_runner_name}" "${comp_name}" "${data_name}" "${fold_id}" "${attr_id}" "${method_name}" "${results_file}" "${max_runtime}"
echo "finished ${job_runner_name}"
cd "${current_dir}"

echo "Quitting..."
exit
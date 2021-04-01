current_dir="$(pwd)"
echo "current_dir: ${current_dir}"
source "comp_name.sh"
source "setup_directories.sh"
source "job_helper.sh"

data_name="adult"
fold_id="K04N01"
results_file_suffix="results"
max_runtime=10
job_runner_name="batch/experiment_runner.py"

all_method_names=("onehot_lr")
#all_method_names=("dccp_parity_lr" "dccp_parity_svm")

for method_name in ${all_method_names[*]}; do

case ${method_name} in
    tree*)
        all_attribute_names=("all")
        max_runtime=5
    ;;
    *)
        case ${data_name} in
            adult)
                all_attribute_names=("all" "maritalstatus")
            ;;
            credit)
                all_attribute_names=("all" "gender" "age" "marital")
            ;;
            diabetes)
                all_attribute_names=("all" "gender" "age" )
            ;;
            apnea)
                all_attribute_names=("all" "gender" "age" )
            ;;
            lungcancer*)
                all_attribute_names=("all" "gender" "age")
            ;;
            compas_arrest)
                all_attribute_names=("all" "race" "sex" "age")
            ;;
            compas_violent)
                all_attribute_names=("all" "race" "sex" "age")
            ;;
 	    	recidivism)
				all_attribute_names=("all" "race" "gender" "age")
	    	;;
	    	*)
				all_attribute_names=("all")
	    	;;
        esac
    ;;
esac

for attr_id in ${all_attribute_names[*]}; do


	job_name="${data_name}_${attr_id}_${fold_id}_${method_name}"
	results_file="${base_run_dir}${job_name}_${results_file_suffix}.pickle"

	echo "comp_name: "${comp_name}
	echo "data_name: "${data_name}
	echo "attr_id: "${attr_id}
	echo "fold_id: "${fold_id}
	echo "method_name: "${method_name}
	echo "results_file: "${results_file}
	echo "max_runtime: "${max_runtime}
	echo "job_runner_name: "${job_runner_name}
	echo "-----------------------------------"

	#run directory and
	run_name="${data_name}_${method_name}_${attr_id}"
	run_dir="${base_run_dir}$"

	#run log
	now=$(date +"%m_%d_%Y_%H_%M_%S")
	run_log="${run_name}_${now}.log"

	# run job
	cd "${repo_dir}"
	echo "running ${job_runner_name} from directory $(pwd)"
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

done
done
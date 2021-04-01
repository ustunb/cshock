joinStrings() { local d=$1; shift; echo -n "$1"; shift; printf "%s" "${@/#/$d}"; }

sendJob() {

    local _return_var=$1
    local job_name=$2
    local script_name=$3
    local script_parameters=$4
    local job_time_limit=$5
    local job_mem_limit=$6
    local hold_until_jobs=$7
    local job_log=$8
    local job_partition=$9

    if [ -n "${hold_until_jobs}" ]; then
        echo "submitting dependent job to run after jobs: ${hold_until_jobs}"
    else
        echo "submitting normal job"
    fi
    now=$(date +"%m_%d_%Y_%H_%M_%S")

    if [ -z "${job_time_limit}" ]; then
        job_time_limit="600"
    fi

    if [ -z "${job_mem_limit}" ]; then
        job_mem_limit="16G"
    fi

    if [ -z "${job_partition}" ]; then
        job_partition="general"
    fi

    if [ -z "${job_log}" ]; then
        job_log="${log_dir}${job_name}_${now}.log"
    fi

    if [ -z "${job_error_log}" ]; then
        job_error_log="${log_dir}${job_name}_${now}.err"
    fi


    case ${comp_name} in
        berkmac)
            bash "${script_name}.sh" "${script_parameters}"
            job_id=101
        ;;
        odyssey)
            if [ -n "${hold_until_jobs}" ]; then
                sbatch --dependency=afterok:"${hold_until_jobs}" --output ${job_log} --error ${job_error_log} --partition ${job_partition} --job-name=${job_name} --mem ${job_mem_limit} --time ${job_time_limit} --nodes=1 --export="${script_parameters}" "${script_name}.sh" | tee -a job_id.tmp
            else
                sbatch --output ${job_log} --error ${job_error_log} --partition ${job_partition} --job-name=${job_name} --mem ${job_mem_limit} --time ${job_time_limit} --nodes=1 --export="${script_parameters}" "${script_name}.sh" | tee -a job_id.tmp
            fi
            job_id=$(cat job_id.tmp | grep -o -P 'Submitted batch job .{0,9}' | cut -f4- -d' ')
            rm job_id.tmp
        ;;
    esac

    echo "-----------------------------"
    echo "submission for job: ${job_id}"
    echo "-----------------------------"
    echo "script_name: "${script_name}".sh"
    echo "script_parameters: "${script_parameters}
    echo "job_name: "${job_name}
    echo "job_mem_limit: "${job_mem_limit}
    echo "job_time_limit: "${job_time_limit}
    echo "job_log: "${job_log}
    echo "job_error_log: "${job_error_log}
    echo "job_partition: "${job_partition}
    echo "------------------------"
    eval $_return_var="'$job_id'"
}
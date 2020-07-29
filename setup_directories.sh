#get directories
comp_name="unknown"

home_dir=`pwd`'/'
log_dir="${home_dir}logs/"
lib_dir="${home_dir}ext/" 				#set as NA to use default R directory
code_dir="${home_dir}src/"
report_dir="${code_dir}reports/"
data_dir="${home_dir}data/"
raw_data_dir="${home_dir}data/raw data files"
base_run_dir="${home_dir}results/"

#create directories if they do not exist 
mkdir -p "${data_dir}"
mkdir -p "${raw_data_dir}"
mkdir -p "${log_dir}"
mkdir -p "${base_run_dir}"

#create directories if they do not exist 
if [ "${lib_dir}" != "NA" ]; then
	mkdir -p "${lib_dir}"
fi

#print directory list to screen
echo "-------------------------------"
echo "directory list"
echo "-------------------------------"
echo "comp_name: ${comp_name}"
echo "home_dir: ${home_dir}"
echo "data_dir: ${data_dir}"
echo "lib_dir: ${lib_dir}"
echo "code_dir: ${code_dir}"
echo "report_dir: ${report_dir}"
echo "raw_data_dir: ${raw_data_dir}"
echo "base_run_dir: ${base_run_dir}"
echo "log_dir: ${log_dir}"

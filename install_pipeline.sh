#get directories
source "./setup_directories.sh"

# write pipeline constants to disk (just in case)
pipeline_cons_file="${home_dir}.PIPELINE_DEFAULTS"
echo "home_dir,${home_dir}" > pipeline_cons_file
echo "lib_dir,${lib_dir}" >> pipeline_cons_file
echo "log_name,NA" >> pipeline_cons_file
echo "print_flag,TRUE" >> pipeline_cons_file
echo "split_flag,FALSE" >> pipeline_cons_file

# write pipeline directories to disk (just in case)
pipeline_dirs_file="${home_dir}.PIPELINE_DIRECTORIES"
echo "home_dir,${home_dir}" > pipeline_dirs_file
echo "data_dir,${data_dir}" >> pipeline_dirs_file
echo "raw_data_dir,${raw_data_dir}" >> pipeline_dirs_file
echo "log_dir,${log_dir}" >> pipeline_dirs_file
echo "lib_dir,${lib_dir}" >> pipeline_dirs_file
echo "code_dir,${code_dir}" >> pipeline_dirs_file
echo "report_dir,${report_dir}" >> pipeline_dirs_file
echo "results_dir,${base_run_dir}" >> pipeline_dirs_file

run_log="${log_dir}/install_pipeline.log"
Rscript "${pipeline_dir}install_pipeline.R" "${run_log}"

#!/bin/bash
current_dir=$(pwd)
source "comp_name.sh"
case ${comp_name} in
    berkmac)
        repo_dir='/Users/berk/Dropbox (Harvard University)/repos/fwh/'
    ;;
    odyssey)
        repo_dir='/n/home01/berk/repos/fwh/'
    ;;
esac
data_dir="${repo_dir}data/"
base_run_dir="${repo_dir}results/"
log_dir="${repo_dir}logs/"

#create directories if they don't exist
mkdir -p "${base_run_dir}"
mkdir -p "${log_dir}"

#print directories
echo "-------------------------------"
echo "BASH Directories"
echo "-------------------------------"
echo "comp_name: ${comp_name}"
echo "current_dir: ${current_dir}"
echo "repo_dir: ${repo_dir}"
echo "base_run_dir: ${base_run_dir}"
echo "log_dir: ${log_dir}"
echo "-------------------------------"

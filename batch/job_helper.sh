#!/usr/bin/env bash
#load modules
source "comp_name.sh"
case ${comp_name} in
    berkmac)
        comp_ram=4194304
        slim_dir="/Users/berk/Desktop/Dropbox (Harvard University)/Research/SLIM/"
    ;;
    odyssey)
        comp_ram=$(cat /proc/meminfo | grep MemTotal | awk '{print $2}')
        module load gcc R/3.4.2-fasrc01
        echo $(printf 'comp_ram: %d' ${comp_ram})
    ;;
esac

module list

#helper functions
writeSettingsToDisk() {  

  local outDir=$1 preName=$2 name fname rest isArray

  while IFS='=' read -r name rest; do

    # Determine the output filename - the variable name w/o preName.
    fname=${name#$preName}

    # Determine if the variable is an array.
    [[ $(declare -p "$name" | cut -d' ' -f2) == *a* ]] && isArray=1 || isArray=0
    (( isArray )) && name="$name[@]"

    # Output var. value and send to output file.
    echo "${!name}" > "$outDir""_""$fname.setting"

  done < <(shopt -so posix; set | egrep "^${preName}[^ =]*=")

}





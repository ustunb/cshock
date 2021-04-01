#!/bin/bash

#get directories
source "comp_name.sh"
echo "comp_name is: ${comp_name}"

#results file name
case ${comp_name} in
    odyssey)

		if [ -z "${python_env_name}" ]; then
			python_env_name="riskslim_python3"
		fi

		case ${python_env_name} in
			my_root)
				module load python/2.7.13-fasrc01
				source activate "${python_env_name}"
			;;
			*)
				module load python/3.6.3-fasrc01
				source activate "${python_env_name}"
			;;
		esac
		echo "loaded python: "${python_env_name}
    ;;

esac


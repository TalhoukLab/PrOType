#!/bin/bash

##################################################
############# Input Paremeters for Run ###########
##################################################

. ./Parameters.sh

# specify consensus metrics to compute
file_to_submit=()
##################################################
############ Execute jobs on cluster #############
##################################################
for dataset in "${dataSets[@]}"; do
    for i in "${cons[@]}"; do
        # execute shell_file to cluster node
        shell_file=$workDir$dataset/sh_file/consensus/Create_$i.sh
        if command -v qsub &>/dev/null; then
            echo "Submitting to queue: $shell_file"
            file_to_submit+=($shell_file)
        fi

        chmod +x $shell_file
    done

    if command -v qsub &>/dev/null; then
        :
    else
      python assets/submit_local.py --num_parallel 4 --file_location $workDir$dataset --step consensus
    fi
done

if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh

    echo "Finished Submitting files.  Check progress with \"qstat -u ${user}\""
    echo "The logs can be found in \"${logDir}\""
fi

#!/bin/bash

. Parameters.sh

##################################################
############# Submit jobs to cluster #############
##################################################
file_to_submit=()
for dataset in "${dataSets[@]}"; do
    for s in `seq 1 $reps`; do
        for i in "${algs[@]}"; do
            shell_file=$workDir$dataset/sh_file/clust/$i$s.sh
            if command -v qsub &>/dev/null; then
                echo "Using: $shell_file"
                file_to_submit+=($shell_file)
            fi

            chmod +x $shell_file
        done
    done

    if command -v qsub &>/dev/null; then
        :
    else
        chmod +x $workDir/1_Unsupervised/submit_local.py
        python $workDir/1_Unsupervised/submit_local.py --num_parallel 4 --file_location $workDir$dataset --step clust
    fi
done

if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh

    echo "Finished Submitting files.  Check progress with \"qstat -u ${user}\""
    echo "The logs can be found in \"${logDir}\""
fi
#!/bin/bash

. ./Parameters.sh

##################################################
############# Submit jobs to cluster #############
##################################################
file_to_submit=()
for dataset in "${dataSets[@]}"; do
    for s in `seq $c $c $reps`; do
        for i in "${algs[@]}"; do
            shell_file=$workDir$dataset/sh_file/merge/Merge_$i$s.sh
            echo "Using: $shell_file"
            if command -v qsub &>/dev/null; then
                # execute shell_file to cluster
                echo "Using: $shell_file"
                file_to_submit+=($shell_file)
            fi

            chmod +x $shell_file
        done
    done

    if command -v qsub &>/dev/null; then
        :
    else
      python $workDir/1_Unsupervised/submit_local.py --num_parallel 4 --file_location $workDir$dataset --step merge
    fi
    # complete
    echo 'Submitted merge files to the queue!'
done

if command -v qsub &>/dev/null; then
    ./assets/submit_queue.sh

    echo "Finished Submitting files.  Check progress with \"qstat -u ${user}\""
    echo "The logs can be found in \"${logDir}\""
fi
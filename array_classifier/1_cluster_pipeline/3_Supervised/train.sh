#!/bin/bash

. ./Parameters.sh

# specify algs
file_to_submit=()
for dataset in "${dataSets[@]}"; do
    for s in `seq 1 $reps`; do
        for i in "${supervisedAlgs[@]}"; do
            shell_file=$workDir$dataset/sh_file/train/$i$s.sh
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
        python $workDir/1_Unsupervised/submit_local.py --num_parallel 4 --file_location $workDir$dataset --step train
    fi
done


if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh

    echo "Finished Submitting files.  Check progress with \"qstat -u ${user}\""
    echo "The logs can be found in \"${logDir}\""
fi
#!/bin/bash

. ./Parameters.sh


##################################################
########## Execute R scripts for merge ###########
##################################################
file_to_submit=()
for dataset in "${dataSets[@]}"; do

    mkdir -p $workDir$dataset/R_file/eval/
    mkdir -p $workDir$dataset/sh_file/eval/

    R_eval=$workDir$dataset/R_file/eval/eval_run.R
    shell_file=$workDir$dataset/sh_file/eval/eval_run.sh
    # execute R scripts
    echo "Rscript $R_eval" > $shell_file

    chmod +x $shell_file

    if command -v qsub &>/dev/null; then
        echo "Using: $shell_file"
        file_to_submit+=($shell_file)
    else
        bash $shell_file
    fi
done

if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh

    echo "Finished Submitting files.  Check progress with \"qstat -u ${user}\""
    echo "The logs can be found in \"${logDir}\""
fi
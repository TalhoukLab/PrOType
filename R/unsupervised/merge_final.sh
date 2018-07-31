#!/bin/bash

. ./Parameters.sh

##################################################
########## Execute R scripts for merge ###########
##################################################
file_to_submit=()
for dataset in "${dataSets[@]}"; do

    # get R scripts
    mkdir -p $workDir$dataset/R_file/merge
    mkdir -p $workDir$dataset/sh_file/merge

    R_merge_final_clust=$workDir$dataset/R_file/merge/Merge_final_clust.R
    R_merge_final_consmat=$workDir$dataset/R_file/merge/Merge_final_consmat.R

    shell_file=$workDir$dataset/sh_file/merge/merge_final.sh

    echo "echo 'merge_final_clust'" > $shell_file
    echo "Rscript $R_merge_final_clust" >> $shell_file
    echo "echo 'merge_final_consmat'" >> $shell_file
    echo "Rscript $R_merge_final_consmat" >> $shell_file

    chmod +x $shell_file
    if command -v qsub &>/dev/null; then
        echo "Adding To Queue: $shell_file"
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
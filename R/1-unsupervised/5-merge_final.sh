#!/bin/bash

. ./Parameters.sh

##################################################
########## Execute R scripts for merge ###########
##################################################
file_to_submit=()
for dataset in "${dataSets[@]}"; do
    # get R scripts
    mkdir -p $workDir/R_file/merge/$dataset
    mkdir -p $workDir/sh_file/merge/$dataset

    R_merge_final_clust=$workDir/R_file/merge/$dataset/Merge_final_clust.R
    R_merge_final_consmat=$workDir/R_file/merge/$dataset/Merge_final_consmat.R

    mkdir -p $outputDir'/unsupervised/merge/data_pr_'$dataset

    # Create R scripts
    echo 'dataset <- "'$dataset'"' > $R_merge_final_clust
    echo 'outputdir <- "'$outputDir'"' >> $R_merge_final_clust
    echo 'algs <- strsplit("'${algs[@]}'", " ")[[1]]' >> $R_merge_final_clust
    echo 'reps <- '$reps >> $R_merge_final_clust
    echo 'k <-'$k >> $R_merge_final_clust
    echo 'shouldCompute <- '$shouldCompute >> $R_merge_final_clust
    echo 'source("R/1-unsupervised/5-merge_clust.R")' >> $R_merge_final_clust

    # Create sh scirpts
    echo 'dataset <- "'$dataset'"' > $R_merge_final_consmat
    echo 'outputdir <- "'$outputDir'"' >> $R_merge_final_consmat
    echo 'algs <- strsplit("'${algs[@]}'", " ")[[1]]' >> $R_merge_final_consmat
    echo 'shouldCompute <- '$shouldCompute >> $R_merge_final_consmat
    echo 'source("R/1-unsupervised/5-merge_complete_consmat.R")' >> $R_merge_final_consmat

    shell_file=$workDir/sh_file/merge/$dataset/merge_final.sh

    echo "echo 'merge_final_clust'" > $shell_file
    echo "Rscript $R_merge_final_clust" >> $shell_file
    echo "echo 'merge_final_consmat'" >> $shell_file
    echo "Rscript $R_merge_final_consmat" >> $shell_file

    chmod +x $shell_file

    if command -v qsub &>/dev/null; then
        file_to_submit+=($shell_file)
        echo -e "$GREEN_TICK Added to queue: $shell_file"
    else
        bash $shell_file
    fi
done

logDir=$baseLogDir'/unsupervised/merge'
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh
fi

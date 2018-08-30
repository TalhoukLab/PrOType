#!/bin/bash

. ./Parameters.sh


##################################################
########## Execute R scripts for merge ###########
##################################################
file_to_submit=()
for dataset in "${dataSets[@]}"; do

    mkdir -p $workDir/R_file/eval/$dataset
    mkdir -p $workDir/sh_file/eval/$dataset

    R_eval=$workDir/R_file/eval/$dataset/eval_run.R
    shell_file=$workDir/sh_file/eval/$dataset/eval_run.sh

    mkdir -p $outputDir'/unsupervised/final/'$dataset

    # create R script
    echo "outputdir <- '"$outputDir"'" > $R_eval
    echo "dataset <- '"$dataset"'" >> $R_eval
    echo "referenceClass <- '"$referenceClass"'" >> $R_eval
    echo 'shouldCompute <- '$shouldCompute >> $R_eval
    echo "source('R/1-unsupervised/7-final_clust.R')" >> $R_eval

    # execute R scripts
    echo "Rscript $R_eval" > $shell_file
    chmod +x $shell_file

    if command -v qsub &>/dev/null; then
        file_to_submit+=($shell_file)
        echo -e "$GREEN_TICK Added to queue: $shell_file"
    else
        bash $shell_file
    fi
done

logDir=$baseLogDir'/unsupervised/finalClust'
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh
fi

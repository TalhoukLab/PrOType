#!/bin/bash

. ./Parameters.sh

file_to_submit=()
for dataset in "${dataSets[@]}"; do

    data_sets=${dataset// /'"','"'}

    # create R script
    mkdir -p $workDir/R_file/CIsuplearn/$dataset
    mkdir -p $workDir/sh_file/CIsuplearn/$dataset

    Rname=$workDir/R_file/CIsuplearn/$dataset/CIsupLearn.R
    shell_file=$workDir/sh_file/CIsuplearn/$dataset/CIsupLearn.sh

    mkdir -p $outputDir'/supervised/ci_sup_lrn/'

    echo 'outputDir <- "'$outputDir'"' > $Rname
    echo 'fdat <- c("'$data_sets'")' >> $Rname
    echo "top <- $top" >> $Rname
    echo 'source("R/2-supervised/4-CIsupLearn.R")' >> $Rname

    # Run Script
    echo "Rscript $Rname" > $shell_file
    chmod +x $shell_file

    if command -v qsub &>/dev/null; then
        file_to_submit+=($shell_file)
        echo -e "$GREEN_TICK Added to queue: $shell_file"
    else
        bash $shell_file
    fi
done

shouldWait=FALSE
logDir=$baseLogDir'/supervised/CIsupLearn'
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh
fi

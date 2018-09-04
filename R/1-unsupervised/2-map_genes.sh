#!/bin/bash

. ./Parameters.sh

#************************************************
#************* create mapping scripts ***********
#************************************************
file_to_submit=()
for dataset in "${dataSets[@]}"; do

    mkdir -p $workDir/R_file/map/$dataset
    mkdir -p $workDir/sh_file/map/$dataset

    Rname=$workDir/R_file/map/$dataset/map.R
    shell_file=$workDir/sh_file/map/$dataset/map.sh

    mkdir -p $outputDir'/unsupervised/map_genes/'$dataset

    # create R script
    echo "source('R/1-unsupervised/2-map_genes.R')" > $Rname
    echo "dataset <- '"$dataset"'" >> $Rname
    echo "outputDir <- '"$outputDir"'" >> $Rname
    echo 'shouldCompute <- '$shouldCompute >> $Rname
    echo "map_to_nano(dataset, outputDir, shouldCompute)" >> $Rname

    # Run Script
    echo "Rscript $Rname" > $shell_file
    chmod +x $shell_file

    if command -v qsub &>/dev/null; then
        file_to_submit+=($shell_file)
        echo -e "$GREEN_TICK Added to queue: $shell_file"
    fi
done

shouldWait=FALSE
logDir=$baseLogDir'/unsupervised/map_genes'
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh
fi

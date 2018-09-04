#!/bin/bash

. ./Parameters.sh

#************************************************
#************* create mapping scripts ***********
#************************************************
file_to_submit=()
for dataset in "${dataSets[@]}"; do

    mkdir -p $workDir/R_file/map/$dataset
    mkdir -p $workDir/sh_file/map/$dataset
    mkdir -p $outputDir'/unsupervised/map_genes/'$dataset

    # Content of R file
    Rname=$workDir/R_file/map/$dataset/map.R
    echo "dataset <- '"$dataset"'" > $Rname
    echo "inputDir <- file.path('"$outputDir"', '"unsupervised"', '"prep_data"', '"$dataSet"')" >> $Rname
    echo "outputDir <- file.path('"$outputDir"', '"unsupervised"', '"map_genes"', '"$dataSet"')" >> $Rname
    echo 'shouldCompute <- '$shouldCompute >> $Rname
    echo "source('R/1-unsupervised/2-map_genes.R')" >> $Rname
    echo "map_to_nano(dataset, inputDir, outputDir, shouldCompute)" >> $Rname

    # Content of sh file
    shell_file=$workDir/sh_file/map/$dataset/map.sh
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

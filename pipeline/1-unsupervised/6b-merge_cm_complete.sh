#!/bin/bash

. ./Parameters.sh

file_to_submit=()

# Make directories for R script, shell script
subDir=unsupervised/data_pr
RSubDir=$RDir/$subDir
shSubDir=$shDir/$subDir

for dataset in "${dataSets[@]}"; do
    # Make job and output directories for dataset
    mkdir -p $RSubDir/$dataset
    mkdir -p $shSubDir/$dataset
    mkdir -p $outputDir/$subDir/$dataset

    R_file=$RSubDir/$dataset/merge_cm.R
    echo 'dataset <- "'$dataset'"' > $R_file
    echo 'outputDir <- "'$outputDir'"' >> $R_file
    echo 'algs <- strsplit("'${algs[@]}'", " ")[[1]]' >> $R_file
    echo 'shouldCompute <- '$shouldCompute >> $R_file
    echo 'source("pipeline/1-unsupervised/6b-merge_cm_complete.R")' >> $R_file

    # Content of sh file
    sh_file=$shSubDir/$dataset/merge_cm.sh
    echo "Rscript $R_file" > $sh_file
    chmod +x $sh_file

    # Add to queue if qsub exists
    if command -v qsub &>/dev/null; then
        file_to_submit+=($sh_file)
        echo -e "$GREEN_TICK Added to queue: $sh_file"
    else
        bash $sh_file
    fi
done

# Submit to queue if qsub exists
logDir=$logDir/$subDir
outputDir=$outputDir/$subDir
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh
fi

#!/bin/bash

. ./Parameters.sh

file_to_submit=()

# Make directories for R script, shell script
subDir=supervised/top_ci
R_dir=$workDir/R_file/$subDir
sh_dir=$workDir/sh_file/$subDir

for dataset in "${dataSets[@]}"; do
    # Make job and output directories for dataset
    mkdir -p $R_dir/$dataset
    mkdir -p $sh_dir/$dataset
    mkdir -p $outputDir/$subDir

    # Content of R file
    R_file=$R_dir/$dataset/top_ci.R
    echo 'outputDir <- "'$outputDir'"' > $R_file
    echo 'fdat <- "'$dataset'"' >> $R_file
    echo "top <- $top" >> $R_file
    echo 'source("pipeline/2-supervised/3-top_ci.R")' >> $R_file

    # Content of sh file
    sh_file=$sh_dir/$dataset/top_ci.sh
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
shouldWait=FALSE
logDir=$baseLogDir/$subDir
outputDir=$outputDir/$subDir
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh
fi

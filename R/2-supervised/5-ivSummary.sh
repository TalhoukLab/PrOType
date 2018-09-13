#!/bin/bash

. ./Parameters.sh

file_to_submit=()

# Make directories for R script, shell script
subDir=supervised/iv_summary
R_dir=$workDir/R_file/$subDir
sh_dir=$workDir/sh_file/$subDir

for dataset in "${dataSets[@]}"; do
    # Make job and output directories for dataset
    mkdir -p $R_dir/$dataset
    mkdir -p $sh_dir/$dataset
    mkdir -p $outputDir/$subDir

    # Content of R file
    R_file=$R_dir/$dataset/iv_summary.R
    echo 'outputDir <- "'$outputDir'"' > $R_file
    echo 'dataset <- "'$dataset'"' >> $R_file
    echo 'source("R/2-supervised/5-ivSummary.R")' >> $R_file

    # Content of sh file
    sh_file=$sh_dir/$dataset/iv_summary.sh
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
logDir=$baseLogDir/$subDir
outputDir2=$outputDir
outputDir=$outputDir/$subDir
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh
fi
outputDir=$outputDir2

# Combine all IV summaries
R_file=$R_dir/iv_combine.R
echo 'outputDir <- "'$outputDir'"' > $R_file
echo "source('R/2-supervised/5-ivCombine.R')" >> $R_file
Rscript $R_file

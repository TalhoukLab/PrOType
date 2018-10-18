#!/bin/bash

. ./Parameters.sh

# Normalization type
if [ "$normalizeBy" = "Genes" ]; then
    model='Model-hc-genes'
elif [ "$normalizeBy" = "Samples" ]; then
    model='Model-hc-samples'
elif [ "$normalizeBy" = "None" ]; then
    model='Model-hc'
else
    echo "A normalization of type Genes, Samples or None must be specified"
    exit 1
fi

file_to_submit=()

# Make directories for R script, shell script
subDir=supervised/summary
R_dir=$workDir/R_file/$subDir
sh_dir=$workDir/sh_file/$subDir

for dataset in "${dataSets[@]}"; do
    # Make job and output directories for dataset
    mkdir -p $R_dir/$dataset
    mkdir -p $sh_dir/$dataset
    mkdir -p $outputDir/$subDir/$dataset

    # Content of R file
    R_file=$R_dir/$dataset/iv_summary.R
    echo 'outputDir <- "'$outputDir'"' > $R_file
    echo 'dataset <- "'$dataset'"' >> $R_file
    echo 'model <- "'$model'"' >> $R_file
    echo 'source("R/2-supervised/4-iv_summary.R")' >> $R_file

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
outputDir=$outputDir/$subDir
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh
fi

# Combine all IV summaries
R_file=$R_dir/iv_combine.R
echo 'outputDir <- "'$(dirname $(dirname $outputDir))'"' > $R_file
echo 'source("R/2-supervised/5-combine_ivs.R")' >> $R_file
Rscript $R_file

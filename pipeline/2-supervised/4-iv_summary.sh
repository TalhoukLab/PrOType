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
RSubDir=$RDir/$subDir
shSubDir=$shDir/$subDir

for dataset in "${dataSets[@]}"; do
    # Make job and output directories for dataset
    mkdir -p $RSubDir/$dataset
    mkdir -p $shSubDir/$dataset
    mkdir -p $outputDir/$subDir/$dataset

    # Content of R file
    R_file=$RSubDir/$dataset/iv_summary.R
    echo 'outputDir <- "'$outputDir'"' > $R_file
    echo 'dataset <- "'$dataset'"' >> $R_file
    echo 'model <- "'$model'"' >> $R_file
    echo 'source("pipeline/2-supervised/4-iv_summary.R")' >> $R_file

    # Content of sh file
    sh_file=$shSubDir/$dataset/iv_summary.sh
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

# Combine all IV summaries
R_file=$RSubDir/iv_combine.R
echo 'outputDir <- "'$(dirname $(dirname $outputDir))'"' > $R_file
echo 'source("pipeline/2-supervised/5-combine_ivs.R")' >> $R_file
Rscript $R_file

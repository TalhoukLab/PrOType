#!/bin/bash

. ./Parameters.sh

file_to_submit=()

# Make directories for R script, shell script
subDir=post_processing
RSubDir=$RDir/$subDir
shSubDir=$shDir/$subDir
mkdir -p $RSubDir
mkdir -p $shSubDir
mkdir -p $outputDir/$subDir/evals
mkdir -p $outputDir/$subDir/plots

trainSets=($trainSet $trainSet2)
for dataset in "${trainSets[@]}"; do
    # Content of R file
    R_file=$RSubDir/batch_effects_$dataset.R
    echo 'dataset <- "'$dataset'"' > $R_file
    echo 'outputDir <- "'$outputDir'"' >> $R_file
    echo 'dataDir <- "data"' >> $R_file
    echo 'source("pipeline/3-post_processing/2-evaluate_batch_effects.R")' >> $R_file

    # Content of sh file
    sh_file=$shSubDir/batch_effects_$dataset.sh
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
outputDir="$outputDir/$subDir/evals $outputDir/$subDir/plots"
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh
fi

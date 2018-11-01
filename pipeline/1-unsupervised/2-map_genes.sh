#!/bin/bash

. ./Parameters.sh

file_to_submit=()

# Make directories for R script, shell script
subDir=unsupervised/map_genes
RSubDir=$RDir/$subDir
shSubDir=$shDir/$subDir

for dataset in "${dataSets[@]}"; do
    # Make job and output directories for dataset
    mkdir -p $RSubDir/$dataset
    mkdir -p $shSubDir/$dataset
    mkdir -p $outputDir/$subDir/$dataset

    # Content of R file
    R_file=$RSubDir/$dataset/map_genes.R
    echo "dataset <- '"$dataset"'" > $R_file
    echo "inputDir <- file.path('"$outputDir"', '"unsupervised"', '"prep_data"', dataset)" >> $R_file
    echo "outputDir <- file.path('"$outputDir"', '"unsupervised"', '"map_genes"', dataset)" >> $R_file
    echo 'shouldCompute <- '$shouldCompute >> $R_file
    echo 'source("pipeline/1-unsupervised/2-map_genes.R")' >> $R_file
    echo "map_to_nano(dataset, inputDir, outputDir, shouldCompute)" >> $R_file

    # Content of sh file
    sh_file=$shSubDir/$dataset/map_genes.sh
    echo "Rscript $R_file" > $sh_file
    chmod +x $sh_file

    # Add to queue if qsub exists
    if command -v qsub &>/dev/null; then
        file_to_submit+=($sh_file)
        echo -e "$GREEN_TICK Added to queue: $sh_file"
    fi
done

# Submit to queue if qsub exists
shouldWait=FALSE
logDir=$logDir/$subDir
outputDir=$outputDir/$subDir
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh
fi

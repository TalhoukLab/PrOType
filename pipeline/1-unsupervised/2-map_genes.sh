#!/bin/bash

. ./Parameters.sh

file_to_submit=()

# Make directories for R script, shell script
subDir=unsupervised/map_genes
R_dir=$workDir/R_file/$subDir
sh_dir=$workDir/sh_file/$subDir

for dataset in "${dataSets[@]}"; do
    # Make job and output directories for dataset
    mkdir -p $R_dir/$dataset
    mkdir -p $sh_dir/$dataset
    mkdir -p $outputDir/$subDir/$dataset

    # Content of R file
    R_file=$R_dir/$dataset/map_genes.R
    echo "dataset <- '"$dataset"'" > $R_file
    echo "inputDir <- file.path('"$outputDir"', '"unsupervised"', '"prep_data"', dataset)" >> $R_file
    echo "outputDir <- file.path('"$outputDir"', '"unsupervised"', '"map_genes"', dataset)" >> $R_file
    echo 'shouldCompute <- '$shouldCompute >> $R_file
    echo 'source("pipeline/1-unsupervised/2-map_genes.R")' >> $R_file
    echo "map_to_nano(dataset, inputDir, outputDir, shouldCompute)" >> $R_file

    # Content of sh file
    sh_file=$sh_dir/$dataset/map_genes.sh
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
logDir=$baseLogDir/$subDir
outputDir=$outputDir/$subDir
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh
fi

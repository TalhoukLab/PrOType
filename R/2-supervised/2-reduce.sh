#!/bin/bash

. ./Parameters.sh

file_to_submit=()

# Normalization type
if [ "$normalizeBy" = "Genes" ]; then
    fname='Model-hc-genes'
elif [ "$normalizeBy" = "Samples" ]; then
    fname='Model-hc-samples'
elif [ "$normalizeBy" = "None" ]; then
    fname='Model-hc'
else
    echo "A normalization of type Genes, Samples or None must be specified"
    exit 1
fi

# Make directories for R script, shell script
subDir=supervised/reduce
R_dir=$workDir/R_file/$subDir
sh_dir=$workDir/sh_file/$subDir

for dataset in "${dataSets[@]}"; do
    # Make job and output directories for dataset
    mkdir -p $R_dir/$dataset
    mkdir -p $sh_dir/$dataset
    mkdir -p $outputDir/$subDir/$fname'_'$dataset

    for i in "${supervisedAlgs[@]}"; do
        # Content of R file
        R_file=$R_dir/$dataset/reduce_$i.R
        echo 'outputDir <- "'$outputDir'"' > $R_file
        echo 'dataset <- "'$dataset'"' >> $R_file
        echo 'alg <- "'$i'"' >> $R_file
        echo 'fname <- "'$fname'"' >> $R_file
        echo 'threshold <- "'$threshold'"' >> $R_file
        echo 'source("R/2-supervised/2-reduce.R")' >> $R_file
        echo 'reduce_supervised(dataset, alg, outputDir, fname, threshold)' >> $R_file

        # Content of sh file
        sh_file=$sh_dir/$dataset/reduce_$i.sh
        echo 'Rscript' $R_file > $sh_file
        chmod +x $sh_file

        # Add to queue if qsub exists
        if command -v qsub &>/dev/null; then
            file_to_submit+=($sh_file)
            echo -e "$GREEN_TICK Added to queue: $sh_file"
        else
            bash $sh_file
        fi
    done
done

# Submit to queue if qsub exists
logDir=$baseLogDir/$subDir
outputDir=$outputDir/$subDir
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh
fi

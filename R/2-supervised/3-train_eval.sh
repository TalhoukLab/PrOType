#!/bin/bash

. ./Parameters.sh

# Normalization type
if [ "$normalizeBy" = "Genes" ]; then
    mname='Model-hc-genes'
elif [ "$normalizeBy" = "Samples" ]; then
    mname='Model-hc-samples'
elif [ "$normalizeBy" = "None" ]; then
    mname='Model-hc'
else
    echo "A normalization of type Genes, Samples or None must be specified"
    exit 1
fi

# Make directories for R script, shell script
subDir=supervised/train_eval
R_dir=$workDir/R_file/$subDir
sh_dir=$workDir/sh_file/$subDir

for dataset in "${dataSets[@]}"; do
    # Make job and output directories for dataset
    mkdir -p $R_dir/$dataset
    mkdir -p $sh_dir/$dataset
    mkdir -p $outputDir/$subDir

    # Content of R file
    R_file=$R_dir/$dataset/train_eval.R
    echo 'outputDir <- "'$outputDir'"' > $R_file
    echo 'dataset <- "'$dataset'"' >> $R_file
    echo 'mname <- "'$mname'"' >> $R_file
    echo 'algs <- strsplit("'${supervisedAlgs[@]}'", " ")[[1]]' >> $R_file
    echo 'source("R/2-supervised/3-train_eval.R")' >> $R_file

    # Content of sh file
    sh_file=$sh_dir/$dataset/train_eval.sh
    echo "Rscript $R_file" > $sh_file
    chmod +x $sh_file

    # Add to queue if qsub exists
    if command -v qsub &>/dev/null; then
        file_to_submit+=($sh_file)
        echo -e "$GREEN_TICK Added to queue: $sh_file"
    else
        bash $shell_file
    fi
done

# Submit to queue if qsub exists
logDir=$baseLogDir/$subDir
outputDir=$outputDir/$subDir
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh
fi

#!/bin/bash

. ./Parameters.sh

file_to_submit=()

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

# Make directories for R script, shell script
subDir=supervised/train_eval
RSubDir=$RDir/$subDir
shSubDir=$shDir/$subDir

for dataset in "${dataSets[@]}"; do
    # Make job and output directories for dataset
    mkdir -p $RSubDir/$dataset
    mkdir -p $shSubDir/$dataset
    mkdir -p $outputDir/$subDir/$dataset

    for s in $(seq -f "%0${#supervised_reps}g" 1 $supervised_reps); do
        for alg in "${supervisedAlgs[@]}"; do
            # Content of R file
            R_file=$RSubDir/$dataset/$alg$s.R
            echo 'dataset <- "'$dataset'"' > $R_file
            echo 'reps <- "'$s'"' >> $R_file
            echo 'algs <- "'$alg'"' >> $R_file
            echo 'inDir <- "'$outputDir$dataset'"' >> $R_file
            echo 'outDir <- "'$outputDir'"' >> $R_file
            echo 'normalizeBy <- "'$normalizeBy'"' >> $R_file
            echo "minVar <- '$minVar'" >> $R_file
            echo 'normType <- "'$normType'"' >> $R_file
            echo 'model <- "'$model'"' >> $R_file
            echo 'threshold <- '$threshold >> $R_file
            echo 'shouldCompute <- '$shouldCompute >> $R_file
            echo 'source("pipeline/2-supervised/1-train_eval.R")' >> $R_file
            echo 'train_supervised(dataset, algs, reps, inDir, outDir, normalizeBy, minVar, threshold, normType, model, shouldCompute)' >> $R_file

            # Content of sh file
            sh_file=$shSubDir/$dataset/$alg$s.sh
            echo 'Rscript' $R_file > $sh_file
            chmod +x $sh_file

            # Add to queue if qsub exists
            if command -v qsub &>/dev/null; then
                file_to_submit+=($sh_file)
                echo -e "$GREEN_TICK Added to queue: $sh_file"
            fi
        done
    done

    if command -v qsub &>/dev/null; then
        :
    else
        python assets/submit_local.py --num_parallel 4 --file_location $scriptDir$dataset --step train
    fi
done

# Submit to queue if qsub exists
logDir=$logDir/$subDir
outputDir=$outputDir/$subDir
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh
fi

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
R_dir=$workDir/R_file/$subDir
sh_dir=$workDir/sh_file/$subDir

for dataset in "${dataSets[@]}"; do
    # Make job and output directories for dataset
    mkdir -p $R_dir/$dataset
    mkdir -p $sh_dir/$dataset
    mkdir -p $outputDir/$subDir/$dataset

    for s in `seq 1 $supervised_reps`; do
        for i in "${supervisedAlgs[@]}"; do
            # Content of R file
            R_file=$R_dir/$dataset/$i$s.R
            echo 'dataset <- "'$dataset'"' > $R_file
            echo 'reps <- '$s >> $R_file
            echo 'algs <- "'$i'"' >> $R_file
            echo 'inDir <- "'$outputDir$dataset'"' >> $R_file
            echo 'outDir <- "'$outputDir'"' >> $R_file
            echo 'normalizeBy <- "'$normalizeBy'"' >> $R_file
            echo "minVar <- '$minVar'" >> $R_file
            echo 'normType <- "'$normType'"' >> $R_file
            echo 'model <- "'$model'"' >> $R_file
            echo 'threshold <- '$threshold >> $R_file
            echo 'shouldCompute <- '$shouldCompute >> $R_file
            echo 'source("R/2-supervised/1-train_eval.R")' >> $R_file
            echo 'train_supervised(dataset, algs, reps, inDir, outDir, normalizeBy, minVar, threshold, normType, model, shouldCompute)' >> $R_file

            # Content of sh file
            sh_file=$sh_dir/$dataset/$i$s.sh
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
        python assets/submit_local.py --num_parallel 4 --file_location $workDir$dataset --step train
    fi
done

# Submit to queue if qsub exists
logDir=$baseLogDir/$subDir
outputDir=$outputDir/$subDir
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh
fi

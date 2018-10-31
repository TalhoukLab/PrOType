#!/bin/bash

. ./Parameters.sh

file_to_submit=()

# Make directories for R script, shell script
subDir=unsupervised/merge_cm
R_dir=$scriptDir/R_file/$subDir
sh_dir=$scriptDir/sh_file/$subDir

for dataset in "${dataSets[@]}"; do
    # Make job and output directories for dataset
    mkdir -p $R_dir/$dataset
    mkdir -p $sh_dir/$dataset
    mkdir -p $outputDir/$subDir/$dataset

    n=$((reps / c)) # number of splits
    for r in $(seq -f "%0${#n}g" 1 $n); do
        for alg in "${algs[@]}"; do
            # Content of R file
            R_file=$R_dir/$dataset/merge_cm_$alg$r.R
            echo 'dataset <- "'$dataset'"' > $R_file
            echo 'alg <- "'$alg'"' >> $R_file
            echo 'reps <- '$reps >> $R_file
            echo 'c <- '$c >> $R_file
            echo 'r <- "'$r'"' >> $R_file
            echo 'k <- "'$k'"' >> $R_file
            echo 'shouldCompute <- '$shouldCompute >> $R_file
            echo 'outputDir <- "'$outputDir'"' >> $R_file
            echo 'source("pipeline/1-unsupervised/6a-merge_cm_partial.R")' >> $R_file

            # Content of sh file
            sh_file=$sh_dir/$dataset/merge_cm_$alg$r.sh
            echo "Rscript $R_file" > $sh_file
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
        python assets/submit_local.py --num_parallel 4 --file_location $scriptDir$dataset --step merge
    fi
done

# Submit to queue if qsub exists
logDir=$baseLogDir/$subDir
outputDir=$outputDir/$subDir
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh
fi

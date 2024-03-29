#!/bin/bash

. ./Parameters.sh

file_to_submit=()

# Make directories for R script, shell script
subDir=unsupervised/merge_cm
RSubDir=$RDir/$subDir
shSubDir=$shDir/$subDir

for dataset in "${dataSets[@]}"; do
    # Make job and output directories for dataset
    mkdir -p $RSubDir/$dataset
    mkdir -p $shSubDir/$dataset
    mkdir -p $outputDir/$subDir/$dataset

    n=$((reps / c)) # number of splits
    for r in $(seq -f "%0${#n}g" 1 $n); do
        for alg in "${algs[@]}"; do
            # Content of R file
            R_file=$RSubDir/$dataset/merge_cm_$alg$r.R
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
            sh_file=$shSubDir/$dataset/merge_cm_$alg$r.sh
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
logDir=$logDir/$subDir
outputDir=$outputDir/$subDir
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh
fi

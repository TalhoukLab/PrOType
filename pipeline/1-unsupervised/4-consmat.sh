#!/bin/bash

. ./Parameters.sh

file_to_submit=()

# Make directories for R script, shell script
subDir=unsupervised/consmat
RSubDir=$RDir/$subDir
shSubDir=$shDir/$subDir

for dataset in "${dataSets[@]}"; do
    # Make job and output directories for dataset
    mkdir -p $RSubDir/$dataset
    mkdir -p $shSubDir/$dataset
    mkdir -p $outputDir/$subDir/$dataset

    for s in $(seq -f "%0${#reps}g" 1 $reps); do
        for alg in "${algs[@]}"; do
            # Content of R file
            R_file=$RSubDir/$dataset/consmat_$alg$s.R
            echo 's <- "'$s'"' >> $R_file
            echo 'alg <- "'$alg'"' >> $R_file
            echo 'dataset <- "'$dataset'"' >> $R_file
            echo 'outputDir <- "'$outputDir'"' >> $R_file
            echo 'source("pipeline/1-unsupervised/4-consmat.R")' >> $R_file

            # Content of sh file
            sh_file=$shSubDir/$dataset/consmat_$alg$s.sh
            echo "Rscript $R_file" > $sh_file
            chmod +x $sh_file

            # Add to queue if qsub exists
            if command -v qsub &>/dev/null; then
                file_to_submit+=($sh_file)
                echo -e "$GREEN_TICK Added to queue: $sh_file"
            fi
        done
    done
done

# Submit to queue if qsub exists
logDir=$logDir/$subDir
outputDir=$outputDir/$subDir
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh
fi

#!/bin/bash

. ./Parameters.sh

file_to_submit=()

# Make directories for R script, shell script
subDir=unsupervised/consmat
R_dir=$workDir/R_file/$subDir
sh_dir=$workDir/sh_file/$subDir

for dataset in "${dataSets[@]}"; do
    # Make job and output directories for dataset
    mkdir -p $R_dir/$dataset
    mkdir -p $sh_dir/$dataset
    mkdir -p $outputDir/$subDir/$dataset

    for s in $(seq -f "%0${#reps}g" 1 $reps); do
        for alg in "${algs[@]}"; do
            # Content of R file
            R_file=$R_dir/$dataset/consmat_$alg$s.R
            echo 's <- "'$s'"' >> $R_file
            echo 'alg <- "'$alg'"' >> $R_file
            echo 'dataset <- "'$dataset'"' >> $R_file
            echo 'outputDir <- "'$outputDir'"' >> $R_file
            echo 'source("R/1-unsupervised/4-consmat.R")' >> $R_file

            # Content of sh file
            sh_file=$sh_dir/$dataset/consmat_$alg$s.sh
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
logDir=$baseLogDir/$subDir
outputDir=$outputDir/$subDir
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh
fi

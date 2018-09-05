#!/bin/bash

. ./Parameters.sh

file_to_submit=()

# Make directories for R script, shell script
subDir=unsupervised/merge_consmat
R_dir=$workDir/R_file/$subDir
sh_dir=$workDir/sh_file/$subDir

for dataset in "${dataSets[@]}"; do
    # Make job and output directories for dataset
    mkdir -p $R_dir/$dataset
    mkdir -p $sh_dir/$dataset
    mkdir -p $outputDir/$subDir'/con_mat_merged_'$dataset

    for r in `seq 1 $((reps / c))`; do
        for i in "${algs[@]}"; do
            # Zero-padded iteration of r * c using # digits of reps
            printf -v s "%0${#reps}d" $((r * c))

            # Content of R file
            R_file=$R_dir/$dataset/merge_$i$s.R
            echo 'dataset <- "'$dataset'"' > $R_file
            echo 'algs <- "'$i'"' >> $R_file
            echo 'c <- '$c >> $R_file
            echo 'r <- '$r >> $R_file
            echo 'reps <- '$reps >> $R_file
            echo 'k <- "'$k'"' >> $R_file
            echo 'shouldCompute <- '$shouldCompute >> $R_file
            echo 'outputDir <- "'$outputDir'"' >> $R_file
            echo 'source("R/1-unsupervised/4-merge_partial_consmat.R")' >> $R_file

            # Content of sh file
            sh_file=$sh_dir/$dataset/merge_$i$s.sh
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
      python assets/submit_local.py --num_parallel 4 --file_location $workDir$dataset --step merge
    fi
done

# Submit to queue if qsub exists
logDir=$baseLogDir/$subDir
outputDir=$outputDir/$subDir
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh
fi

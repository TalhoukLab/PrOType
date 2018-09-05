#!/bin/bash

. ./Parameters.sh

file_to_submit=()

# Make directories for R script, shell script
subDir=unsupervised/merge
R_dir=$workDir/R_file/$subDir
sh_dir=$workDir/sh_file/$subDir

for dataset in "${dataSets[@]}"; do
    # Make job and output directories for dataset
    mkdir -p $R_dir/$dataset
    mkdir -p $sh_dir/$dataset
    mkdir -p $outputDir/$subDir'/data_pr_'$dataset

    # Content of R files
    R_file1=$R_dir/$dataset/merge_final_clust.R
    echo 'dataset <- "'$dataset'"' > $R_file1
    echo 'outputdir <- "'$outputDir'"' >> $R_file1
    echo 'algs <- strsplit("'${algs[@]}'", " ")[[1]]' >> $R_file1
    echo 'reps <- '$reps >> $R_file1
    echo 'k <-'$k >> $R_file1
    echo 'shouldCompute <- '$shouldCompute >> $R_file1
    echo 'source("R/1-unsupervised/5-merge_clust.R")' >> $R_file1

    R_file2=$R_dir/$dataset/merge_final_consmat.R
    echo 'dataset <- "'$dataset'"' > $R_file2
    echo 'outputdir <- "'$outputDir'"' >> $R_file2
    echo 'algs <- strsplit("'${algs[@]}'", " ")[[1]]' >> $R_file2
    echo 'shouldCompute <- '$shouldCompute >> $R_file2
    echo 'source("R/1-unsupervised/5-merge_complete_consmat.R")' >> $R_file2

    # Content of sh file
    sh_file=$sh_dir/$dataset/merge_final.sh
    echo "Rscript $R_file1" >> $sh_file
    echo "Rscript $R_file2" >> $sh_file
    chmod +x $sh_file

    # Add to queue if qsub exists
    if command -v qsub &>/dev/null; then
        file_to_submit+=($sh_file)
        echo -e "$GREEN_TICK Added to queue: $sh_file"
    else
        bash $sh_file
    fi
done

# Submit to queue if qsub exists
logDir=$baseLogDir/$subDir
outputDir=$outputDir/$subDir
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh
fi

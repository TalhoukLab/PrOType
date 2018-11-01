#!/bin/bash

. ./Parameters.sh

file_to_submit=()

# Make directories for R script, shell script
subDir=unsupervised/consensus
RSubDir=$RDir/$subDir
shSubDir=$shDir/$subDir

for dataset in "${dataSets[@]}"; do
    # Make job and output directories for dataset
    mkdir -p $RSubDir/$dataset
    mkdir -p $shSubDir/$dataset
    mkdir -p $outputDir/$subDir/$dataset

    for i in "${cons[@]}"; do
        # Content of R file
        R_file=$RSubDir/$dataset/cons_$i.R
        echo 'outputDir <- "'$outputDir'"' > $R_file
        echo 'dataset <- "'$dataset'"' >> $R_file
        echo 'cons.funs <- "'$i'"'>> $R_file
        echo "k <- $k" >> $R_file
        echo 'shouldCompute <- '$shouldCompute >> $R_file
        echo 'source("pipeline/1-unsupervised/7-con_fun.R")' >> $R_file

        # Content of sh file
        sh_file=$shSubDir/$dataset/cons_$i.sh
        echo "Rscript $R_file" > $sh_file
        chmod +x $sh_file

        # Add to queue if qsub exists
        if command -v qsub &>/dev/null; then
            file_to_submit+=($sh_file)
            echo -e "$GREEN_TICK Added to queue: $sh_file"
        else
            bash $sh_file
        fi
    done

    if command -v qsub &>/dev/null; then
        :
    else
      python assets/submit_local.py --num_parallel 4 --file_location $scriptDir$dataset --step consensus
    fi
done

# Submit to queue if qsub exists
logDir=$logDir/$subDir
outputDir=$outputDir/$subDir
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh
fi

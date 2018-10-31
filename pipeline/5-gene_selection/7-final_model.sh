#!/bin/bash

. ./Parameters.sh

file_to_submit=()

# Make directories for R script, shell script
subDir=gene_selection/final_model
R_dir=$scriptDir/R_file/$subDir
sh_dir=$scriptDir/sh_file/$subDir
mkdir -p $R_dir
mkdir -p $sh_dir
mkdir -p $outputDir/$subDir

# Content of R file
R_file=$R_dir/final_model.R
echo 'outputDir <- "'$outputDir'"' > $R_file
echo 'source("pipeline/5-gene_selection/7-final_model.R")' >> $R_file

# Content of sh file
sh_file=$sh_dir/final_model.sh
echo 'Rscript' $R_file > $sh_file
chmod +x $sh_file

# Add to queue if qsub exists, submit by python otherwise
if command -v qsub &>/dev/null; then
  file_to_submit+=($sh_file)
  echo -e "$GREEN_TICK Added to queue: $sh_file"
fi

# Submit to queue if qsub exists, to python otherwise
logDir=$logDir/$subDir
outputDir=$outputDir/$subDir
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh
else
    python assets/submit_local.py --num_parallel 4 --file_location $scriptDir --step build_final_model
fi

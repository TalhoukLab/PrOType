#!/bin/bash

. ./Parameters.sh

file_to_submit=()

# Make directories for R script, shell script
subDir=gene_selection/retrain
RSubDir=$RDir/$subDir
shSubDir=$shDir/$subDir
mkdir -p $RSubDir
mkdir -p $shSubDir
mkdir -p $outputDir/$subDir
mkdir -p $outputDir/gene_selection/plots

# Content of R file
R_file=$RSubDir/retrain.R
echo 'outputDir <- "'$outputDir'"' > $R_file
echo 'source("pipeline/5-gene_selection/6-retrain.R")' >> $R_file

# Content of sh file
sh_file=$shSubDir/retrain.sh
echo 'Rscript' $R_file > $sh_file
chmod +x $sh_file

# Add to queue if qsub exists
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
    python assets/submit_local.py --num_parallel 4 --file_location $scriptDir --step training_part2
fi

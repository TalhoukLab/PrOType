#!/bin/bash

. ./Parameters.sh

# Make directories for R script, shell script
subDir=cross_platform/predictions
R_dir=$scriptDir/R_file/$subDir
sh_dir=$scriptDir/sh_file/$subDir
mkdir -p $R_dir
mkdir -p $sh_dir
mkdir -p $outputDir/$subDir

# Content of R file
R_file=$R_dir/cp_predictions.R
echo 'outputDir <- "'$outputDir'"' > $R_file
echo 'source("pipeline/6-cross_platform/2-cp_predictions.R")' >> $R_file

# Content of sh file
sh_file=$sh_dir/cp_predictions.sh
echo "Rscript $R_file" > $sh_file
chmod +x $sh_file

# Add to queue if qsub exists
if command -v qsub &>/dev/null; then
  file_to_submit=($sh_file)
  echo -e "$GREEN_TICK Added to queue: $sh_file"
else
  bash $shname
fi

# Submit to queue if qsub exists
logDir=$baseLogDir/$subDir
outputDir=$outputDir/$subDir
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh
fi

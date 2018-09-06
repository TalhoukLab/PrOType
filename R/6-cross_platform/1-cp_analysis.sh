#!/bin/bash

. ./Parameters.sh

# Make directories for R script, shell script
subDir=cross_platform/analysis
R_dir=$workDir/R_file/$subDir
sh_dir=$workDir/sh_file/$subDir
mkdir -p $R_dir
mkdir -p $sh_dir
mkdir -p $outputDir/$subDir

# Content of R file
R_file=$R_dir/cp_analysis.R
echo 'outputDir <- "'$outputDir'"' > $R_file
echo "source('R/6-cross_platform/1-cp_analysis.R')" >> $R_file

# Content of sh file
sh_file=$sh_dir/cp_analysis.sh
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

#!/bin/bash

. ./Parameters.sh

# Make directories for R script, shell script
subDir=cross_platform/analysis
RSubDir=$RDir/$subDir
shSubDir=$shDir/$subDir
mkdir -p $RSubDir
mkdir -p $shSubDir
mkdir -p $outputDir/$subDir

# Content of R file
R_file=$RSubDir/cp_analysis.R
echo 'outputDir <- "'$outputDir'"' > $R_file
echo 'source("pipeline/6-cross_platform/1-cp_analysis.R")' >> $R_file

# Content of sh file
sh_file=$shSubDir/cp_analysis.sh
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
logDir=$logDir/$subDir
outputDir=$outputDir/$subDir
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh
fi

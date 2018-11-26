#!/bin/bash

. ./Parameters.sh

# Make directories for R script, shell script
subDir=nanostring
RSubDir=$RDir/$subDir
shSubDir=$shDir/$subDir
mkdir -p $RSubDir
mkdir -p $shSubDir
mkdir -p $outputDir/$subDir/evals
mkdir -p $outputDir/$subDir/predictions

# Content of R file
R_file=$RSubDir/validate_nanostring.R
echo 'outputDir <- "'$outputDir'"' > $R_file
echo 'trainSet <- "'$trainSet'"' >> $R_file
echo 'cli::cat_line("NanoString classifier 1: validating overlap NanoString")' >> $R_file
echo 'source("pipeline/4-nanostring_classifier/1-validate_nanostring.R")' >> $R_file

# Content of sh file
sh_file=$shSubDir/validate_nanostring.sh
echo "Rscript $R_file" > $sh_file
chmod +x $sh_file
Rscript $R_file

# Add to queue if qsub exists
if command -v qsub &>/dev/null; then
  file_to_submit+=($sh_file)
  echo -e "$GREEN_TICK Added to queue: $sh_file"
fi

# Submit to queue if qsub exists
logDir=$logDir/$subDir
outputDir="$outputDir/nanostring/evals $outputDir/nanostring/predictions"
if command -v qsub &>/dev/null; then
  . ./assets/submit_queue.sh
fi

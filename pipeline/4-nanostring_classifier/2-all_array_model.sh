#!/bin/bash

. ./Parameters.sh

# Make directories for R script, shell script
subDir=nanostring/predictions
RSubDir=$RDir/$subDir
shSubDir=$shDir/$subDir
mkdir -p $RSubDir
mkdir -p $shSubDir
mkdir -p $outputDir/$subDir

# Content of R file
R_file=$RSubDir/all_array_model.R
echo 'outputDir <- "'$outputDir'"' > $R_file
echo 'trainSet <- "'$trainSet'"' >> $R_file
echo 'cli::cat_line("NanoString classifier 2: predicting full NanoString data using all-array model")' >> $R_file
echo 'source("pipeline/4-nanostring_classifier/2-all_array_model.R")' >> $R_file

# Content of sh file
sh_file=$shSubDir/all_array_model.sh
echo "Rscript $R_file" > $sh_file
chmod +x $sh_file

# Add to queue if qsub exists
if command -v qsub &>/dev/null; then
  file_to_submit+=($sh_file)
  echo -e "$GREEN_TICK Added to queue: $sh_file"
fi

# Submit to queue if qsub exists
logDir=$logDir/$subDir
outputDir=$outputDir/$subDir
if command -v qsub &>/dev/null; then
  . ./assets/submit_queue.sh
fi

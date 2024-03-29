#!/bin/bash

. ./Parameters.sh

# Make directories for R script, shell script
subDir=nanostring
RSubDir=$RDir/$subDir
shSubDir=$shDir/$subDir
mkdir -p $RSubDir
mkdir -p $shSubDir
mkdir -p $outputDir/$subDir/predictions

# Content of R file
R_file=$RSubDir/tcga_model.R
echo 'outputDir <- "'$outputDir'"' > $R_file
echo 'trainSet <- "'$trainSet'"' >> $R_file
echo 'cli::cat_line("NanoString classifier 3: predicting full NanoString data using TCGA model")' >> $R_file
echo 'source("pipeline/4-nanostring_classifier/3-tcga_model.R")' >> $R_file

# Content of sh file
sh_file=$shSubDir/tcga_model.sh
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

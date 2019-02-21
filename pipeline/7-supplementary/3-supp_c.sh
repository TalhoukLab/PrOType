#!/bin/bash

. ./Parameters.sh

# Make directories for R script, shell script
subDir=supplementary
RSubDir=$RDir/$subDir
shSubDir=$shDir/$subDir
mkdir -p $RSubDir
mkdir -p $shSubDir
mkdir -p $outputDir/$subDir/figures/C04
mkdir -p $outputDir/$subDir/figures/C05
mkdir -p $outputDir/$subDir/tables

# Content of R file
R_file=$RSubDir/supp_c.R
echo 'outputDir <- "'$outputDir'"' > $R_file
echo 'cli::cat_line("Supplementary C: Minimal Model Development")' >> $R_file
echo 'source("pipeline/7-supplementary/3-supp_c.R")' >> $R_file

# Content of sh file
sh_file=$shSubDir/supp_c.sh
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

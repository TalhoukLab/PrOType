#!/bin/bash

. ./Parameters.sh

# Make directories for R script, shell script
subDir=post_processing
RSubDir=$RDir/$subDir
shSubDir=$shDir/$subDir
mkdir -p $RSubDir
mkdir -p $shSubDir

# Content of R file
R_file=$RSubDir/check_baseline.R
echo 'datasets <- c("'$trainSet'", "'$trainSet2'")' > $R_file
echo 'outputDir <- "'$outputDir'"' >> $R_file
echo 'dataDir <- "data"' >> $R_file
echo 'source("pipeline/3-post_processing/0-compare_reference.R")' >> $R_file

# Content of sh file
sh_file=$shSubDir/check_baseline.sh
echo "Rscript $R_file" > $sh_file
chmod +x $sh_file

# Add to queue if qsub exists
if command -v qsub &>/dev/null; then
    file_to_submit=($sh_file)
    echo -e "$GREEN_TICK Added to queue: $sh_file"
else
    bash $sh_file
fi

# Submit to queue if qsub exists
logDir=$logDir/$subDir
outputDir=""
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh
fi

#!/bin/bash

. ./Parameters.sh

# Make directories for R script, shell script
subDir=post_processing
R_dir=$workDir/R_file/$subDir
sh_dir=$workDir/sh_file/$subDir
mkdir -p $R_dir
mkdir -p $sh_dir

# Content of R file
R_file=$R_dir/check_baseline.R
echo 'datasets <- c("'$trainSet'", "'$trainSet2'")' > $R_file
echo 'outputDir <- "'$outputDir'"' >> $R_file
echo 'dataDir <- "assets/data"' >> $R_file
echo 'source("R/3-post_processing/0-validate_baseline_results.R")' >> $R_file

# Content of sh file
sh_file=$sh_dir/check_baseline.sh
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
logDir=$baseLogDir/$subDir
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh
fi

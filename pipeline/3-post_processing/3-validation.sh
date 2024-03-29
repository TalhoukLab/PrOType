#!/bin/bash

. ./Parameters.sh

# Make directories for R script, shell script
subDir=post_processing
RSubDir=$RDir/$subDir
shSubDir=$shDir/$subDir
mkdir -p $RSubDir
mkdir -p $shSubDir
mkdir -p $outputDir/$subDir/fits
mkdir -p $outputDir/$subDir/evals
mkdir -p $outputDir/$subDir/plots
mkdir -p $outputDir/$subDir/predictions

# Content of R file
R_file=$RSubDir/validation.R
echo 'trainSet <- "'$trainSet'"' > $R_file
echo 'testSet <- "'$testSet'"' >> $R_file
echo 'outputDir <- "'$outputDir'"' >> $R_file
echo 'dataDir <- "data"' >> $R_file
echo 'source("pipeline/3-post_processing/3-fit_top_models.R")' >> $R_file
echo 'source("pipeline/3-post_processing/4-validate_array.R")' >> $R_file
echo 'source("pipeline/3-post_processing/5-predict_C2.R")' >> $R_file
echo 'source("pipeline/3-post_processing/6-probe_mapping_C2v2.R")' >> $R_file
echo 'source("pipeline/3-post_processing/7-mapping_signatures_C2.R")' >> $R_file

# Content of sh file
sh_file=$shSubDir/validation.sh
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
outputDir="$outputDir/$subDir/fits $outputDir/$subDir/evals $outputDir/$subDir/plots $outputDir/$subDir/predictions"
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh
fi

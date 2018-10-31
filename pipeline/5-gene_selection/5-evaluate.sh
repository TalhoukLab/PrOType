#!/bin/bash

. ./Parameters.sh

file_to_submit=()

# Make directories for R script, shell script
subDir=gene_selection/post_processing
RSubDir=$RDir/$subDir
shSubDir=$shDir/$subDir
mkdir -p $RSubDir
mkdir -p $shSubDir
mkdir -p $outputDir/gene_selection/plots

# Content of R file
R_file=$RSubDir/evaluate.R
echo 'algs <- strsplit("'${geneSelectionAlgs[@]}'", " ")[[1]]' > $R_file
echo 'shouldCompute <- '$shouldCompute >> $R_file
echo 'outputDir <- "'$outputDir'"' >> $R_file
echo 'source("pipeline/5-gene_selection/5-evaluate.R")' >> $R_file

# Content of sh file
sh_file=$shSubDir/evaluate.sh
echo 'Rscript' $R_file > $sh_file
chmod +x $sh_file

# Add to queue if qsub exists, submit by bash otherwise
if command -v qsub &>/dev/null; then
  file_to_submit+=($sh_file)
  echo -e "$GREEN_TICK Added to queue: $sh_file"
else
  bash $sh_file
fi

# Submit to queue
logDir=$logDir/$subDir
outputDir="$outputDir/gene_selection/plots $outputDir/gene_selection/sum_freq"
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh
fi

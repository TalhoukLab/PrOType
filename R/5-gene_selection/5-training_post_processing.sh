#!/bin/bash

. ./Parameters.sh

file_to_submit=()

echo "Processing Evaluate Predictions"

# Make directories for R script, shell script
subDir=gene_selection/post_processing
R_dir=$workDir/R_file/$subDir
sh_dir=$workDir/sh_file/$subDir
mkdir -p $R_dir
mkdir -p $sh_dir
mkdir -p $outputDir/plots

# Content of R file
R_file=$R_dir/eval_preds.R
echo 'algs <- strsplit("'${geneSelectionAlgs[@]}'", " ")[[1]]' > $R_file
echo 'shouldCompute <- '$shouldCompute >> $R_file
echo 'outputDir <- "'$outputDir'"' >> $R_file
echo 'source("R/5-gene_selection/5-evaluate_predictions.R")' >> $R_file

# Content of sh file
sh_file=$sh_dir/eval_preds.sh
echo 'Rscript' $R_file > $sh_file
chmod +x $sh_file

# Add to queue if qsub exists, submit by bash otherwise
if command -v qsub &>/dev/null; then
  file_to_submit+=($sh_file)
  echo -e "$GREEN_TICK Added to queue: $sh_file"
else
  bash $sh_file
fi

echo "Processing Summary Frequencies"

# Content of R file
R_file=$R_dir/summary_freq.R
echo 'algs <- strsplit("'${geneSelectionAlgs[@]}'", " ")[[1]]' > $R_file
echo 'shouldCompute <- '$shouldCompute >> $R_file
echo 'outputDir <- "'$outputDir'"' >> $R_file
echo 'source("R/5-gene_selection/5-summary_frequencies.R")' >> $R_file

# Content of sh file
sh_file=$sh_dir/summary_freq.sh
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
logDir=$baseLogDir/$subDir
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh
fi

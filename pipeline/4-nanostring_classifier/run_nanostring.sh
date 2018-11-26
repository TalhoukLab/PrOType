#!/bin/bash

. ./Parameters.sh

# Make directories for R script, shell script
subDir=nanostring
RSubDir=$RDir/$subDir
mkdir -p $RSubDir

mkdir -p $outputDir/$subDir/evals
mkdir -p $outputDir/$subDir/predictions

R_file=$RSubDir/nanostring.R

echo 'outputDir <- "'$outputDir'"' > $R_file
echo 'trainSet <- "'$trainSet'"' >> $R_file

echo 'cli::cat_line("NanoString classifier 1: validating overlap NanoString")' >> $R_file
echo 'source("pipeline/4-nanostring_classifier/1-validate_nanostring.R")' >> $R_file

echo 'cli::cat_line("NanoString classifier 2: predicting full NanoString data using all-array model")' >> $R_file
echo 'source("pipeline/4-nanostring_classifier/2-all_array_model.R")' >> $R_file

echo 'cli::cat_line("NanoString classifier 3: predicting full NanoString data using TCGA model")' >> $R_file
echo 'source("pipeline/4-nanostring_classifier/3-tcga_model.R")' >> $R_file

Rscript $R_file

#!/bin/bash

. ./Parameters.sh

mkdir -p $workDir/R_file/nanostring

mkdir -p $outputDir/nanostring/evals
mkdir -p $outputDir/nanostring/predictions

Rname=$workDir/R_file/nanostring/nanostring.R

echo 'outputDir <- "'$outputDir'"' > $Rname
echo 'trainSet <- "'$trainSet'"' >> $Rname

echo 'cli::cat_line("NanoString classifier 1: validating overlap NanoString")' >> $Rname
echo 'source("R/4-nanostring_classifier/1-validate_nanostring.R")' >> $Rname

echo 'cli::cat_line("NanoString classifier 2: predicting all NanoString batches")' >> $Rname
echo 'source("R/4-nanostring_classifier/2-predict_all_nanostring.R")' >> $Rname

Rscript $Rname

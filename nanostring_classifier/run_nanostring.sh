#!/bin/bash

. ./Parameters.sh

mkdir -p $workDir$dataSet/R_file/nanostring
Rname=$workDir$dataSet/R_file/nanostring/nanostring.R

echo 'outputDir <- "'$outputDir'"' > $Rname
echo 'trainSet <- "'$trainSet'"' >> $Rname

echo 'cli::cat_line("NanoString classifier 1: validating overlap NanoString")' >> $Rname
echo 'source("nanostring_classifier/1-validate_nanostring.R")' >> $Rname

echo 'cli::cat_line("NanoString classifier 2: predicting all NanoString batches")' >> $Rname
echo 'source("nanostring_classifier/2-predict_all_nanostring")' >> $Rname

Rscript $Rname

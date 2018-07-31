#!/bin/bash

. ./Parameters.sh
for dataset in "${dataSets[@]}"; do

    mkdir -p $workDir$dataset/R_file/nanostring

    Rname=$workDir$dataset/R_file/nanostring/nanostring.R

    echo 'output_dir <- "'$outputDir'"' > $Rname
    echo 'dataset <- "'$dataset'"' >> $Rname
    echo 'cat("Starting Part 1\n")' >> $Rname
    echo 'source("nanostring_classifier/step3_ValidateNanoString.R")' >> $Rname
    echo 'cat("Starting Part 2\n")' >> $Rname
    echo 'source("nanostring_classifier/step4_PredictNanoString.R")' >> $Rname

    Rscript $Rname
done